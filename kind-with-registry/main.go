package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/google/go-jsonnet"
	"github.com/spf13/cobra"
)

type createClusterParams struct {
	TmpDir                string
	ClusterName           string
	RegistryHostPort      int
	RegistryContainerPort int
	K8sImage              string
	NrWorkerNodes         int
	SupportKubePrometheus bool
}

const (
	// _defaultRegistryHostPort is the host port that the local Docker Registry
	// listens to
	_defaultRegistryHostPort = 6001
	// _registryContainerName is the container name of the local Docker Registry
	_registryContainerName = "kind-registry"
	// _registryContainerPort is the container port of the local Docker Registry
	_registryContainerPort = 5000
	// _defaultK8sVersion is the default Kubernetes version that will be used
	// to create the kind cluster
	_defaultK8sVersion = "1.24"
	// _defaultNrWorkerNodes is the default number of worker nodes in the kind
	// cluster
	_defaultNrWorkerNodes = 3

	// _kindConfigTemplate is the configuration template for kind clusters
	_kindConfigTemplate = `
function(k8sImage, nrWorkerNodes, hostPort, containerPort, controlPlanePatches={}) {
  apiVersion: 'kind.x-k8s.io/v1alpha4',
  kind: 'Cluster',
  nodes: [
    { role: 'control-plane', image: k8sImage } + controlPlanePatches,
  ] + [
    { role: 'worker', image: k8sImage }
    for x in std.range(1, nrWorkerNodes)
  ],
  containerdConfigPatches: [
    |||
    [plugins."io.containerd.grpc.v1.cri".registry.mirrors."localhost:%(hostPort)d"]
    endpoint = ["http://kind-registry:%(containerPort)d"]
||| % { hostPort: hostPort, containerPort: containerPort },
  ],
}
`

	// _controlPlanePatchKubeletForKubePrometheus is a patch on kind's control
	// plane configuration for adding a few kubelet flags to support
	// kube-prometheus
	// Also see:
	// https://github.com/prometheus-operator/kube-prometheus
	// https://kind.sigs.k8s.io/docs/user/configuration/#kubeadm-config-patches
	// https://kubernetes.io/docs/reference/config-api/kubeadm-config.v1beta3/#kubeadm-k8s-io-v1beta3-NodeRegistrationOptions
	_controlPlanePatchKubeletForKubePrometheus = `
{
  kubeadmConfigPatches: [
    |||
    kind: InitConfiguration
    nodeRegistration:
      kubeletExtraArgs:
        authentication-token-webhook: "true"
        authorization-mode: Webhook
|||
  ],
}
`

	// _configMapTemplate is template for creating a ConfigMap required by the
	// local Docker Registry
	_configMapTemplate = `
function(hostPort) {
  apiVersion: 'v1',
  kind: 'ConfigMap',
  metadata: {
    name: 'local-registry-hosting',
    namespace: 'kube-public',
  },
  data: {
    'localRegistryHosting.v1': |||
      host: "localhost:%(hostPort)d"
      help: "https://kind.sigs.k8s.io/docs/user/local-registry/"
    ||| % { hostPort: hostPort },
  },
}
`
)

var (
	// _clusterName stores the desired cluster name that is supplied by the
	// user on the command line
	_clusterName string
	// _registryHostPort stores the desired docker registry host port that is
	// supplied by the user on the command line
	_registryHostPort int
	// _k8sVersion stores the desired k8s version that is supplied by the user
	// on the command line
	_k8sVersion string
	// _nrWorkerNodes is the number of desired k8s worker nodes that is
	// supplied by the user on the command line
	_nrWorkerNodes int
	// _supportKubePrometheus is true if we want to run kube-prometheus on
	// the cluster
	_supportKubePrometheus bool

	// _k8sImages contains k8s version -> Docker image mapping for kind
	// The images are for kind 0.11.1
	// TODO: Support different kind versions; Shift these to a more permanent file
	_k8sImages = map[string]string{
		"1.16": "kindest/node:v1.16.15@sha256:83067ed51bf2a3395b24687094e283a7c7c865ccc12a8b1d7aa673ba0c5e8861",
		"1.21": "kindest/node:v1.21.1@sha256:69860bda5563ac81e3c0057d654b5253219618a22ec3a346306239bba8cfa1a6",
		"1.22": "kindest/node:v1.22.0@sha256:b8bda84bb3a190e6e028b1760d277454a72267a5454b57db34437c34a588d047",
		"1.23": "kindest/node:v1.23.0@sha256:49824ab1727c04e56a21a5d8372a402fcd32ea51ac96a2706a12af38934f81ac",
		"1.24": "kindest/node:v1.24.7@sha256:577c630ce8e509131eab1aea12c022190978dd2f745aac5eb1fe65c0807eb315",
	}
)

func validateArgs(_ *cobra.Command, _ []string) error {
	var errs []string
	if _registryHostPort <= 0 || _registryHostPort > 65535 {
		errs = append(errs, fmt.Sprintf("Port must be from 1 to 65535 (got %d)", _registryHostPort))
	}
	if len(_clusterName) <= 0 {
		errs = append(errs, "cluster name must be non-empty string")
	}
	if _, ok := _k8sImages[_k8sVersion]; !ok {
		var supportedVersions []string
		for k := range _k8sImages {
			supportedVersions = append(supportedVersions, k)
		}
		errs = append(errs, fmt.Sprintf("Unsupported k8s version given. Supported versions: %s", strings.Join(supportedVersions, ",")))
	}
	if _nrWorkerNodes <= 0 {
		errs = append(errs, fmt.Sprintf("1 or more worker nodes must be supplied (%d given)", _nrWorkerNodes))
	}
	if len(errs) > 0 {
		return fmt.Errorf(strings.Join(errs, "\n"))
	}
	return nil
}

func rootRunE(cmd *cobra.Command, args []string) error {
	vm := jsonnet.MakeVM()
	if err := createRegistryContainer(_registryHostPort, _registryContainerName, _registryContainerPort); err != nil {
		return err
	}
	tmpDir, err := ioutil.TempDir("", "kwr*")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmpDir)
	fmt.Printf("tmpDir = %q\n", tmpDir)
	params := &createClusterParams{
		TmpDir:                tmpDir,
		ClusterName:           _clusterName,
		RegistryHostPort:      _registryHostPort,
		RegistryContainerPort: _registryContainerPort,
		K8sImage:              _k8sImages[_k8sVersion],
		NrWorkerNodes:         _nrWorkerNodes,
		SupportKubePrometheus: _supportKubePrometheus,
	}
	if err := createKindCluster(vm, params); err != nil {
		return err
	}
	if err := connectRegistryToClusterNetwork(_registryContainerName); err != nil {
		return err
	}
	return addConfigMapForLocalRegistry(vm, tmpDir, _registryHostPort)
}

func createRegistryContainer(hostPort int, containerName string, containerPort int) error {
	buf := &bytes.Buffer{}
	cmd := exec.Command("docker", "inspect", "-f", "'{{ .State.Running }}'", containerName)
	cmd.Stdout = buf
	if err := cmd.Run(); err != nil {
		if _, ok := err.(*exec.ExitError); ok {
			// Create container
			fmt.Printf("Creating Docker Registry container which will listen at 127.0.0.1:%d\n", hostPort)
			cmdCreate := exec.Command("docker", "run", "-d", "--restart", "always", "-p", fmt.Sprintf("127.0.0.1:%d:%d", hostPort, containerPort), "--name", containerName, "registry:2")
			return cmdCreate.Run()
		}
		return err
	}
	s := cleanDockerInspectOutput(buf.String())
	if s == "true" {
		fmt.Println("kind-registry was created previously")
		return nil
	}
	return fmt.Errorf("When looking for existing Docker Registry container, the `docker inspect` command output %q", s)
}

func createKindCluster(vm *jsonnet.VM, params *createClusterParams) error {
	buf := &bytes.Buffer{}
	cmd := exec.Command("kind", "get", "clusters")
	cmd.Stdout = buf
	if err := cmd.Run(); err != nil {
		return err
	}
	// No cluster = empty string
	// Have clusters = cluster names separated by newlines
	clusterName := params.ClusterName
	existingClusters := strings.Split(buf.String(), "\n")
	for _, s := range existingClusters {
		if clusterName == s {
			return fmt.Errorf("kind cluster %q already exists", clusterName)
		}
	}
	// Create cluster
	vm.TLAReset()
	vm.TLACode("k8sImage", fmt.Sprintf("%q", params.K8sImage))
	vm.TLACode("hostPort", strconv.Itoa(params.RegistryHostPort))
	vm.TLACode("containerPort", strconv.Itoa(params.RegistryContainerPort))
	vm.TLACode("nrWorkerNodes", strconv.Itoa(params.NrWorkerNodes))
	controlPlanePatch, err := generateControlPlanePatch(params)
	if err != nil {
		return err
	}
	vm.TLACode("controlPlanePatches", controlPlanePatch)
	kindTemplate, err := vm.EvaluateAnonymousSnippet("kind-config.jsonnet", _kindConfigTemplate)
	if err != nil {
		return err
	}
	filename := filepath.Join(params.TmpDir, fmt.Sprintf("%s-config.yml", clusterName))
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	if _, err := f.WriteString(kindTemplate); err != nil {
		return err
	}
	if err := f.Close(); err != nil {
		return err
	}
	fmt.Printf("kindTemplate = %s", kindTemplate)
	fmt.Println("=====")
	cmd = exec.Command("kind", "create", "cluster", "--name", clusterName, "--config", filename)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func generateControlPlanePatch(params *createClusterParams) (string, error) {
	if params.SupportKubePrometheus {
		vm := jsonnet.MakeVM()
		return vm.EvaluateAnonymousSnippet("control-plane-patch.jsonnet", _controlPlanePatchKubeletForKubePrometheus)
	}
	return "{}", nil
}

func connectRegistryToClusterNetwork(registryContainerName string) error {
	buf := &bytes.Buffer{}
	cmd := exec.Command("docker", "inspect", "-f", "'{{ json .NetworkSettings.Networks.kind }}'", registryContainerName)
	cmd.Stdout = buf
	if err := cmd.Run(); err != nil {
		return err
	}
	s := cleanDockerInspectOutput(buf.String())
	if s == "null" {
		cmdLink := exec.Command("docker", "network", "connect", "kind", registryContainerName)
		return cmdLink.Run()
	}
	return nil
}

func addConfigMapForLocalRegistry(vm *jsonnet.VM, tmpDir string, port int) error {
	vm.TLAReset()
	vm.TLACode("hostPort", strconv.Itoa(port))
	cm, err := vm.EvaluateAnonymousSnippet("cm.jsonnet", _configMapTemplate)
	if err != nil {
		return err
	}
	filename := filepath.Join(tmpDir, "cm.yaml")
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	if _, err := f.WriteString(cm); err != nil {
		return err
	}
	if err := f.Close(); err != nil {
		return err
	}
	fmt.Printf("=====\ncm:\n%s\n=====", cm)
	cmd := exec.Command("kubectl", "apply", "-f", filename)
	return cmd.Run()
}

func cleanDockerInspectOutput(s string) string {
	return strings.TrimSuffix(strings.TrimPrefix(strings.TrimSpace(s), "'"), "'")
}

func main() {
	rootCmd := &cobra.Command{
		Use:     "kind-with-registry",
		Short:   "Creates k8s clusters using kind",
		Long:    "Creates k8s clusters using kind (TODO: edit this)",
		PreRunE: validateArgs,
		RunE:    rootRunE,
	}
	rootCmd.Flags().StringVarP(&_clusterName, "name", "n", "", "kind cluster name")
	rootCmd.MarkFlagRequired("name")
	rootCmd.Flags().IntVarP(&_registryHostPort, "port", "p", _defaultRegistryHostPort, "host port of the local Docker Registry")
	rootCmd.Flags().StringVarP(&_k8sVersion, "k8s-version", "k", _defaultK8sVersion, "Kubernetes version in MAJOR.MINOR format")
	rootCmd.Flags().IntVarP(&_nrWorkerNodes, "workers", "w", _defaultNrWorkerNodes, "Number of worker nodes in the cluster")
	rootCmd.Flags().BoolVarP(&_supportKubePrometheus, "kube-prom", "", false, "If true, patches the kind config template's 'kubeadmConfigPatches' section by adding 2 kubelet arguments. This is done to support running kube-prometheus on the cluster")
	fmt.Println("vim-go")
	rootCmd.SetArgs(os.Args)
	if err := rootCmd.Execute(); err != nil {
		fmt.Printf("Error: %+v\n", err)
		os.Exit(1)
	}
}
