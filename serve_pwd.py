#!/usr/bin/env python

import argparse
import re
import BaseHTTPServer
import SimpleHTTPServer 

parser = argparse.ArgumentParser(
  description=re.sub(r"""\s+""", " ", """serves files in the current directory
    using python SimpleHTTPServer"""
  )
)
parser.add_argument("-s", "--server", default="127.0.0.1", type=str,
  dest="host",
  help="Host to serve from, eg. 127.0.0.1 , 0.0.0.0",
)
parser.add_argument("-p", "--port", default=8000, type=int,
  dest="port", help="Port of serve from, eg. 8000"
)

def main():
  args = parser.parse_args()
  server = BaseHTTPServer.HTTPServer((args.host, args.port),
    SimpleHTTPServer.SimpleHTTPRequestHandler
  )
  print("Serving HTTP on {host} port {port}".format(host=args.host,
    port=args.port
  ))
  server.serve_forever()

if __name__ == "__main__":
  main()
