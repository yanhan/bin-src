module BinSrc.TermSize (
  get_terminal_cols
) where

#include <sys/ioctl.h>
#include <unistd.h>

import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.Types (CInt(CInt))
import Foreign.Ptr (Ptr)
import Foreign.Storable
  ( Storable, sizeOf, alignment, peek, peekByteOff, poke, pokeByteOff
  )
import Foreign.Marshal.Alloc (alloca)

data TermSize = TermSize { ws_row :: Int, ws_col :: Int }

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable TermSize where
  alignment _ = #{alignment struct winsize}
  sizeOf    _ = #{size struct winsize}
  peek ptr = do
    rows <- #{peek struct winsize, ws_row} ptr
    cols <- #{peek struct winsize, ws_col} ptr
    return $ TermSize { ws_row = rows, ws_col = cols }
  poke ptr (TermSize { ws_row = rows, ws_col = cols }) = do
    #{poke struct winsize, ws_row} ptr rows
    #{poke struct winsize, ws_col} ptr cols

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr TermSize -> IO CInt

get_terminal_cols :: IO Int
get_terminal_cols =
  alloca (\ptr -> do
    poke ptr (TermSize { ws_row = 0, ws_col = 0 })
    _ <- throwErrnoIfMinus1 "ioctl error" $
           ioctl #{const STDOUT_FILENO} #{const TIOCGWINSZ} ptr
    retTermSize <- peek ptr
    return $ ws_col retTermSize
  )
