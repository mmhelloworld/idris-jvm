# Windows Installation
Windows users, please follow the instructions below to install idris-jvm.
These steps are taken from Idris installation steps
from [here](https://github.com/idris-lang/Idris-dev/wiki/Idris-on-Windows) and modified
to install idris-jvm.

1. Download and install [MSYS2](http://msys2.github.io/). Choose the 64-bit version.
1. Open an MSYS2 shell by starting `mingw64.exe`, located in the directory you installed MSYS2 in, by default C:\msys64.
1. Update MSYS2 packages by running `pacman -Syu`
1. Install the needed tools with `pacman -S make binutils msys2-w32api-runtime mingw-w64-x86_64-gcc mingw-w64-x86_64-pkg-config mingw-w64-x86_64-libffi git`.
1. Restart msys2.
1. Now run the `bin\setup` script.

## Trouble shooting
1. If you get an error like this:

```
make: Entering directory '/tmp/stack7236/idris-0.12.2/rts'

rm -f idris_rts.o idris_heap.o idris_gc.o idris_gmp.o idris_bitstring.o idris_opts.o idris_stats.o idris_utf8.o idris_stdfgn.o mini-gmp.o getline.o windows/idris_net.o windows/win_utils.o libidris_rts.a

/usr/bin/rm: missing operand
```
This is due to multiple msys2 installations (one that is also installed by stack) as explained [here](https://github.com/commercialhaskell/stack/issues/1482#issuecomment-176941036). If you get this issue, just remove the msys2 installation under stack and try running the `bin\setup` script again:

`rm -rf  /c/Users/USERNAME/AppData/Local/Programs/stack/x86_64-windows/msys2-20150512`
