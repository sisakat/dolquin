#!/bin/bash

SRC='src'

gcc -fPIC -c $SRC/stb_image_write.c -o $SRC/stb_image_write.o
gcc -fPIC -c $SRC/stb_image.c -o $SRC/stb_image.o
fpc $SRC/Main.pas -o$PWD/Dolquin

