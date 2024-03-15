#! /bin/bash

# Convert any sRGB files of any format to RGB PNG.
# Useful for matcaps which are often in EXR format.

mogrify -gamma 2.2 -type truecolor -format png "$@"
