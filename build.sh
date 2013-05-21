#!/bin/sh
#
# Run to build MCLIDE

CCL_DEFAULT_DIRECTORY=./ccl
./ccl/dx86cl64 --batch --no-init --load "./mclide/build.lisp"