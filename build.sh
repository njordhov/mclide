#!/bin/sh
#
# Run to build MCLIDE

CCL_DEFAULT_DIRECTORY=./ccl
# export CCL_DEFAULT_DIRECTORY
./ccl/dx86cl64 --batch --no-init --load "./mclide/build.lisp" --eval "(ccl::quit)"