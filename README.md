mclide
======

Macintosh IDE for Lisp-related languages.

== Usage ==

Preliminary instructions to get started:

  1. Open a Terminal and change directory to here.

  2. Download MCLIDE 2:

     $ curl -C - -O https://mclide.googlecode.com/files/MCLIDE-20a0-b95dbee-.dmg

  3. Open the application package by control-clicking on the icon

  4. Locate the following directories and copy them here:

  * Contents/Resources/lib/

  * Contents/resources/slime-2012-02-02/

  5. Rename 'slime-2012-02-02' to 'slime'

  6. Download Clozure CL 1.7 and place the ccl directory here:

     $ curl -O ftp://ftp.clozure.com/pub/release/1.7/ccl-1.7-darwinx86.tar.gz

  7. Execute in shell to build MCLIDE:

     $ sh ./build.sh


