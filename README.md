# Filtered Functions
Filtered functions provide an extension of CLOS generic function invocation that add a simple preprocessing step before the actual method dispatch is performed and thus enable the use of arbitrary predicates for selecting and applying methods. See [Filtered Dispatch](http://www.p-cos.net/documents/filtered-dispatch.pdf "Filtered Dispatch") for a paper that introduces and explains filtered functions in detail.

Filtered functions are also provided by [Quicklisp](https://www.quicklisp.org/).

ContextL depends on [Closer to MOP](https://github.com/pcostanza/closer-mop "Closer to MOP").

New in version 0.2.0:
* New version number based on semantic versioning.
* Added support for Allegro Common Lisp 8.2 & 9.0.
