# Small throw away scripts

This thing is on the clever side.

A file at `interpreterPackage/foo` will translate into:

``` sh
#!<path to interpreterPackage>/bin/<interpreterPackage>

<contents of foo>
```

If the file `interpreterPackage/binaryName` exists, instead it is:

``` sh
#!<path to interpreterPackage>/bin/<contents of file binaryName>

<contents of foo>
```
