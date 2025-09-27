# Small throw away scripts

This thing is perhaps a bit too clever for its own good, but I can't think of an easier way to dump in new scripts.

A file at `interpreterPackage/foo` will translate into:

```
#!<path to interpreterPackage in nix store>/bin/interpreterPackage

<contents of foo>
```

If the file `interpreterPackage/binaryName` exists, instead it is:

```
#!<path to interpreterPackage>/bin/<contents of file binaryName>

<contents of foo>
```

I.e. for the `python311` folder (which contains a `binaryName` file, the text for which is `python3`) and the `http-debug-server` file, that's:

``` python
#!/nix/store/<hash>-python311/bin/python3

<contents of python311/http-debug-server>
```

