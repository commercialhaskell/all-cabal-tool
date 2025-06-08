## all-cabal-tool

This tool is designed to run continuously and check for possible changes to a
[01-index.tar.gz](https://hackage.haskell.org/01-index.tar.gz) file,
which usually happens either due to an upload of a new package/package version
to [Hackage](http://hackage.haskell.org/), or due to change in a `.cabal` or
`preferred-versions` files. Any change to the above tarball triggers an update of each
one of the three repositories:
* [all-cabal-files](https://github.com/commercialhaskell/all-cabal-files)
* [all-cabal-hashes](https://github.com/commercialhaskell/all-cabal-hashes)
* [all-cabal-metadata](https://github.com/commercialhaskell/all-cabal-metadata)

Reasoning for the actual existence of the above repositories and the need for their
update you can find in the blog post on the FPComplete website:
[Updated Hackage mirroring](https://www.fpcomplete.com/blog/2016/09/updated-hackage-mirroring)

Besides updating the repositories, this tool also uploads an updated version
of
[00-index.tar.gz](https://hackage.haskell.org/00-index.tar.gz) to
Stackage's Hackage mirror. Normally, mirroring is taken care by
a [hackage-mirror-tool](https://github.com/commercialhaskell/hackage-mirror-tool),
but since making the copy of the above file is not part of the default mirroring
process, it has to be uploaded separatly. On the bright side, prior to the
upload of `00-index.tar.gz`, which is still the type of file used by `cabal`,
Hackage hash values generated for every package are validated by
`hackage-mirror-tool` and `all-cabal-tool`, therefore it can be considered
unaltered and totally secure.


## Developing the package

The package can be built with stack and nix (and with cabal, from within the nix
shell).

* `stack build`
* `nix build`
* `nix develop` --> `cabal build`

### Updating dependencies

Kind of a pain.

We want all build systems to use the same dependencies.

The main set of dependencies comes from the Stackage snapshot specified in
stack.yaml.

We rely on some unreleased versions of packages, however. This are easily
handled in stack.yaml. To get the nix build, we use cabal2nix to define the
sources.

To control cabal2nix, modify `nix/scripts/gen-packages.sh` and run it with `nix
run .#gen-packages`.

Why would you want to modify gen-packages.sh? When bumping dependencies.

To do that, start with changing the stack snapshot. But you have to synchronize
the change with the nixpkgs version, as well. This part is done manually. You
have to find a nixpkgs revision that uses the same snapshot. It also has to
*work* with the given snapshot! The last time, our strategy was to choose the
last commit that used a given LTS.

Search for changes to snapshots by grepping the nixpkgs git log.

#### Example

We were updating to LTS 22.40. After finding the commit that moves *beyond*
22.40, we picked the previous commit to be our nixpkgs revision.

```
$ git log --oneline | grep stackage | head -n 1
ca5dc07d3759 haskellPackages: stackage LTS 22.40 -> LTS 22.43
$ git show ca5dc07d3759^
commit 69e7105d5d8bff9e0cb1718d4a76a54aa9210f98
...

--> Use 69e7105d5d8bff9e0cb1718d4a76a54aa9210f98 as the nixpkgs pin!
```
