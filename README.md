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
