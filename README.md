## all-cabal-tool

This tool is designed to run continuously and check for possible changes to a
[01-index.tar.gz](https://s3.amazonaws.com/hackage.fpcomplete.com/01-index.tar.gz) file,
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

Besides updating the repositories, this tool also uploads an updated version of
[00-index.tar.gz](https://s3.amazonaws.com/hackage.fpcomplete.com/00-index.tar.gz) to
FPComplete's Hackage mirror, which is still used by the `cabal install`. Since
making the copy of the file is not part of the default mirroring process, which
is normally taking care by
[hackage-mirror-tool](https://hub.docker.com/r/snoyberg/hackage-mirror-tool/),
it has to be uploaded separatly. On the bright side, prior to the upload of
`00-index.tar.gz` to the mirror, hash values generated by Hackage for every
package are validated, therefore it can be considered unaltered and totally
secure.
