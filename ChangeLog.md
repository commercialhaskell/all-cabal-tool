## 0.1.1.0

* Implemented dependency of packages on hash validation:
  * cabal files that are missing corresponding package.json (or contain
    mismatched hashes) are skipped.
  * Versions with missing/missmatched hashes are no longer added to metadata.

## 0.1.0.0

* Initial commit
