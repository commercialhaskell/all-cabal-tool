module Stackage.Package.Git (
  module X
  ) where

import Stackage.Package.Git.Repository as X
import Stackage.Package.Git.Types as X
       hiding (getPerson, getTreeRef, getTreeMode, toShortRef,
               fromShortRef)
import Stackage.Package.Git.Object as X
