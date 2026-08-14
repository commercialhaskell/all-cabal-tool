{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | An @http-client@ backed 'HttpLib', the HTTP abstraction that
-- @hackage-security@ layers its repository access on top of.
--
-- This should really be packaged up as a library and published on Hackage to
-- complement @hackage-security-HTTP@.
--
-- We need this because the HF mirror only speaks TLS, which the HTTP package
-- doesn't support.
module Stackage.Package.HttpLib
  ( withHttpLib
  , UnexpectedResponse(..)
  , unexpectedResponseStatus
  ) where

import Control.Exception (Exception(..), IOException)
import qualified Data.ByteString.Char8 as S8
import Data.List (intercalate)
import Data.Typeable (cast)
import Network.HTTP.Client
       (Manager, Request(..), Response(..), requestFromURI)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (statusCode)
import Network.URI (URI)

import Hackage.Security.Client (SomeRemoteError(..))
import Hackage.Security.Client.Repository.HttpLib
import Hackage.Security.Util.Checked
import Hackage.Security.Util.Pretty (Pretty(..))

-- | Run an action with an 'HttpLib' backed by the same connection pool that
-- @Network.HTTP.Simple@ uses elsewhere in this program.
withHttpLib :: (HttpLib -> IO a) -> IO a
withHttpLib callback = do
  manager <- getGlobalManager
  callback
    HttpLib
    { httpGet = get manager
    , httpGetRange = getRange manager
    }

get
  :: Throws SomeRemoteError
  => Manager
  -> [HttpRequestHeader]
  -> URI
  -> ([HttpResponseHeader] -> BodyReader -> IO a)
  -> IO a
get manager reqHeaders uri callback =
  wrapCustomEx $ do
    req <- addRequestHeaders reqHeaders <$> requestFromURI uri
    HTTP.withResponse req manager $ \res ->
      case statusCode (responseStatus res) of
        200 -> callback (responseHeaders' res) (wrapCustomEx (responseBody res))
        code -> throwChecked $ UnexpectedResponse uri code

getRange
  :: Throws SomeRemoteError
  => Manager
  -> [HttpRequestHeader]
  -> URI
  -> (Int, Int)
  -> (HttpStatus -> [HttpResponseHeader] -> BodyReader -> IO a)
  -> IO a
getRange manager reqHeaders uri (from, to) callback =
  wrapCustomEx $ do
    req <-
      setRange from to . addRequestHeaders reqHeaders <$> requestFromURI uri
    HTTP.withResponse req manager $ \res ->
      let body = wrapCustomEx (responseBody res)
      in case statusCode (responseStatus res) of
           200 -> callback HttpStatus200OK (responseHeaders' res) body
           206 ->
             callback HttpStatus206PartialContent (responseHeaders' res) body
           code -> throwChecked $ UnexpectedResponse uri code

-- | @Range@ takes an inclusive end offset, whereas 'httpGetRange' is given an
-- exclusive one.
setRange :: Int -> Int -> Request -> Request
setRange from to req =
  req
  { requestHeaders =
      ("Range", S8.pack ("bytes=" ++ show from ++ "-" ++ show (to - 1))) :
      requestHeaders req
  }

-- | Both request headers 'HttpLib' can express are @Cache-Control@ directives,
-- so they collapse into a single comma-separated header.
addRequestHeaders :: [HttpRequestHeader] -> Request -> Request
addRequestHeaders [] req = req
addRequestHeaders hs req =
  req
  { requestHeaders =
      ("Cache-Control", S8.pack (intercalate ", " (map directive hs))) :
      requestHeaders req
  }
  where
    directive HttpRequestMaxAge0 = "max-age=0"
    directive HttpRequestNoTransform = "no-transform"

responseHeaders' :: Response a -> [HttpResponseHeader]
responseHeaders' res =
  [HttpResponseAcceptRangesBytes | ("Accept-Ranges", "bytes") `elem` responseHeaders res]

wrapCustomEx
  :: ((Throws UnexpectedResponse, Throws IOException, Throws HTTP.HttpException) =>
        IO a)
  -> (Throws SomeRemoteError =>
        IO a)
wrapCustomEx act =
  handleChecked (\(ex :: UnexpectedResponse) -> go ex) $
  handleChecked (\(ex :: HTTP.HttpException) -> go ex) $
  handleChecked (\(ex :: IOException) -> go ex) act
  where
    go ex = throwChecked (SomeRemoteError ex)

-- | A mirror answered with something other than the file that was asked for.
data UnexpectedResponse = UnexpectedResponse
  { unexpectedUri :: URI
  , unexpectedStatus :: Int
  }

instance Pretty UnexpectedResponse where
  pretty (UnexpectedResponse uri code) =
    "Unexpected response " ++ show code ++ " for " ++ show uri

deriving instance Show UnexpectedResponse

instance Exception UnexpectedResponse where
  displayException = pretty

-- | The status a mirror answered with, for failures that came from the
-- response rather than from the transport underneath it.
unexpectedResponseStatus :: SomeRemoteError -> Maybe Int
unexpectedResponseStatus (SomeRemoteError inner) = unexpectedStatus <$> cast inner
