{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
  This module provides some utilities that make for better HTTP-based
  programs.
-}
module Canteven.HTTP (
  FromEntity(..),
  ToEntity(..),
  DecodeResult(..),
  ContentType,
  requestLogging,
  logExceptionsAndContinue,
  setServer,
  staticSite,
) where

import Canteven.Internal (staticSite)
import Canteven.Log.MonadLog (LoggerTImpl)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.Catch (try, throwM)
import Control.Monad.Logger (runLoggingT, LoggingT, logInfo, logError)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.Version (showVersion, Version)
import Network.HTTP.Types (internalServerError500, Status, statusCode, statusMessage)
import Network.Wai (Middleware, responseStatus, requestMethod, rawPathInfo,
  rawQueryString, Response, ResponseReceived, requestHeaders, responseLBS, modifyResponse)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.StripHeaders (stripHeader)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T


{- | The class of things that can be read as http message entities. -}
class FromEntity e where
  {- | Decode the entity, according to the specified content type. -}
  decodeEntity :: Maybe ContentType -> BSL.ByteString -> DecodeResult e


{- | The class of things that can be used to generate http message entities. -}
class ToEntity e where
  {- | The content type of the respone entity. -}
  getContentType :: e -> ContentType

  {- | The bytes associated with the response entity. -}
  getBytes :: e -> BSL.ByteString


{- | The result of trying to decode a request entity. -}
data DecodeResult e
  = Unsupported
    {- ^ Signifies an unsupported content type.  -}
  | BadEntity String
    {- ^
      Signifies that the request entity is invalid, and provides some
      kind of reason why.
    -}
  | Ok e
    {- ^ Successfully decoded the entity. -}
  deriving (Show)


{- | ContentType is an alias for ByteString. -}
type ContentType = BSL.ByteString


{- |
  Logs all exceptions, and returns a 500 Internal Server error. 

  This is useful because your wai framework won't always do what you
  expect when it encounters random exceptions. For instance, an exception
  thrown in IO may cause functionality of higher-level middlewares to be
  bypassed unless they know how to catch and re-throw exceptions (making
  them more complicated). This middleware explicitly will not re-throw
  exceptions, unless those exceptions were encountered after the headers
  have already been sent, e.g. when using 'Network.Wai.StreamingBody'.
  
  What it will do is generate a unique id for the exception and print
  that ID, so you can easily find it in the logs.
-}
logExceptionsAndContinue :: LoggerTImpl -> Middleware
logExceptionsAndContinue logging app req respond = (`runLoggingT` logging) $
    try (lift (app req loggingRespond)) >>= \case
      Right ack -> return ack
      Left err -> do
        uuid <- logProblem err
        lift $ respond (errResponse uuid)

  where
    errResponse :: UUID -> Response
    errResponse uuid =
      responseLBS
        internalServerError500
        [("Content-Type", "text/plain")] 
        (BSL.fromStrict . encodeUtf8 . pack
          $ "Internal Server Error. Error ID: " ++ show uuid)

    getUUID :: LoggingT IO UUID
    getUUID = lift nextUUID >>= \case
      Nothing -> lift (threadDelay 1000) >> getUUID
      Just uuid -> return uuid

    loggingRespond :: Response -> IO ResponseReceived
    loggingRespond response = (`runLoggingT` logging) $
      try (lift (respond response)) >>= \case
        Right ack -> return ack
        Left err -> do
          void $ logProblem err
          throwM err

    logProblem :: SomeException -> LoggingT IO UUID
    logProblem err = do
      uuid <- getUUID
      $(logError) . pack
        $ "Internal Server Error [" ++ show uuid ++ "]: "
        ++ show (err :: SomeException)
      return uuid


{- |
  This request logger is better than the ones in
  `Network.Wai.Middleware.RequestLogger`, where "better" means:

  1) Operates in `Control.Monad.Logger.MonadLogger`.
  2) Logs the start of a request separate from its completion.
  3) Logs the duration of the request.
-}
requestLogging :: LoggerTImpl -> Middleware
requestLogging logging app req respond = (`runLoggingT` logging) $ do
    $(logInfo) . pack
      $ "Starting request: " ++ reqStr
    $(logInfo) $ maybe
      "X-Request-Id header missing"
      (T.append "X-Request-Id: " . decodeUtf8)
      requestHeaderRequestId
    lift . app req . loggingRespond =<< lift getCurrentTime
  where
    {- | Delegate to the underlying responder, and do some logging. -}
    loggingRespond :: UTCTime -> Response -> IO ResponseReceived
    loggingRespond start response = (`runLoggingT` logging) $ do
      {-
        Execute the underlying responder first so we get an accurate
        measurement of the request duration.
      -}
      ack <- lift $ respond response
      now <- lift getCurrentTime
      $(logInfo) . pack
        $ reqStr ++ " --> " ++ showStatus (responseStatus response)
        ++ " (" ++ show (diffUTCTime now start) ++ ")"
      return ack

    {- | A string representation of the request, suitable for logging. -}
    reqStr :: String
    reqStr = unpack . decodeUtf8
      $ requestMethod req <> " " <> rawPathInfo req <> rawQueryString req

    {- |
      @instance Show Status@ shows the Haskell structure, which is
      not suitable for logging.
    -}
    showStatus :: Status -> String
    showStatus stat =
      show (statusCode stat) ++ " "
      ++ (unpack . decodeUtf8 . statusMessage) stat

    requestHeaderRequestId :: Maybe ByteString
    requestHeaderRequestId = snd <$> find ((==) "X-Request-Id" . fst)
      (requestHeaders req)


{- |
  Set the @Server:@ header.

  @since 0.1.2
-}
setServer :: Text -> Version -> Middleware
setServer serviceName version = addServerHeader . stripServerHeader
  where
    {- | Strip the server header. -}
    stripServerHeader :: Middleware
    stripServerHeader = modifyResponse (stripHeader "Server")

    {- | Add our own server header. -}
    addServerHeader :: Middleware
    addServerHeader = addHeaders [("Server", serverValue)]

    {- | The value of the @Server:@ header. -}
    serverValue = encodeUtf8 (serviceName <> "/" <> pack (showVersion version))
