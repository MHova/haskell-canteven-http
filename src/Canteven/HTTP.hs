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


import Canteven.Log.MonadLog (LoggerTImpl)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Control.Monad (void, join)
import Control.Monad.Catch (try, throwM)
import Control.Monad.Logger (runLoggingT, LoggingT, logInfo, logError)
import Control.Monad.Trans.Class (lift)
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.Version (showVersion, Version)
import Language.Haskell.TH (TExp, Q, runIO)
import Network.HTTP.Types (internalServerError500, Status, statusCode,
  statusMessage, ok200)
import Network.Mime (defaultMimeLookup)
import Network.Wai (Middleware, responseStatus, requestMethod,
  rawPathInfo, rawQueryString, Response, ResponseReceived, responseLBS,
  modifyResponse, pathInfo)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.StripHeaders (stripHeader)
import System.Directory (getDirectoryContents)
import System.FilePath.Posix (combine, (</>))
import System.Posix.Files (isRegularFile, isDirectory, getFileStatus)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
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


{- |
  The Template-Haskell splice @$$(staticSite dir)@ will build a
  'Middleware' that serves a set of static files determined at
  compile time, or else passes the request to the underlying
  'Network.Wai.Application'.

  All files under @dir@ will be served relative to the root path of
  your web server, so the file @\<dir\>\/foo\/bar.html@ will be served at
  @http://your-web-site.com/foo/bar.html@

  @since 0.1.3
-}
staticSite :: FilePath -> Q (TExp Middleware)
staticSite baseDir = join . runIO $ do
    files <- readStaticFiles
    mapM_ (printResource . fst) files
    return $ [||
        let
          {- |
            Build a middleware that serves a single static file path, or
            delegates to the underlying application.
          -}
          static :: (FilePath, String) -> Middleware
          static (filename, content) app req respond =
            let
              {- | Guess the content type of the static file. -}
              contentType :: BS.ByteString
              contentType =
                defaultMimeLookup
                . pack
                $ filename
            in
              if pathInfo req == T.split (== '/') (pack filename)
                then
                  respond (
                      responseLBS
                        ok200
                        [("content-type", contentType)]
                        (BSL8.pack content)
                    )
                else app req respond
        in
          foldr (.) id (fmap static files) :: Middleware
      ||]
  where
    printResource :: String -> IO ()
    printResource file =
      putStrLn ("Generating static resource for: " ++ show file)

    {- | Reads the static files that make up the admin user interface. -}
    readStaticFiles :: IO [(FilePath, String)]
    readStaticFiles =
      let
        findAll :: FilePath -> IO [FilePath]
        findAll dir = do
            contents <-
              (\\ [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
            dirs <- catMaybes <$> mapM justDir contents
            files <- catMaybes <$> mapM justFile contents
            more <- concat <$> mapM (findAll . combine dir) dirs
            return $ (combine dir <$> files) ++ more
          where
            justFile :: FilePath -> IO (Maybe FilePath)
            justFile filename = do
              isfile <-
                isRegularFile <$>
                  getFileStatus (baseDir </> dir </> filename)
              return $ if isfile then Just filename else Nothing

            justDir :: FilePath -> IO (Maybe FilePath)
            justDir filename = do
              isdir <-
                isDirectory <$>
                  getFileStatus (baseDir </> dir </> filename)
              return $ if isdir then Just filename else Nothing
      in do
        allFiles <- findAll "."
        allContent <- mapM (fmap BS8.unpack . BS.readFile . combine baseDir) allFiles
        return (zip (drop 2 <$> allFiles) allContent)

