{- |
  This module provides some utilities that make for better HTTP-based
  programs.
-}
module Canteven.HTTP (
  FromEntity(..),
  ToEntity(..),
  DecodeResult(..),
  ContentType
) where

import Data.ByteString.Lazy (ByteString)


{- |
  The class of things that can be read as http message entities.
-}
class FromEntity e where
  {- |
    Decode the entity, according to the specified content type.
  -}
  decodeEntity :: Maybe ContentType -> ByteString -> DecodeResult e


{- |
  The class of things that can be used to generate http message entities.
-}
class ToEntity e where
  {- |
    The content type of the respone entity.
  -}
  getContentType :: e -> ContentType

  {- |
    The bytes associated with the response entity.
  -}
  getBytes :: e -> ByteString


{- |
  The result of trying to decode a request entity.
-}
data DecodeResult e
  = Unsupported
    -- ^ Signifies an unsupported content type.
  | BadEntity String
    -- ^ Signifies that the request entity is invalid, and provides some
    --   kind of reason why.
  | Ok e
    -- ^ Successfully decoded the entity.


{- |
  ContentType is an alias for ByteString
-}
type ContentType = ByteString


