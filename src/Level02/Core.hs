{-# LANGUAGE OverloadedStrings #-}

module Level02.Core (runApp, app) where

import qualified Data.ByteString.Lazy as LBS
import Data.Either (either)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Level02.Types
  ( ContentType (PlainText),
    Error (Error),
    RqType (AddRq, ListRq, ViewRq),
    mkCommentText,
    mkTopic,
    renderContentType,
  )
import Network.HTTP.Types
  ( Method,
    Status,
    hContentType,
    status200,
    status400,
    status404,
  )
import qualified Network.HTTP.Types.Method as Method
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    requestMethod,
    responseLBS,
    strictRequestBody,
  )
import Network.Wai.Handler.Warp (run)
import Text.Printf (printf)

-- | -------------------------------------------|
--  |- Don't start here, go to Level02.Types!  -|
--  |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse s t = responseLBS s [("content-type", renderContentType t)]

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- | ----------------------------------------------------------------------------------
--  These next few functions will take raw request information and construct         --
--  one of our types.                                                                --
--                                                                                   --
--  By breaking out these smaller functions, we're able to isolate our               --
--  validation requirements into smaller components that are simpler to maintain     --
--  and verify. It also allows for greater reuse and it also means that              --
--  validation is not duplicated across the application, maybe incorrectly.          --

--------------------------------------------------------------------------------------

mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest x y =
  mkTopic x >>= (\t -> AddRq t <$> mkCommentText (lazyByteStringToStrictText y))
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest :: Text -> Either Error RqType
mkViewRequest x = ViewRq <$> mkTopic x

mkListRequest :: Either Error RqType
mkListRequest = pure ListRq

-- | ----------------------------------
--  end of RqType creation functions --

--------------------------------------

mkErrorResponse :: Error -> Response
mkErrorResponse (Error x) = resp400 PlainText (LBS.fromStrict (encodeUtf8 x))

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO (Either Error RqType)
mkRequest r =
  case (requestMethod r, pathInfo r, strictRequestBody r) of
    ("POST", t : ["add"], b) -> mkAddRequest t <$> b
    ("GET", t : ["view"], _) -> pure (mkViewRequest t)
    ("GET", ["list"], _) -> pure mkListRequest
    (m, p, _) ->
      pure
        ( Left
            ( Error
                ( T.pack (printf "unexpected request, method was %s, path was %s" (show m) (show p))
                )
            )
        )

-- Remembering your pattern-matching skills will let you implement the entire
-- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest (AddRq _ _) = pure $ resp200 PlainText "add not implemented yet"
handleRequest (ViewRq _) = pure $ resp200 PlainText "view not implemented yet"
handleRequest ListRq = pure $ resp200 PlainText "list not implemented yet"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app :: Application
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app r h = mkRequest r >>= h . either mkErrorResponse id . (>>= handleRequest)

runApp :: IO ()
runApp = run 3000 app
