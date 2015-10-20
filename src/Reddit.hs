{-# LANGUAGE OverloadedStrings #-}
module Reddit
  (Post(..),
  videoIdFromPost,
  filterYoutube,
  postStream
  ) where

import           Control.Applicative       (pure, (<$>), (<*>))
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (Exception, try)
import           Control.Lens              ((&), (.~), (^.), (^?))
import           Data.Aeson
import           Data.Foldable             (for_)
import           Data.Text                 (Text, append, pack, unpack)
import           Network.HTTP.Client       (HttpException (StatusCodeException))
import           Network.HTTP.Types.Status (Status (..))
import           Network.URL               (URL (..), URLType (Absolute), host,
                                            importURL, url_params, url_path)
import           Network.Wreq
import qualified Network.Wreq.Session      as S
import           Pipes
import qualified Pipes.Prelude             as P

data Post = Post {
          title  :: !Text,
          domain :: !Text,
          score  :: Int,
          url    :: !Text
} deriving (Show)

instance FromJSON Post where
  parseJSON (Object v) = do
    objectData <- v .: "data"
    title  <- objectData .: "title"
    domain <- objectData .: "domain"
    score  <- objectData .: "score"
    url    <- objectData .: "url"
    return $ Post title domain score url

data GetPostsResult = GetPostsResult {
  posts :: [Post],
  after :: Maybe Text
} deriving (Show)

instance FromJSON GetPostsResult where
  parseJSON (Object v) = do
    rootData   <- v        .:  "data"
    posts      <- rootData .:  "children"
    afterCode  <- rootData .:? "after"
    return $ GetPostsResult posts afterCode

redditVideoUrl = "https://www.reddit.com/r/all/top/.json?sort=top&t=day&after="

retryWithDelay  ::  IO a -> IO a
retryWithDelay x = threadDelay 10000000 >>= \_ -> x

fetchPageWithRetries :: S.Session -> Text -> Int -> IO (Maybe GetPostsResult)
fetchPageWithRetries sess afterCode retries  = do
  result <- try (fetchPage sess afterCode) :: IO (Either HttpException (Maybe GetPostsResult))
  case result of
    Left (StatusCodeException (Status 503 _ ) _ _) -> if retries > 0
    then retryWithDelay $  fetchPageWithRetries sess afterCode (retries - 1) else return Nothing
    Left _ -> return Nothing
    (Right r) -> return r

fetchPage :: S.Session -> Text -> IO (Maybe GetPostsResult)
fetchPage sess afterCode =
  S.get sess  (redditVideoUrl ++ unpack afterCode) >>= \response -> return $ decode $ response ^. responseBody

postStream :: S.Session -> Text -> Producer Post IO ()
postStream sess afterCode = do
  posts <- liftIO $ fetchPageWithRetries sess afterCode 4
  for_ posts $ \(GetPostsResult ps afterParam) -> do
    each ps
    for_ afterParam $ postStream sess

isCorrectDomain :: Text -> Bool
isCorrectDomain x = elem x validDomains

postIsValidDomain :: Post -> Bool
postIsValidDomain x = isCorrectDomain $ domain x

youtubeShortURL = "youtu.be"
youtubeLongURL = "youtube.com"

validDomains = prependHost [youtubeLongURL , youtubeShortURL]

prependHost :: [Text] -> [Text]
prependHost hs = ([append "www.", append "", append "m."] <*> hs)

filterYoutube ::  Pipe Post Post IO ()
filterYoutube  =  (P.filter postIsValidDomain)

isLongUrl :: URL -> Bool
isLongUrl u =  case (url_type u) of
  Absolute h -> elem ((pack . host) h) $ prependHost [youtubeLongURL]
  _ -> False

videoIdFromPost :: Post -> Maybe String
videoIdFromPost p = videoIdFromURL $ unpack $ url p

videoIdFromURL :: String -> Maybe String
videoIdFromURL u = case (importURL u) of
  Just x -> if isLongUrl x then videoIdFromLongURL x else videoIdFromShortURL x
  Nothing -> Nothing

videoIdFromLongURL :: URL -> Maybe String
videoIdFromLongURL x = let params = url_params x
                           videoParam =   lookup "v"  params
                          in case videoParam of
                           Nothing -> Nothing
                           Just "" -> Nothing
                           Just p -> Just p

videoIdFromShortURL :: URL -> Maybe String
videoIdFromShortURL url = let path = url_path url in
                              case path of
                                "" -> Nothing
                                s -> Just s
