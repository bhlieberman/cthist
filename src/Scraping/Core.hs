{-# LANGUAGE OverloadedStrings #-}

module Scraping.Core (showVersions) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.UTF8
import Data.CaseInsensitive (CI, mk)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import qualified Scraping.Study as S
import qualified Scraping.Version as V

userAgent :: CI BS.ByteString
userAgent = mk $ fromString "User-Agent"

userAgentVal :: BS.ByteString
userAgentVal = fromString "Mozilla/5.0 (X11; Linux x86_64; rv:123.0) Gecko/20100101 Firefox/123.0"

accept :: CI ByteString
accept = mk $ fromString "Accept"

acceptVal :: ByteString
acceptVal = fromString "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"

mkRequest :: String -> Request
mkRequest nct =
  defaultRequest
    { host = "clinicaltrials.gov",
      path = fromString_ nct,
      requestHeaders = [(userAgent, userAgentVal), (accept, acceptVal)]
    }

getVersionHistory :: String -> IO (Response BS.ByteString)
getVersionHistory nct = httpBS $ mkRequest nct

fromString_ :: String -> BS.ByteString
fromString_ nct = fromString $ "/api/int/studies/" <> nct <> "/history"

processResp :: IO ByteString
processResp = do
  resp <- getVersionHistory "NCT00000125"
  return $ getResponseBody resp

decodeResp :: IO V.VersionHistory
decodeResp = do
  json <- (eitherDecodeStrict <$> processResp) :: IO (Either String V.VersionHistory)
  case json of
    (Right j) -> return j
    Left _ -> return V.VersionHistory {V.changes = []}

-- getVersions :: (FromJSON a) => V.VersionHistory -> IO (Response a)
getVersions :: FromJSON a => V.VersionHistory -> IO [Either String a]
getVersions vs =
  let version_nos = map V.version $ V.changes vs
   in forM
        (Prelude.take 3 version_nos)
        ( \v ->
            let url = "clinicaltrials.gov"
                req =
                  defaultRequest
                    { path = fromString $ "/api/int/studies/NCT00000125/history/" <> show v,
                      host = url
                    }
                resp = httpBS req :: IO (Response BS.ByteString)
             in eitherDecodeStrict . getResponseBody <$> resp
        )

matchVersions :: Either String S.Study -> IO ()
matchVersions v = case v of
  Right val -> print val
  Left err -> print err

showVersions :: IO ()
showVersions =
  decodeResp
    >>= ( \resp -> do
            vs <- getVersions resp :: IO [Either String S.Study]
            mapM_ matchVersions vs
        )
