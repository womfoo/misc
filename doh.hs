{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser

main :: IO ()
main = do
  result <- withManager $ \manager -> do
    browse manager $ do
      let url = "https://api.digitalocean.com/droplets/"
          params = [("client_id","replace-me")
                   ,("api_key","replace-me")]
      req <- fmap (urlEncodedBody params) $ parseUrl url
      Response _ _ _ body3 <- makeRequestLbs $ req { method = "GET"}
      return ( decode body3 :: Maybe Value )
  print result
