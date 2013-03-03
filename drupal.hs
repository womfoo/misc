import Data.Maybe
import Network.Browser
import Network.CGI.Protocol
import Network.HTTP
import Text.HTML.TagSoup

u1 = "http://localhost/drupal7/"
u2 = "http://localhost/drupal7/?q=node&destination=node"

toParams (TagOpen _ attrs) = (name,value)
  where name = fromJust (lookup "name" attrs)
        value = fromMaybe "" $ lookup "value" attrs

overrideParams (name,value) = (name,newvalue)
   where newvalue = fromMaybe value $ lookup name overrides
         overrides = [("name","admin")
                     ,("pass","admin-passwd")]

main = do
  html <- browse $ do
    (uri,resp) <- request $ getRequest u1
    let rspTags = parseTags (rspBody resp)
        inputTags = filter (~== "<input>") rspTags
        params = map toParams inputTags
        paramsx = formEncode (map overrideParams params)
    (uri2,resp2) <- request $ postRequestWithBody u2
                              "application/x-www-form-urlencoded"
                              paramsx
    return $ rspBody resp2
  print html
