-- works for basic auth
import Network.Browser (browse, setAuthorityGen,request)
import Network.HTTP (getRequest)

main = do
  result <- browse $ do
    setAuthorityGen (\_ _ -> return (Just ("test","password1234")))
    request $ getRequest "http://localhost/basictest/secretfile"
  print result
