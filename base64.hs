import Data.ByteString.Base64
import Data.ByteString.Char8
import Prelude hiding (readFile,writeFile)

main = do bs <- readFile "/home/womfoo/base64.pdf"
          writeFile "crap.pdf" $ decodeLenient bs
