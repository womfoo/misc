{-

this parses an email and writes all attachments to the current directory

make sure you have https://github.com/np/mime-bytestring and expose the Codec.MIME.Parse.parseMIMEMessage function

also hide the mime package while your at it
ghc-pkg hide mime
ghc-pkg expose mime-bytestring

-}
import Codec.MIME.Parse
import Codec.MIME.Type
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import System.Environment

main = do
  (input:_) <- getArgs
  bs <- B.readFile input
  let attachments = filter isAttachment $ flatten $ parseMIMEMessage (WithoutCRLF bs)
  forM_ attachments writeAttachment

flatten m  = squish m []
  where
    squish m' xs = case mime_val_content m' of
      Multi ms -> Prelude.foldr squish ms xs
      Single _ -> [m']

isAttachment m = case mime_val_disp m of
  Just d  -> dispType d == DispAttachment
  _       -> False

writeAttachment m = do
  putStr $ "Writing " ++ filename ++ "... "
  B.writeFile filename dat
  putStrLn "(Done)"
  where
    filename = fromMaybe "noname.dat" (findFileName m)
    dat      = case mime_val_content m of
                 (Single bs)  -> withoutCRLF bs
                 _            -> error "you should have never reached this code :-)"

findFileName m = case mime_val_disp m of
  Just d  -> case dispType d of
               DispAttachment -> case find isFilename (dispParams d) of
                 Just (Filename name) -> Just name
                 Nothing              -> Nothing
  Nothing -> Nothing

isFilename (Filename a) = True
isFilename _ = False
