module App.Parse where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseIO :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s a -> String -> s -> IO a
parseIO parser src inp =
  case parse parser src inp of
    Left bundle -> do
      putStrLn (errorBundlePretty bundle)
      fail "Failed Parse"
    Right g -> pure g

line :: Parser Text
line = do
  pack <$> manyTill anySingle newline
