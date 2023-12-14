module Main where

import App.Parse
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

-- import Data.Text qualified as T
-- import Debug.Trace

main :: IO ()
main = do
  inp <- Utf8.readFile "data/Caverns.xml"
  -- let elems = parseXML inp

  putStrLn "PARSING"
  mons <- parseIO parseMonsters "data/Caverns.xml" inp

  mapM_ print mons

data Monster = Monster
  { name :: Text
  , tags :: [Text]
  , attack :: Attack
  , hp :: Int
  , armor :: Int
  , qualities :: Maybe Text
  , description :: Text
  , instinct :: Text
  , moves :: [Text]
  }
  deriving (Show)

data Attack = Attack
  { name :: Text
  , damage :: Text
  , tags :: [Text]
  }
  deriving (Show)

parseMonsters :: Parser [Monster]
parseMonsters = do
  _ <- manyTill anySingle (string "<Body>")
  mons <- parseMonster `sepEndBy` newline
  _ <- string "</Body></Root>"
  pure mons

parseMonster :: Parser Monster
parseMonster = do
  (n, ts) <- paragraph "MonsterName" $ do
    n <- name
    ts <- tags
    pure (n, ts)

  (atk, dmg, hp, armor) <- paragraph "MonsterStats" stats
  att <- paragraph "MonsterStats" tags

  qual <- optional $ try $ paragraph "MonsterQualities" qualities
  (desc, inst) <- paragraph "MonsterDescription" description

  mvs <- moves

  pure
    $ Monster
      { name = n
      , tags = ts
      , attack = Attack atk dmg att
      , hp
      , armor
      , qualities = qual
      , description = desc
      , instinct = inst
      , moves = mvs
      }
 where
  paragraph :: Text -> Parser a -> Parser a
  paragraph att prs = do
    _ <- string "<p aid:pstyle=\""
    _ <- string att
    _ <- string "\">"
    a <- prs
    _ <- string "</p>"
    _ <- newline
    pure a

  name :: Parser Text
  name = do
    -- parse until tab
    nm <- manyTill anySingle (string "\t")
    pure $ pack nm

  tags :: Parser [Text]
  tags = do
    _ <- string "<span aid:cstyle=\"Tags\">"
    ts <- some alphaNumChar `sepBy` string ", "
    _ <- string "</span>"
    pure $ map pack ts

  stats :: Parser (Text, Text, Int, Int)
  stats = do
    att <- manyTill anySingle (string " (")
    dmg <- manyTill anySingle (char ')')
    space
    hp <- decimal
    _ <- string " HP"
    space
    armor <- decimal
    _ <- string " Armor"
    pure (pack att, pack dmg, hp, armor)

  qualities :: Parser Text
  qualities = do
    _ <- string "<strong>Special Qualities:</strong>"
    space
    t <- some $ anySingleBut '<'
    pure $ pack t

  description :: Parser (Text, Text)
  description = do
    d <- manyTill anySingle (string "<em>Instinct</em>:")
    space
    i <- some $ anySingleBut '<'
    pure (T.strip (pack d), pack i)

  moves :: Parser [Text]
  moves = do
    _ <- string "<ul>"
    mvs <- move `sepBy` newline
    _ <- string "</ul>"
    pure mvs

  move :: Parser Text
  move = do
    _ <- string "<li>"
    pack <$> manyTill anySingle (string "</li>")
