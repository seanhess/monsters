module Main where

import App.Parse
import Data.Maybe (fromMaybe)
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
  let filePath = "data/Woods.xml"
  inp <- Utf8.readFile filePath
  -- let elems = parseXML inp

  putStrLn "PARSING"
  mons <- parseIO parseMonsters filePath inp

  mapM_ print mons

data Monster = Monster
  { name :: Text
  , tags :: [Text]
  , stats :: Maybe Stats
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

data Stats = Stats
  { attack :: Attack
  , hp :: Int
  , armor :: Int
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
    ts <- optional $ try tags
    pure (n, fromMaybe [] ts)

  stats <- optional $ try $ parseStats

  qual <- optional $ try $ paragraph "MonsterQualities" qualities
  (desc, inst) <- paragraph "MonsterDescription" description

  mvs <- moves

  pure
    $ Monster
      { name = n
      , tags = ts
      , stats = stats
      , qualities = qual
      , description = desc
      , instinct = inst
      , moves = mvs
      }
 where
  name :: Parser Text
  name = do
    -- parse until tab
    nm <- manyTill anySingle (string "\t")
    pure $ pack nm

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

parseStats :: Parser Stats
parseStats = do
  (name, damage, hp, armor) <- paragraph "MonsterStats" line1
  tgs <- paragraph "MonsterStats" tags
  pure $ Stats{hp, armor, attack = Attack{name, damage, tags = tgs}}
 where
  line1 :: Parser (Text, Text, Int, Int)
  line1 = do
    att <- manyTill anySingle (string " (")
    dmg <- manyTill anySingle (char ')')
    space
    hp <- decimal
    _ <- string " HP"
    space
    armor <- decimal
    _ <- string " Armor"
    pure (pack att, pack dmg, hp, armor)

paragraph :: Text -> Parser a -> Parser a
paragraph att prs = do
  _ <- string "<p aid:pstyle=\""
  _ <- string att
  _ <- string "\">"
  a <- prs
  _ <- string "</p>"
  _ <- newline
  pure a

tags :: Parser [Text]
tags = do
  _ <- string "<span aid:cstyle=\"Tags\">"
  ts <- some alphaNumChar `sepEndBy` string ", "
  _ <- string "</span>"
  pure $ map pack ts
