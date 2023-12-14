{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import App.Parse
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import System.Directory
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

-- import Data.Text qualified as T
-- import Debug.Trace
--
--
files :: [FilePath]
files =
  [ "data/Woods.xml"
  , "data/Caverns.xml"
  , "data/Depths.xml"
  , "data/Experiments.xml"
  , "data/Folk.xml"
  , "data/Hordes.xml"
  , "data/Planes.xml"
  , "data/Swamp.xml"
  , "data/Undead.xml"
  , "data/Woods.xml"
  ]

main :: IO ()
main = do
  forM_ files $ \f -> do
    convertFile f "output"

-- let elems = parseXML inp

-- putStrLn "PARSING"
-- mons <- parseIO parseMonsters filePath inp
-- putStrLn $ T.unpack $ render $ head mons

convertFile :: FilePath -> FilePath -> IO ()
convertFile src dir = do
  let settingName = dropExtension $ takeFileName src
      outputDir = dir </> settingName
  createDirectoryIfMissing True outputDir
  inp <- Utf8.readFile src
  mons <- parseIO parseMonsters src inp
  forM_ mons $ \m -> do
    let outFile = outputDir </> T.unpack m.name <> ".md"
    putStrLn $ "Writing " <> outFile
    writeFile outFile (T.unpack $ render m)

-- mapM_ print mons

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
    ts <- optional $ try parseTags
    pure (n, fromMaybe [] ts)

  stats <- optional $ try parseStats

  qual <- optional $ try $ paragraph "MonsterQualities" qualities
  (desc, inst) <- multiDescription

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

  multiDescription :: Parser (Text, Text)
  multiDescription = do
    (des, ins) <- paragraph "MonsterDescription" description
    pure (cleanDescription des, ins)

  cleanDescription :: Text -> Text
  cleanDescription =
    T.strip
      . T.replace "<p aid:pstyle=\"NoIndent\">" ""
      . T.replace "</p>" ""

  description :: Parser (Text, Text)
  description = do
    d <- manyTill anySingle (string "<em>Instinct</em>:")
    space
    ins <- some $ anySingleBut '<'
    pure (pack d, pack ins)

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
  tgs <- paragraph "MonsterStats" parseTags
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

parseTags :: Parser [Text]
parseTags = do
  _ <- string "<span aid:cstyle=\"Tags\">"
  ts <- some alphaNumChar `sepEndBy` string ", "
  _ <- string "</span>"
  pure $ map pack ts

render :: Monster -> Text
render m = T.intercalate "\n\n" sections
 where
  sections =
    filter
      (not . T.null)
      $ [ tags m.tags
        , instinct m.instinct
        , maybe "" stats m.stats
        , maybe "" quality m.qualities
        , maybe "" (attack . (.attack)) m.stats
        , description m.description
        ]
      <> map move m.moves

  tag t = "#" <> t
  tags ts = T.intercalate " " $ map tag ts
  instinct is = "**Instinct**: " <> is
  quality q = "**Special Qualities**: " <> q
  description d = "*" <> d <> "*"

  move mv = ">" <> mv

  attack a =
    let line1 = "> **" <> a.name <> "** (" <> meta "dmg" a.damage <> ")"
        line2 = "> *" <> T.intercalate ", " a.tags <> "*"
     in T.intercalate "\n" [line1, line2]

  meta :: Text -> Text -> Text
  meta f t = "(" <> f <> " :: " <> t <> ")"

  stats :: Stats -> Text
  stats s =
    let hp = meta "hp" (pack $ show s.hp)
        arm = meta "armor" (pack $ show s.armor)
     in [i||       |         |
| ----- | ------- |
| #{hp} HP | #{arm} Armor ||]
