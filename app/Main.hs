module Main where

import Control.Monad (forM_)
import Data.Either (lefts, rights)
import Data.List
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as Utf8
import Text.XML.Light

main :: IO ()
main = do
  inp <- Utf8.readFile "data/Caverns.xml"
  let elems = parseXML inp

  putStrLn "PARSING"
  Just bod <- pure $ findBody elems
  print bod.elName
  print $ length bod.elContent

  let mels = groupMonsters bod
  print $ length mels

  let ems = map parseMonster mels
  let errs = lefts ems
  let mons = rights ems

  putStrLn "ERRORS"
  forM_ errs $ \e -> do
    fail e

  putStrLn "MONSTERS"
  let m = head mons
  print m
  putStrLn $ T.unpack m.description

findBody :: [Content] -> Maybe Element
findBody [] = Nothing
findBody elems = do
  listToMaybe $ mapMaybe findBody' elems
 where
  findBody' :: Content -> Maybe Element
  findBody' (Elem e) =
    if e.elName.qName == "Body"
      then Just e
      else findBody e.elContent
  findBody' _ = Nothing

element :: Content -> Maybe Element
element (Elem e) = Just e
element _ = Nothing

-- oh no, they're nested!
groupMonsters :: Element -> [[Content]]
groupMonsters body =
  groupBy sameMonster body.elContent
 where
  sameMonster :: Content -> Content -> Bool
  sameMonster _ (Elem (Element _ (a : _) _ _)) =
    a.attrVal /= "MonsterName"
  sameMonster _ _ = True

data Monster = Monster
  { name :: Text
  , attack :: Text
  , hp :: Text
  , armor :: Text
  , description :: Text
  }
  deriving (Show)

parseMonster :: [Content] -> Either String Monster
parseMonster cnt = do
  name <- contentText <$> paragraph "MonsterName" cnt
  stats <- contentText <$> paragraph "MonsterStats" cnt
  (attack, hp, armor) <- parseStats stats
  description <- contentText <$> paragraph "MonsterDescription" cnt
  pure $ Monster{name, description, hp, armor, attack}
 where
  isAnyAttr :: String -> Content -> Bool
  isAnyAttr n (Elem el) = any (\a -> a.attrVal == n) el.elAttribs
  isAnyAttr _ _ = False

  paragraph :: String -> [Content] -> Either String Content
  paragraph n cs =
    maybe (Left "Paragraph") pure $ find (isAnyAttr n) cs

  contentText :: Content -> Text
  contentText (Text c) = pack c.cdData
  contentText (Elem e) = T.strip $ pack $ unwords $ map (.cdData) $ onlyText e.elContent
  contentText _ = ""

  parseStats :: Text -> Either String (Text, Text, Text)
  parseStats t = do
    case filter (not . T.null) $ T.splitOn "\t" t of
      [att, hp, armor] -> Right (att, hp, armor)
      w -> Left $ show w
