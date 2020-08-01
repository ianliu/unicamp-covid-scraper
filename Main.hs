{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.Maybe
import           Data.Text.Lazy.Encoding
import           Data.Text.Lazy.Read
import           Debug.Trace
import           Network.HTTP.Conduit
import           Text.HTML.TagSoup
import qualified Data.Text.Lazy as T
import           System.Environment
import           Control.Exception

data Date = Date
    { year  :: Int
    , month :: Int
    , day   :: Int
    } deriving (Show)

data Report = Report
    { deaths :: Int
    , cases  :: Int
    , date   :: Date
    } deriving (Show)

covidUrl :: Int -> String
covidUrl id =
    "https://www.unicamp.br/unicamp/coronavirus/boletim-"
        ++ (show id) ++ "-atendimento-coronavirus"

openReport :: Int -> IO (Either HttpException T.Text)
openReport =
    try . fmap decodeUtf8 . (simpleHttp . covidUrl)

parseHeader :: T.Text -> Maybe Date
parseHeader text =
    let parts = T.splitOn "/" text
        year  = decimal $ T.take 4 $ parts !! 2
        month = decimal $ parts !! 1
        day'  = head parts
        day   = decimal $ T.drop (T.length day' - 2) day'
        date  = Date <$> (fst <$> year)
                     <*> (fst <$> month)
                     <*> (fst <$> day)
    in
        case date of
            Left _  -> Nothing
            Right d -> Just d

labelIsCase :: T.Text -> Bool
labelIsCase text
    | text == "Positivos"   = True
    | text == "Confirmados" = True
    | otherwise             = False

labelIsDeath :: T.Text -> Bool
labelIsDeath = T.isPrefixOf "Óbitos"

firstTdText :: [Tag T.Text] -> T.Text
firstTdText
    = T.unwords
    . T.words
    . innerText
    . head
    . partitions (~== ("<td>" :: String))

lastTdText :: [Tag T.Text] -> T.Text
lastTdText
    = T.unwords
    . T.words
    . innerText
    . last
    . partitions (~== ("<td>" :: String))

rightToJust :: Either a b -> Maybe b
rightToJust (Right x) = Just x
rightToJust _         = Nothing

lastInt :: [Tag T.Text] -> Maybe Int
lastInt tags =
    fst <$> (rightToJust $ (decimal . lastTdText) tags)

dbg :: (Show a) => String -> a -> a
dbg s a =
    trace (s ++ ": " ++ (show a)) a

takeBetween :: T.Text -> [Tag T.Text] -> [Tag T.Text]
takeBetween name =
    takeWhile (~/= TagClose name) . dropWhile (~/= (TagOpen name []))

parseTextReport :: T.Text -> Maybe Report
parseTextReport text =
    let rows
            = (fmap $ takeBetween "tr")
            $ partitions (~== ("<tr>" :: String))
            . dropWhile (~/= ("<tbody>" :: String))
            . parseTags
            $ text
        header = lastTdText $ head rows
        deaths = lastInt =<< find (labelIsDeath . firstTdText) rows
        cases =  lastInt =<< find (labelIsCase . firstTdText) rows
        in Report (fromMaybe 0 deaths) (fromMaybe 0 cases) <$> (parseHeader header)

parseReport :: Int -> IO (Maybe Report)
parseReport id = do
    result <- openReport id
    return $ report result
        where
            report (Left _)  = Nothing
            report (Right t) = parseTextReport t

main :: IO ()
main = do
    id     <- (read . head) <$> getArgs
    report <- parseReport id
    putStrLn $ show report

-- vim: set sw=4 et sta:
