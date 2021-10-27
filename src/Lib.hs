{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( maintextToData
    , subtextToData 
    , merge 
    , outputEntries
    , filterByLength
    , byComments
    , byPoints
    ) where

import Text.HTML.Scalpel

-- Data type to store the scrapped information
data Entry = Entry { orderID      :: Int
                   , rank         :: Int
                   , title        :: String
                   , points       :: Int
                   , numComments  :: Int
                   } deriving (Show, Eq, Ord)


{- SCRAPING RULES 
   This implementation uses the Scalpel library:
   https://hackage.haskell.org/package/scalpel-0.6.2/docs/Text-HTML-Scalpel.html
-} 

maintextToData :: Scraper String [Entry]
maintextToData = chroots ("tr" @: [hasClass "athing"]) orderAndTitle

subtextToData :: Scraper String [Entry]
subtextToData = chroots ("td" @: [hasClass "subtext"]) pointsAndComments

orderAndTitle :: Scraper String Entry
orderAndTitle = do
  orderID <- attr "id" "tr"
  rank <- text $ "span" @: [hasClass "rank"]
  title <- text $ "a" @: [hasClass "titlelink"]
  return $ Entry {orderID = toIntID orderID, rank = read $ init rank , title = title, points = 0, numComments = 0}

pointsAndComments :: Scraper String Entry
pointsAndComments = do
  orderID <- attr "id" $ "span" @: [hasClass "score"]
  points <- text $ "span" @: [hasClass "score"]
  comments <- texts "a"
  return $ Entry {orderID = toIntID orderID, title = "", rank = 0, points = toIntPoints points, numComments = toIntComments comments}

{- FORMATTING AND CLEANING
   Includes functions for data formatting, cleaning and outputting 
-}

-- Merge of Entry lists by orderID (Entry list with Rank and Title and Entry list with Points and #Comments 
merge :: [Entry] -> [Entry] -> [Entry]
merge [] _ = []
merge xs [] = xs
merge (x:xs) (y:ys) | orderID x == orderID y = Entry {orderID = orderID x, rank = rank x, title = title x, points = points y, numComments = numComments y} : merge xs ys
                    | otherwise              = x : merge xs (y:ys)

toIntID :: String -> Int
toIntID str | head str == 's' = read $ tail $ dropWhile (/= '_') str
            | otherwise       = read str

toIntPoints :: String -> Int
toIntPoints = read . takeWhile (/= ' ') 

toIntComments :: [String] -> Int
toIntComments str | str == []             = 0
                  | last str == "discuss" = 0
                  | otherwise             = read $ takeWhile (/= ' ') . map replace $ last str

replace :: Char -> Char
replace '\160' = ' '
replace x = x

outputEntry :: Entry -> String
outputEntry x = show (rank x) ++ "\t" ++ show (orderID x) ++ " | " ++ title x ++ " | " ++ show (points x) ++ " points | " ++ show (numComments x) ++ " comments"

outputEntries :: [Entry] -> [String]
outputEntries = map outputEntry

{- FILTERING
   Includes functions to filter the scrapped data:
   filter1 - entries with titles with more than 5 words sorted by #comments
   filter2 - entries with titles with less than or equal to 5 words sorted by points 
-}

filterByLength :: (Int -> Bool) -> [Entry] -> [Entry]
filterByLength f x = filter (\x -> f $ length (words (title x))) x

byComments :: Entry -> Entry -> Ordering
byComments x y | numComments x > numComments y  = GT
               | numComments x < numComments y  = LT
               | otherwise                      = EQ

byPoints :: Entry -> Entry -> Ordering
byPoints x y | points x > points y = GT
             | points x < points y = LT
             | otherwise           = EQ
