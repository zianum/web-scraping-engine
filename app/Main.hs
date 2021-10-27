module Main where

import Lib
import Text.HTML.Scalpel
import Data.List

main = do
  Just maintext <- scrapeURL "https://news.ycombinator.com/" maintextToData -- Scraps ID, Title and Rank
  Just subtext <- scrapeURL "https://news.ycombinator.com/" subtextToData   -- Scraps ID, Points and Comments
  let entries = merge maintext subtext                                      -- Merge by ID
  putStrLn $ unlines $ outputEntries entries                                -- Shows all the information scrapped
  let entries1 = sortBy (flip byComments) $ filterByLength (> 5) entries    -- Filters by title words > 5 and sort by comments
  putStrLn $ unlines $ outputEntries entries1                               -- Shows the information just filtered
  let entries2 = sortBy (flip byPoints) $ filterByLength (<= 5) entries     -- Filters by title words <= 5 and sor by points
  putStrLn $ unlines $ outputEntries entries2                               -- Shows the information just filtered
