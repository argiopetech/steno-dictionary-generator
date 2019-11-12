module CommonIssues where

import Dictionary
import Stroke (Stroke)

import Control.Monad (when)
import Data.List (intersperse, delete, foldl', sortBy)
import Data.Ord (comparing)
import Text.Printf

import qualified Data.Map as M


checkEmpty = filter (null . stroke)
notEmpty   = filter (not . null . stroke)

printIfEmpty es = do
  let empties = not $ null es

  when empties $ do
    putStrLn "Missing definitions for the following:"
    putStrLn $ unlines $ map (\(Entry n _) -> printf "\t%s" n) es

  return empties

-- | Deduplicates entries by stroke, keeping the last occurrence of any
-- stroke. This simulates the loading of multiple steno dictionary without
-- needing multiple files.
--
-- Useful primarily for e.g., Dotterel, which, for all its lovely attributes,
-- has (as of version 0.2 beta) a very poor dictionary mangement system.


dedupEntries :: [Entry] -> [Entry]
dedupEntries es =
  let map = foldl' (\m e ->
                      M.insert (stroke e) e m)
                   M.empty es
  in M.elems map


checkDuplicate es =
  let map = foldl' (\m e ->
                      appendOrInsert (stroke e) (name e) m)
                   M.empty es
  in M.toList $ M.filter ((> 1) . length) map

  
printDuplicates es = do
  let duplicates = not $ null es

  when duplicates $ do
    putStrLn $ unlines $ intersperse "" $ map go es

  return duplicates
  where go (stroke, entries) =
          let header = printf "Duplicate entries for %s:" (prettyPrintStrokes stroke)
              rest   = concatMap (\e -> printf "\n\t%s" e) entries
          in header ++ rest


checkBoundaryErrors :: [Entry] -> [(Stroke, [String])]
checkBoundaryErrors es =
  let (b, w, e) = foldl' (\(b, w, e) entry ->
                            let n  = name   entry
                                ss = stroke entry
                            in case length ss of
                                 0 -> (b, w, e)
                                 1 -> (b, appendOrInsert (head ss) n w, e)
                                 _ -> (appendOrInsert (head ss) n b
                                      ,w
                                      ,appendOrInsert (last ss) n e))
                         (mempty, mempty, mempty)
                         es
      doUnion     = M.unionWith (++)
      doIntersect = M.intersectionWith (++)
      conflicts = doIntersect b e `doUnion`
                  doIntersect w b `doUnion`
                  doIntersect w e
  in M.toList $ conflicts
  
printBoundaryErrors conflicts = do
  let anyConflicts = not $ null conflicts
  
  when anyConflicts $ do
    putStr "Potential conflicting strokes:"
    putStrLn $ unlines
             $ map (\(k, vs) ->
                      printf "\n\t%s" (show k)
                   ++ concatMap (printf "\n\t\t%s") vs) conflicts
    
  return anyConflicts

appendOrInsert key value map = M.alter (f value) key map
  where f n Nothing   = Just [n]
        f n (Just ks) = Just (n:ks)
