module Codewars.Kata.Pagination where

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = (itemCount xs + n-1) `div` n

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page
  | page >= totalPages || page < 0 = Nothing
  | page == totalPages - 1 && totalItems `mod` n /= 0 = Just (totalItems `mod` n)
  | otherwise = Just n
  where
  totalItems = itemCount xs
  totalPages = pageCount xs n

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex xs n item = if item >= totalItems || item < 0 then Nothing else Just (item `div` n)
  where
  totalItems = itemCount xs
