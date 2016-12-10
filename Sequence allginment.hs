{-# OPTIONS_GHC -Wall #-}
module Soal1.DNA where

data Edit = Same |  --Edit ketika char yang dibandingkan sama
 Different |   --Edit ketika char yang dibandingkan berbeda
 Space1 |   --Edit untuk memberikan spasi pada string pertama
 Space2    --Edit untuk memberikan spasi pada string kedua
 deriving (Eq,Show)

--Transformasi kedua string menjadi list of edit yang sesuai pada setiap karakternya
transform :: String -> String -> [Edit]
transform [] [] = []         --base case
transform [] ys = (Space1):transform [] (tail ys)   --ketika string pertama kosong, isi semua dengan space
transform xs [] = (Space2):transform (tail xs) []   --ketika string kedua kosong, isi semua dengan space
transform (x:xs) (y:ys)         --recursive, cari mana yang paling maksimal diantara semua kombinasi edit
 | x == y = Same : transform xs ys
 | otherwise = best [ Different : transform xs ys,
     Space1 : transform (x:xs) ys, 
     Space2 : transform xs (y:ys)]

-- mencari edit maksimal
best :: [[Edit]] -> [Edit]
best [] = error "element tidak boleh kosong";
best [x] = x
best (x:xs)
 | cost x >= cost b = x
 | otherwise = b
  where
  b = best xs

-- menghitung nilai dari list of edit yang sesuai dengan cost masing2 edit
cost :: [Edit] -> Int
cost [] = 0
cost (Same:xs) = 1 + cost xs
cost (Different:xs) = -1 + cost xs
cost (Space1:xs) = -2 + cost xs
cost (Space2:xs) = -2 + cost xs

--string1 untuk generate output baris pertama, yaitu apabila memasuka spasi ketika ada Edit Space1 
string1 :: [Edit] -> String -> String
string1 [] _  = ""
string1 (x:xs) ys
 | x == Space1 = ' ' : (string1 xs ys)
 | otherwise = (head ys):string1 xs (tail ys)

--string2 untuk generate output baris kedua, yaitu apabila memasuka spasi ketika ada Edit Space2
string2 :: [Edit] -> String -> String
string2 [] _  = ""
string2 (x:xs) ys
 | x == Space2 = ' ' : (string2 xs ys)
 | otherwise = (head ys):string2 xs (tail ys)

--string3 untuk generate output baris ketiga, yaitu apabila Different diganti -, Same +, Sapce1 dan Space2 *
string3 :: [Edit] -> String
string3 [] = ""
string3 (x:xs)
 | x == Different = '-':string3 xs
 | x == Same = '+':string3 xs
 | otherwise = '*':string3 xs

showAlign :: String -> String -> String
showAlign str1 str2 = string1 edit str1 ++ "\n" ++ string2 edit str2 ++ "\n" ++ string3 edit ++ "\n" ++ show (cost edit) ++ "\n"
 where edit = transform str1 str2