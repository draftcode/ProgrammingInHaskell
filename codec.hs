module Codec where
import Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b| (w, b) <- zip weights bits]
               where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop n bits = take n bits : chop n (drop n bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop 8

