module Parity where
import Codec
import Char

make_parity :: [Bit] -> Bit
make_parity [] = 0
make_parity (x:xs) = (x + make_parity xs) `mod` 2

add_parity :: [Bit] -> [Bit]
add_parity bits = (make_parity bits):bits

sane_parity :: Bit -> [Bit] -> Bool
sane_parity parity bits = parity == make_parity bits

split_parity :: [Bit] -> (Bit, [Bit])
split_parity bits = (head bits, tail bits)

check_parity :: [Bit] -> [Bit]
check_parity bits = if sane_parity parity encoded
                      then encoded
                      else error "Insane parity."
                    where
                      (parity, encoded) = split_parity bits

encode_parity :: String -> [Bit]
encode_parity = concat . map (add_parity . make8 . int2bin . ord)

decode_parity :: [Bit] -> String
decode_parity = map (chr . bin2int. check_parity) . chop 9

transmit = decode_parity . channel . encode_parity
channel = id
-- channel = tail

main = print $ transmit "There is no insane parity bits."
