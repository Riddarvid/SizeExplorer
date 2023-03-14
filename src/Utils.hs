module Utils (bytesToHuman) where
import Text.Printf (printf)

base :: Integer
base = 1024

prefixes :: [String]
prefixes = ["", "K", "M", "G", "T", "P"]

bytesToHuman :: Integer -> String
bytesToHuman n | n < 0 = error "Size must be positive."
bytesToHuman n = printf "%-7.2f %-sB" n' prefix
  where
    q' = if n == 0 
      then 0
      else logBase (fromIntegral base) (fromIntegral n)
    q = floor (q' :: Double)
    factor = base ^ toInteger q
    n' = fromInteger n / fromInteger factor :: Double
    prefix = prefixes !! q