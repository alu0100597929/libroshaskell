import Data.List (take, takeWhile)
import Data.Numbers.Primes (primes)
import Data.List

sumaPrimos = sum (takeWhile (< 2000000) primes)