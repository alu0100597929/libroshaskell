import Data.List (take, takeWhile)
import Data.Numbers.Primes (primes)

sumaPrimos = sum (takeWhile (< 2000000) primes)