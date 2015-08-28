import Data.List as List
import Data.Time
import Data.Time.Format
import GHC.Exts
import Data.Maybe
import Control.Arrow

data TransactionData = TransactionData { 
    key :: String,
    value :: String
} deriving Show

data Transaction = Transaction {
    name :: String,
    fields :: [TransactionData]
} deriving Show

t1 = Transaction "A1" [TransactionData "amount" "500.00", TransactionData "date" "2015-06-11"]
t2 = Transaction "A2" [TransactionData "amount" "700.00", TransactionData "date" "2015-06-11"]

findAmountValue :: Transaction -> Maybe Double
findAmountValue (Transaction id xs) = m >>= (\x -> return $ read $ value x)
    where m = List.find (\x -> key x == "amount") xs

--findDateValue :: Transaction -> Maybe Double
--findDateValue (Transaction id xs) = m >>= (\x -> return $ parseTimeOrError True defaultTimeLocale "%Y-%b-%d" $ value x)
--    where m = List.find (\x -> key x == "date") xs

compareAmounts :: Transaction -> Transaction -> Bool
compareAmounts a b = if (isJust a') == True && (isJust b') == True then a' <= b' else True
    where 
        a' = findAmountValue a
        b' = findAmountValue b

potentialMatches :: [Transaction] -> [Transaction] -> [(String, [String])]
potentialMatches ps bs = map tupleToNames $ map (\b -> potentialMatchesByAmount b ps) bs
    where tupleToNames = name *** (map name)
    
potentialMatchesByAmount :: Transaction -> [Transaction] -> (Transaction, [Transaction])
potentialMatchesByAmount b ps = (b, matches)
    where 
        sortedPs = sortWith findAmountValue ps
        matches = takeWhile (\p -> compareAmounts p b) sortedPs