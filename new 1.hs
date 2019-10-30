import Data.Char
import Data.List

toDecimal :: Int -> String -> String
toDecimal base snumber | base == 1 = show (length snumber)
		               | base > 1 && base <= 61 = count base snumber
     	 	           | otherwise = error "the number system is incorrect"
		        where
			     numInNum :: Char->Int
			     numInNum x   | ord x > 96 && ord x < 123 = ord x - 87
				              | ord x > 47 && ord x < 58 = ord x - 48
		      	  	          | ord x > 64 && ord x < 91 = ord x - 29
			                  | otherwise = error "incorecct symbol"
						   
			     count base snumber = show (foldl (\y number -> if (numInNum number < base) then y * base + numInNum number else error "num out") 0 snumber)
fromDecimal :: Int -> String -> String
fromDecimal base snumber  | base == 1 = replicate (((read (snumber)::Int)+1) '1')
			              | base > 1 && base <= 61 = count base (read snumber::Int) []
			              | otherwise = error "the number system is incorrect"
			   where
				count :: Int -> Int -> String -> String
				count base 0 num = num
				count base snumber num = count base (div snumber base) ((chr(intToChar (mod snumber base))):num)
				 where					intToChar x | x >= 0 && x < 10 = x + 48
				       		    | x >=10 && x < 36 = x + 87
						        | x >= 36 && x < 62 = x + 29
	   					        | otherwise = error "incorecct number"
						   												
convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = (fromDecimal toBase)(toDecimal fromBase snumber)