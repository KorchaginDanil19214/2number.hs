import Data.Char
import Data.List

toDecimal :: Int -> String -> String
toDecimal base snumber         | base == 1 = show (length snumber)
		               | base > 1 && base < 62 = count base snumber
     	 	               | otherwise = error "the number system is incorrect"
		        where
			     numInNum :: Int->Int
			     numInNum x |  x > 96 &&  x < 123 = x - 87
				        |  x > 47 &&  x < 58 =  x - 48
		      	  	        |  x > 64 &&  x < 91 =  x - 29
			                | otherwise = error "incorecct symbol"
							count base snumber = show (foldl (\y number -> if (numInNum(ord number) < base) then y * base + numInNum( ord number) else error "num out range") 0 snumber)
						   
			     
fromDecimal :: Int -> String -> String
fromDecimal base snumber              | base == 1 = replicate (((read (snumber)::Int)+1) '1')
			              | base > 1 && base < 62 = count base (read snumber::Int) []
			              | otherwise = error "the number system is incorrect"
			   where
				count :: Int -> Int -> String -> String
				count base 0 num = num
				count base snumber num = count base (div snumber base) ((chr (intToChar (mod snumber base)) ):num)
				 where					
				    intToChar x         | x >= 0 && x < 10 = x + 48
				       		        | x >9 && x < 36 = x + 87
						        | x > 35 && x < 62 = x + 29
	   					        | otherwise = error "incorecct number"
						   												
convertFromTo :: Int -> Int -> String -> String
convertFromTo fromBase toBase snumber = (fromDecimal toBase)(toDecimal fromBase snumber)
