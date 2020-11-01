
-- prime number
factor n = [i | i<-[1..n], n `mod` i == 0]
prime n = if (factor n) == [1,n] then True
                                  else False