
-- Stack
data Stack t = Nil  | No t (Stack t) deriving(Show)

push :: Stack t -> t -> Stack t
push Nil n = (No n Nil)
push (No t tail) n = (No n (No t tail)) 

pop :: Stack t -> Stack t
pop Nil = Nil
pop (No t tail) = tail

stackToList :: Stack t -> [t]
stackToList Nil = []
stackToList (No t tail) = t:stackToList tail

listToStack :: [t] -> Stack t
listToStack [] = Nil
listToStack (x:xs) = aux (No x Nil) xs
            where
                aux stack [] = stack
                aux stack (x:xs) = aux (push stack x) xs