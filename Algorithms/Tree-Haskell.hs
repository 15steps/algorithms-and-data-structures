
-- Tree
data Tree t = Nil | Node t (Tree t) (Tree t) deriving(Show, Eq)

insert Nil v = (Node v (Nil) (Nil)) 
insert (Node t left right) v | (v<t) = (Node t (insert left v) right)
                          | otherwise = (Node t left (insert right v))

treeToList :: Tree t -> [t]
treeToList Nil = []
treeToList (Node t left right) = t:(treeToList left ++ treeToList right)