data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ fe fd) = 1 + size fe + size fd

height :: Tree a -> Int
height Empty = 0
height (Node _ fe fd)
    | height fe >= height fd = 1 + height fe
    | otherwise = 1 + height fd


equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node a1 fe1 fd1) (Node a2 fe2 fd2) = a1 == a2 && equal fd1 fd2 && equal fe1 fe2

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a fe fd) = [a] ++ preOrder fe ++ preOrder fd

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a fe fd) = postOrder fe ++ postOrder fd ++ [a]


inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a fe fd) =  inOrder fe ++ [a] ++ inOrder fd


data Queue a = Queue [a] [a]
     deriving (Show)
 
create :: Queue a
push :: a -> Queue a -> Queue a
pop :: Queue a -> Queue a
top :: Queue a -> a
empty :: Queue a -> Bool

create = Queue [] []
push a (Queue l1 l2) = (Queue l1 (a:l2))

pop (Queue [] l2) = pop (Queue (reverse l2) [])
pop (Queue l1 l2) = (Queue (tail l1) l2)

top (Queue [] l2) = head $ reverse l2
top (Queue l1 _) = head l1

empty (Queue [] []) = True
empty _ = False

instance Eq a => Eq (Queue a)
     where
         (Queue l1 l2) == (Queue t1 t2) = l1 ++ reverse l2 == t1 ++ reverse t2