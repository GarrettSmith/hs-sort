import Data.List
{-import System.Random-}

type Comparator   a = a -> a -> Ordering
type SimpleSorter a = [a] -> [a]
type Sorter       a = Comparator a -> SimpleSorter a

------------------------------------------------------------------------------
-- Selection Sort
------------------------------------------------------------------------------
selectionsort :: Ord a => SimpleSorter a
selectionsort = selectionsortBy compare

selectionsortBy :: Ord a => Sorter a
selectionsortBy _   [] = []
selectionsortBy cmp xs = min : (selectionsortBy cmp unsorted)
  where 
    min      = minimumBy cmp xs
    unsorted = delete min xs

------------------------------------------------------------------------------
-- Insertion Sort
------------------------------------------------------------------------------
insertionsort :: Ord a => SimpleSorter a
insertionsort = insertionsortBy compare

insertionsortBy :: Ord a => Sorter a
insertionsortBy cmp xs = foldr (insertBy cmp) [] xs

------------------------------------------------------------------------------
-- Merge Sort
------------------------------------------------------------------------------
mergesort :: Ord a => SimpleSorter a
mergesort = mergesortBy compare

mergesortBy :: Ord a => Sorter a
mergesortBy cmp xs = mergesortBy' cmp (map wrap xs)

wrap :: a -> [a]
wrap a = [a]

mergesortBy' :: Ord a => Comparator a -> [[a]] -> [a]
mergesortBy' _   []   = []
mergesortBy' _   [xs] = xs
mergesortBy' cmp xss  = mergesortBy' cmp (mergePairs cmp xss)

mergePairs :: Ord a => Comparator a -> [[a]] -> [[a]]
mergePairs _   []          = []
mergePairs _   [xs]        = [xs]
mergePairs cmp (xs:ys:zss) = mergeBy cmp xs ys : mergePairs cmp zss

mergeBy :: Ord a => Comparator a -> [a] -> [a] -> [a]
mergeBy _   xs     []     = xs
mergeBy _   []     ys     = ys
mergeBy cmp (x:xs) (y:ys) | cmp x y == LT = x : mergeBy cmp xs (y:ys)
                          | otherwise     = y : mergeBy cmp (x:xs) ys

------------------------------------------------------------------------------
-- Heap Sort
------------------------------------------------------------------------------
heapsort :: Ord a => SimpleSorter a
heapsort = heapsortBy compare

heapsortBy :: Ord a => Sorter a
heapsortBy cmp = flattenHeap cmp . mergeHeaps cmp . map heapify

-- http://stackoverflow.com/questions/932721/efficient-heaps-in-purely-functional-languages/932945#932945
treefold :: (a -> a -> a) -> a -> [a] -> a
treefold f zero [] = zero
treefold f zero [x] = x
treefold f zero (a:b:l) = treefold f zero (f a b : pairfold l)
  where 
    pairfold (x:y:rest) = f x y : pairfold rest
    pairfold l = l

data Heap a = Nil | Node a [Heap a]

heapify :: a -> Heap a
heapify x = Node x []

flattenHeap :: Ord a => Comparator a -> Heap a -> [a]
flattenHeap _   Nil            = []
flattenHeap cmp (Node x heaps) = x : flattenHeap cmp (mergeHeaps cmp heaps)

mergeHeaps :: Ord a => Comparator a -> [Heap a] -> Heap a
mergeHeaps cmp = treefold (mergeHeaps' cmp) Nil

mergeHeaps' :: Ord a => Comparator a -> Heap a -> Heap a -> Heap a
mergeHeaps' _   heap            Nil             = heap
mergeHeaps' cmp na@(Node a nas) nb@(Node b nbs)
  | (cmp a b) == LT = Node a (nb:nas)
  | otherwise       = Node b (na:nbs)

------------------------------------------------------------------------------
-- Quick Sort
------------------------------------------------------------------------------
quicksort :: Ord a => SimpleSorter a
quicksort = quicksortBy compare

quicksortBy :: Ord a => Sorter a
quicksortBy _   []     = []
quicksortBy cmp (p:xs) = (quicksort less) ++ [p] ++ (quicksort more)
  where (less, more) = partition (\x -> (cmp x p) == LT ) xs

------------------------------------------------------------------------------
-- Bubble Sort
------------------------------------------------------------------------------
bubblesort :: Ord a => SimpleSorter a 
bubblesort = bubblesortBy compare

bubblesortBy :: Ord a => Sorter a
bubblesortBy _   [] = []
bubblesortBy cmp xs = bubblesortBy' cmp xs [] False

bubblesortBy' :: Ord a => Comparator a -> [a] -> [a] -> Bool -> [a]
bubblesortBy' _   [x]        ys False = reverse (x:ys)
bubblesortBy' cmp [x]        ys True  
  = bubblesortBy' cmp (reverse (x:ys)) [] False
bubblesortBy' cmp (x1:x2:xs) ys swap 
  | (cmp x1 x2) == GT = bubblesortBy' cmp (x1:xs) (x2:ys) True
  | otherwise         = bubblesortBy' cmp (x2:xs) (x1:ys) swap

------------------------------------------------------------------------------
-- Shell Sort
------------------------------------------------------------------------------
{-shellsort :: Ord a => SimpleSorter a -}
{-shellsort = shellsortBy compare-}

{-shellsortBy :: Ord a => Sorter a-}
{-shellsortBy _   [] = []-}

------------------------------------------------------------------------------
-- Comb Sort
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Bogo Sort
------------------------------------------------------------------------------
-- bogosort :: Ord a => [a] -> IO [a]
-- bogosort = bogosortBy compare
-- 
-- bogosortBy :: Ord a => Comparator a
-- bogosortBy cmp xs | sorted cmp xs = xs
--                   | otherwise     = bogosortBy cmp (shuffle xs)
-- 
-- sorted :: Ord a => Comparator a -> [a] -> Bool
-- sorted _   []         = True
-- sorted cmp (x1:x2:xs) | (cmp x1 x2) /= GT = sorted cmp (x2:xs)
--                       | otherwise         = False
-- 
-- {-shuffle :: [a] -> [a]-}
-- shuffle xs = do 
--   gen <- getStdGen
--   return $ randPerm gen xs
-- 
-- -- http://stackoverflow.com/questions/9877969/haskell-functions-to-randomly-order-a-list-not-working-properly-homework-begin
-- randPerm :: StdGen -> [a] -> [a]
-- randPerm _ []   = []
-- randPerm gen xs = let (n,newGen) = randomR (0,length xs -1) gen
--                       front = xs !! n
--                   in  front : randPerm newGen (take n xs ++ drop (n+1) xs)

------------------------------------------------------------------------------
-- Stooge Sort
------------------------------------------------------------------------------
stoogesort :: Ord a => SimpleSorter a
stoogesort = stoogesortBy compare

stoogesortBy :: Ord a => Sorter a
stoogesortBy _   []  = []
stoogesortBy _   [x] = [x]
stoogesortBy cmp xs  = (s . s' . s) (stoogeFlip cmp xs)
  where 
    s  = stooge 2 cmp
    s' = stooge 1 cmp

stooge :: Ord a => Int -> Sorter a
stooge i cmp xs = (stoogesortBy cmp sort) ++ leave
  where 
    (sort, leave) = splitAt sortL xs
    sortL         = (length xs) * i `div` 3

stoogeFlip :: Ord a => Sorter a
stoogeFlip cmp xs 
  | (cmp h l) == LT = xs
  | otherwise       = (l:m++[h])
  where
    h  = head xs
    m  = (init . tail) xs
    l  = last xs
