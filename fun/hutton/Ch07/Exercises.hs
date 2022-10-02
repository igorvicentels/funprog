-- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and filter. 
mapfilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter f p xs = map f (filter p xs)


-- 2. Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.
    
-- a. Decide if all elements of a list satisfy a predicate:
all' :: (a -> Bool) -> [a] -> Bool
-- all' _ []     = True 
-- all' p (x:xs) = p x && all' p xs 
all' p = foldr (\x acc -> p x && acc) True    

-- b. Decide if any element of a list satisfies a predicate:
any' :: (a -> Bool) -> [a] -> Bool
-- any' _ []     = False
-- any' p (x:xs) = p x || any' p xs
any' p = foldr (\x acc -> p x || acc) False

-- c. Select elements from a list while they satisfy a predicate:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p []     = []
takeWhile' p (x:xs) = 
    if p x 
        then x : takeWhile' p xs
        else []

-- d. Remove elements from a list while they satisfy a predicate:
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropwhile' p [] = []
dropWhile' p (x:xs) = 
    if p x 
        then dropWhile' p xs
        else x:xs



-- 3. Redefine the functions map f and filter p using foldr.
map' f = foldr (\x acc -> f x : acc) []

filter' p = foldr (\x acc -> if p x then x : acc else acc) []



-- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer. For example:
-- > dec2int [2,3,4,5]
-- 2345
dec2int :: [Int] -> Int
dec2int = foldr (\x acc -> x + 10 * acc) 0 . reverse




-- 5. Without looking at the definitions from the standard prelude, define the higher-order library function curry that converts a function on pairs into a curried function, and, conversely, the function uncurry that converts a curried function with two arguments into a function on pairs.
-- Hint: first write down the types of the two functions.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x,y) = f x y



-- 6. A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)
-- That is, the function unfold p h t produces the empty list if the predicate p is true of the argument value, and otherwise produces a non-empty list by applying the function h to this value to give the head, and the function t to generate another argument that is recursively processed in the same way to produce the tail of the list. For example, the function int2bin can be rewritten more compactly using unfold as follows:
int2bin = unfold (== 0) (`mod` 2) (`div` 2)
-- Redefine the functions chop8, map f and iterate f using unfold
chop8 :: [Int] -> [[Int]]
chop8 = unfold (== []) (take 8) (drop 8)

map'' :: Eq a => (a -> b) -> [a] -> [b]
map'' f = unfold (== []) (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\x -> False) id f



-- 7. Modify the binary string transmitter example to detect simple transmission errors using the concept of parity bits. That is, each eight-bit binary number produced during encoding is extended with a parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn, each resulting nine-bit binary number consumed during decoding is checked to ensure that its parity bit is correct, with the parity bit being discarded if this is the case, and a parity error being reported otherwise.
-- Hint: the library function error :: String -> a displays the given string as an error message and terminates the program; the polymorphic result type ensures that error can be used in any context.

parity :: [Int] -> [Int]
parity xs = if even (sum xs) then xs ++ [0] else xs ++ [1]

unparity :: [Int] -> [Int]
unparity xs = if even (sum xs) then init xs else error "parity error"



-- 8. Test your new string transmitter program from the previous exercise using a faulty communication channel that forgets the first bit, which can be modelled using the tail function on lists of bits.

-- Answer in BinaryStringTransmitter.hs



-- 9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies its two argument functions to successive elements in a list, in turn about order. For example:
-- > altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []        = []
altMap f g [x]       = [f x]
altMap f g (x:x':xs) = f x : g x' : altMap f g xs 



-- 10. Using altMap, define a function luhn :: [Int] -> Bool that implements the Luhn algorithm from the exercises in chapter 4 for bank card numbers of any length. Test your new function using your own bank card.

luhnDouble :: Int -> Int
luhnDouble x = if x * 2 > 10 then x * 2 - 9 else x * 2

luhn :: [Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0