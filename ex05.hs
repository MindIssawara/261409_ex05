rev' :: [a] -> [a]
rev_aux :: [a] -> [a] -> [a]
rev' l = rev_aux l []
rev_aux []     res = res
rev_aux (x:xs) res = rev_aux xs (x:res)

--ex05 list mapping
list_map :: (t -> a) -> [t] -> [a] -- the type of list_map
list_map _ [] = []
list_map func (x:xs) = func x : list_map func xs

-- can you use tail recursion?
list_map' :: (a -> b) -> [a] -> [b]
list_map' func xs = rev' (lm_aux xs [])
  where
    lm_aux [] result = result
    lm_aux (x:xs) result = lm_aux xs (func x : result)

--write three more test cases for list_map
--      list_map (*2) [1, 2, 3, 4, 5]
--          [2, 4, 6, 8, 10]
--      list_map (tail) ["hello", "world", "haskell"]           
--          ["ello","orld","askell"]
--      list_map (tail) []           
--          []


-- rewrite zipper using tail recursion
zipper' :: [a] -> [b] -> [(a, b)]
zipper' xs ys = rev'(zip_aux xs ys [])
    where
        zip_aux [] _ result =  result
        zip_aux _ [] result = result
        zip_aux (x:xs) (y:ys) result = zip_aux xs ys ((x, y) : result)

