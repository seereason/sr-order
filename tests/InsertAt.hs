

import Data.Order
import Data.List


-- To implement insertAt, we need to first add the element to the
-- Order, which will append it.
-- Then we need to permute the order list to what was last is now at position k.


insertAt :: (Enum k, Ord k) => Int -> v -> Order k v -> Order k v
insertAt n v o = move z n o'
  where z = length (order o') - 1
        o' = appendItem v o

move :: (Enum i, Enum j, Ord k) => i -> j -> Order k v -> Order k v
move i j o = o { order = neworder }
  where neworder = insertAtList (fromEnum j) rs cutlist
        (cutlist, rs) = removeAt (fromEnum i) (order o)

sample :: Order Int String
sample = foldr (appendItem . show)  mempty (reverse [1,2,3])

test = insertAt 0 "0" sample

split3 i xs = (as, bs, cs)
  where (as,bs') = splitAt i xs
        (bs,cs) = splitAt 1 bs'

remove i xs = as ++ cs
  where (as,bs,cs) = split3 i xs

removeAt i xs = (as ++ cs, bs)
  where (as,bs,cs) = split3 i xs

insertAtList i vs xs = as ++ vs ++ bs
  where (as,bs) = splitAt i xs

shw :: (Show k, Show v) => Order k v -> String
shw (Order es os n) = "Order { elems = " ++ show es ++ ", order = " ++ show os ++ ", next = " ++ show n ++ "}"


