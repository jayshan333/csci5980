import Data.Monoid


data CatList a = Nil
               | Wrap a
               | Cat (CatList a) (CatList a)

collapse :: Monoid a => CatList a -> a
collapse Nil = mempty
collapse (Wrap a) = a
collapse (Cat a b) = mappend (collapse a) (collapse b)

t1 = (Cat (Cat (Wrap (Product 4)) (Wrap (Product 2)))(Wrap (Product 3)))

test1 = getProduct (collapse (Cat (Cat (Wrap (Product 4)) (Wrap (Product 2)))(Wrap (Product 3)))) == 24

catHazSum :: Num a => CatList a -> a
catHazSum Nil = 0
catHazSum (Wrap a) = a
catHazSum (Cat a b) = (+) (catHazSum a) (catHazSum b)

test2 = catHazSum (Cat (Cat (Wrap 4) (Wrap 4)) (Wrap 3)) == 11