import Control.Monad.Writer

data Term = Con Int
          | Div Term Term
          | Count
     deriving (Eq, Show)


-- State - counting divisions.
----------------------------------------

-- the same definitions as before.
newtype St a = MkSt (State -> (a,State))
type State = Int

apply :: St a -> State -> (a,State)
apply (MkSt f) s = f s

instance Show a => Show (St a) where
  show f = "value: " ++ show x ++ ", count: " ++ show s
           where (x,s) = apply f 0

-- The monad
instance Monad St where
  return x = MkSt f  where f s = (x, s)
  p >>= q  = MkSt f
             where
              f s = apply (q x) s'
                    where (x,s') = apply p s

instance Functor St where
    fmap f (MkSt st) = MkSt st'
        where
        st' s = (f a, s')
            where (a, s') = st s

instance Applicative St where
    pure x = MkSt (\s -> (x, s))
    MkSt stf <*> MkSt sta = error "Incomplete"
                 
-- Another monad specific function
tick = MkSt f where f s = ((), s+1)

count = MkSt f where f s = (s, s)

eval_s :: Term -> St Int
eval_s (Con x) = return x
eval_s (Div t u)
 = do x <- eval_s t
      y <- eval_s u
      tick
      return (x `div` y)
eval_s (Count) = count




test1 = eval_s (Div (Con 6) (Con 3))
test2 = eval_s (Div (Div (Div (Con 6) (Con 3)) (Con 1)) (Con 1)) 
test3 = eval_s (Div (Div (Div (Con 6) (Con 3)) Count) Count) 



--data Term = Con Int
--          | Div Term Term
--          | Count
--     deriving (Eq, Show)

--newtype St a = MkSt (State -> (a,State))
--type State = Int

--apply :: St a -> State -> (a,State)
--apply (MkSt f) s = f s

--instance Show a => Show (St a) where
--  show f = "value: " ++ show x ++ ", count: " ++ show s
--           where (x,s) = apply f 0

---- The monad
--instance Monad St where
--  return x = MkSt f  where f s = (x, s)
--  p >>= q  = MkSt f
--             where
--              f s = apply (q x) s'
--                    where (x,s') = apply p s

--instance Functor St where
--    fmap f (MkSt st) = MkSt st'
--        where
--        st' s = (f a, s')
--            where (a, s') = st s

--instance Applicative St where
--    pure x = MkSt (\s -> (x, s))
--    MkSt stf <*> MkSt sta = error "Incomplete"

--tick = MkSt f where f s = ((), s+1)

--eval_s :: Term -> St Int
--eval_s (Con x) = return x
--eval_s (Div t u)
-- = do x <- eval_s t
--      y <- eval_s u
--      tick
--      return (x `div` y)
--eval_s Count = return s where (f,s) = apply tick 0