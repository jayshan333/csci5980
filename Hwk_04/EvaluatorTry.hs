import Control.Monad.Writer

{- Material here comes from Phil Wadler's paper 
   "Monads for Functional Programming"
   which can be found on Moodle and in the public class repository.
 -}


{- For this import to work, you may need to have cabal, Haskell's
   package manager install the mtl library. To do this, follow these
   steps:
   % cabal upgrade
   % cabal intall mtl
-}





data Term = Con Int
          | Div Term Term
          | Try Term Term
     deriving (Eq, Show)

answer = Div (Div (Con 1972) (Con 2)) (Con 23)
wrong  = Div (Con 2) (Div (Con 1) (Con 0))

-- A monadic evaluator, with do notation
eval :: Monad m => Term -> m Int
eval (Con x) = return x
eval (Div t u) = eval t >>= (\x -> eval u >>= (\y -> return (x `div` y) ) )
                 
-- A monadic evaluator, with do notation
evald :: Monad m => Term -> m Int
evald (Con x) = return x
evald (Div t u) = do x <- evald t
                     y <- evald u
                     return (x `div` y)

eval_maybe :: Term -> Maybe Int
--- eval_maybe = -- eval
eval_maybe (Con x) = return x
eval_maybe (Div t u) = do x <- eval_maybe t
                          y <- eval_maybe u
                          if y == 0
                             then Nothing
                             else return (x `div` y)

--eval_maybe (Div t u) = eval t >>= (\x -> eval u >>= (\y -> return (x `div` y) ) )

-- The type of eval indicates that it takes a term 
-- and performs a "computation" m yielding an integer.


-- First, the identity monad. 
newtype Id a = MkId a
    
instance Monad Id where
  return x        = MkId x
  (MkId x) >>= q  = q x


-- Monads are Applicative Functors, so we need instances
-- for these type classes.
instance Functor Id where
    fmap f (MkId a) = MkId (f a)
instance Applicative Id where
    pure x = MkId x
    MkId f <*> MkId a = MkId (f a)

instance Show a => Show (Id a) where
  show (MkId x) = "value: " ++ show x


eval_id :: Term -> Id Int
eval_id = eval


-- 1. Maybe?




-- Exceptions
----------------------------------------

-- Exc and Exception as before.
data Exc a =  Raise Exception   
           |  Return a
type Exception = String

-- Treat Exc as a monad
instance Monad Exc where
  return x         = Return x
  (Raise e) >>= q  = Raise e
  (Return x) >>= q = q x

instance Functor Exc where
    fmap f (Return a) = Return (f a)
    fmap f (Raise e) = Raise e
instance Applicative Exc where
    pure x = Return x
    Return f <*> Return a = Return (f a)
    Return f <*> Raise e = Raise e
    Raise e <*> Return a = Raise e
    Raise ef <*> Raise ea = Raise ef


-- We define a function specific to this monad.
-- Primarily for stylistic reasons.
raise :: Exception -> Exc a
raise e = Raise e


eval_e :: Term -> Exc Int
eval_e (Con x) = return x
eval_e (Div t u) = do x <- eval_e t
                      y <- eval_e u
                      if y == 0
                             then raise "Division by zero"
                             else return (x `div` y)
eval_e (Try t1 t2) = case (eval_e t1) of
					Raise e -> eval_e t2
					_ -> eval_e t1


t1 = Try (Div (Con 6) (Con 3)) (Con 42)
t2 = Try (Div (Con 1) (Con 0)) (Con 42)



-- same as before
instance (Show a) => Show (Exc a) where
  show (Raise e)  = "exception: " ++ e
  show (Return x) = "value: " ++ show x




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

eval_s :: Term -> St Int
eval_s (Con x) = return x
eval_s (Div t u)
 = do x <- eval_s t
      y <- eval_s u
      tick
      return (x `div` y)








-- Finally, the trace evaluator. 
----------------------------------------

-- definitions from before
newtype Out a = MkOut (Output, a)
type Output = String

line t x = "term: " ++ show t ++ ", " ++
           "yields " ++ show x ++ "\n"

instance Show a => Show (Out a) where
  show (MkOut (ox, x)) = ox ++ "value: " ++ show x

-- the monad
instance Monad Out where
  return x = MkOut ("", x)
  p >>= q  = MkOut (ox ++ oy, y)
             where MkOut (ox, x) = p
                   MkOut (oy, y) = q x


instance Functor Out where
    fmap f (MkOut (s, a)) = MkOut (s, f a)
instance Applicative Out where
    pure x = MkOut ("", x)
    MkOut (sf, f) <*> MkOut (sa, a) = MkOut (sf ++ sa, f a)

-- monad specific functions
out :: Output -> Out ()
out ox = MkOut (ox, ())

eval_o :: Term -> Out Int
eval_o (Con x) = do out (line (Con x) x)
                    return x
eval_o (Div t u)
 = do x <- eval_o t
      y <- eval_o u
      out (line (Div t u) (x `div` y))
      return (x `div` y)


------------------------------------------------------------
-- Versions using Monads from Haskell standard libraries. --
------------------------------------------------------------


-- A version using the IO monad
eval_io :: Term -> IO Int
eval_io (Con x) = do putStrLn (line (Con x) x)
                     return x
eval_io (Div t u)
 = do x <- eval_io t
      y <- eval_io u
      putStrLn (line (Div t u) (x `div` y))
      return (x `div` y)

{- A version using the Writer monad -}
eval_wm :: Term -> Writer String Int
eval_wm (Con x)
  = do tell (line (Con x) x)
       return x

eval_wm (Div t u)
  = do x <- eval_wm t
       y <- eval_wm u
       tell (line (Div t u) (x `div` y))
       return (x `div` y)
      
ew1 = runWriter (eval_wm answer)
ew2 = fst ew1
ew3 = putStr (snd ew1)
evw = putStr $ snd ew1  -- equivalent to ew3


-- A version using Writer and IO.
-- This requires a monad transformer.
-- It prints its operations and counts the number of divisions.

eval_wiom :: Term -> WriterT String IO Int
eval_wiom (Con x)
  = do
    tell "Con"
    lift $ putStrLn (line (Con x) x)
    return x

eval_wiom (Div t u)
  = do x <- eval_wiom t
       y <- eval_wiom u
       lift $ putStrLn (line (Div t u) (x `div` y))
       tell "Div"
       return (x `div` y)

ewiom1 = runWriterT (eval_wiom answer)

