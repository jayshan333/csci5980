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


test1 = eval_e (Try (Div (Con 6) (Con 3)) (Con 42))
test2 = eval_e (Try (Div (Con 1) (Con 0)) (Con 42))


-- same as before
instance (Show a) => Show (Exc a) where
  show (Raise e)  = "exception: " ++ e
  show (Return x) = "value: " ++ show x