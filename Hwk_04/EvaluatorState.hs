import Control.Monad.Writer
import Control.Monad.State

data Term = Con Int
          | Div Term Term
          | Count
     deriving (Eq, Show)


eval_s :: Term -> State Int Int
eval_s (Con x) = do 
                  return x
eval_s (Div t u) =  do 
                    x <- eval_s t 
                    y <- eval_s u
                    count <- get
                    put (count + 1)
                    return (x `div` y)
eval_s (Count) = get

run :: Term -> (Int, Int)
run termer = runState (eval_s termer) 0

test1 = run (Div (Con 6) (Con 3))
test2 = run (Div (Div (Div (Con 6) (Con 3)) (Con 1)) (Con 1)) 
test3 = run (Div (Div (Div (Con 6) (Con 3)) Count) Count) 