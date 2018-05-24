module Hangman where

import System.IO

done = return ()

write ::  String -> IO ()
write [] = done
write (x:xs) = putChar x >> write xs

writeln ::  String -> IO ()
writeln s = write s >> putChar '\n'

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

getCh2 :: IO Char
getCh2  = do 
            c <- getChar
            return c

--readn :: Int -> IO String 
--readn 0 = return [] 
--readn n = getChar >>= q
--              where q c = readn (n-1) >>= r
--                          where r cs = return (c : cs)

readln = getCh >>= q
   where q c = if c == '\n'
               then return ""
               else readln >>= r
                    where r cs =
                           return (c:cs)


readln2 = getCh2 >>= q
   where q c = if c == '\n'
               then return ""
               else readln2 >>= r
                    where r cs =
                           return (c:cs)

checker x list =
  case list of
  [] -> False
  y:ys -> if y == x then True else checker x ys

-- do I need to check for lower case?
checkWord guess word =
  guess == word

display :: [Char] -> [Char] ->  [Char] -- needs to be IO ()
display word guessLetters =
  [if checker c guessLetters then c else '_' | c <- word]

combine :: [a] -> [a] -> [a]
combine [] list = list
combine (x:xs) list = x : combine xs list

unknown = undefined

play :: [Char] -> [Char] -> IO ()
play word guessLetters = do
                    putStrLn (display word guessLetters)
                    putStrLn "Guess the word"
                    guessWord <- readln2
                    (if checkWord guessWord word then putStrLn "You guessed the word!"
                                else play word (combine guessWord guessLetters))




hangman = do
          putStrLn "Enter a word to guess"
          theWord <- readln --string
          play theWord []
          putStrLn "Thanks for playing!"

--hangman = do write "Think of a word: "
--                cs <- readln
                

--hangmanStep xs = 

