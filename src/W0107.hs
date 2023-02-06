module W0107 where

import Control.Monad.State


data GP a = End a
          | Get (Int -> GP a)
          | Put Int (GP a)

echo :: GP a
echo = Get (\n -> Put n echo)

run :: GP a -> IO a
run (End a) = return a
run (Get f) = putStr "? " >> (readLn :: IO Int) >>= run . f
run (Put a gp) = putStrLn (show a) >> run gp

add :: GP ()
add = Get $ \n1 ->
    Get $ \n2 ->
        Put (n1 + n2) (End ())

add' :: GP Int
add' = Get $ \n1 ->
    Get $ \n2 ->
        Put (n1 + n2) (End (n1 + n2))

startAccum :: Int -> GP Int
startAccum t =
    Get $ \n ->
        if n == 0
            then End t
            else Put (t+n) $ startAccum (t+n)

accum :: GP Int
accum = start 0
    where 
        start :: Int -> GP Int
        start t = 
            Get $ \n ->
                if n == 0
                    then End t
                    else Put (t+n) $ start (t+n)

simulate :: GP a -> [Int] -> (a, [Int])
simulate (End a) _ = (a, [])
simulate (Get _) [] = error "insufficient input"
simulate (Put o gp) xs = 
    let (x, y) = simulate gp xs
    in (x, o:y)
simulate (Get f) (x:xs) = simulate (f x) xs

simulateS :: GP a -> State [Int] (a, [Int])
simulateS (End a) = return (a, [])
simulateS (Get f) =
    do
        ys <- get
        case ys of
            [] -> error "End of input"
            (x:xs) ->
                do 
                    put xs
                    simulateS $ f x
simulateS (Put y m) = 
    do
        (z, ys) <- simulateS m
        return (z, y:ys)

simulate' :: GP a -> [Int] -> (a, [Int])
simulate' m xs = evalState (simulateS m) xs

instance Functor GP where
    fmap f (End a) = End (f a)
    fmap f (Get gf) = Get ((fmap f) . gf)
    fmap f (Put o gp) = Put o (fmap f gp)

instance Applicative GP where

    pure = End

    (End f) <*> (End a) = End (f a)
    (End f) <*> (Get gf) = Get ((fmap f).gf)
    (End f) <*> (Put o gp) = Put o (fmap f gp)
    (Get gf) <*> gp = Get $ \n -> (gf n) <*> gp
    (Put o gf) <*> gp = Put o (gf <*> gp)

instance Monad GP where

    return = End

    (End a) >>= f = f a
    (Get gf) >>= f = Get $ \n -> (gf n) >>= f
    (Put o gp) >>= f = Put o $ gp >>= f

-- Unable to come up with any function implementation using (>>=). Checked solution for it.