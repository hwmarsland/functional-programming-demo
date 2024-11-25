-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage = print

-- Write division here
division :: Double -> Double -> Maybe Double
division x y =
    if y == 0
        then Nothing
        else Just (x/y)

-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- Write factList here
factList :: Int -> [Int]
factList n = map factorial [1..n]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do -- Replace this with your testing code
    printAMessage "Hello World!"

    print "Testing division:"
    let z = division 1 2
    let w = division 1 1
    let g = division 6 2
    let h = division 10 0
    print $ "z: " ++ show z ++ ", w: " ++ show w ++ ", g: " ++ show g ++ ", h: " ++ show h

    print "Testing factorial:"
    let a = factorial 1
    let b = factorial 7
    print $ "a: " ++ show a ++ ", b: " ++ show b

    print "Testing factList:"
    let testList = factList 5
    print $ "testList: " ++ show testList

    print "Testing merge"
    let merged = merge [1, 3, 6] [2, 4, 5, 6, 7]
    print $ "merged: " ++ show merged



