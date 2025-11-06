find :: Int -> [(Int,String)] -> String
find _ [] = ""
find n ((i,str):xs)
  | n == i    = str
  | otherwise = find n xs

main = do
    let pairs = [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e")]
    print (find 3 pairs)
