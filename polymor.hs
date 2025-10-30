import Distribution.Simple.Utils (xargs)
identity :: a -> a

identity x = x

main = do
    print(identity 10)
    print(identity "hi")
    print(identity 10.6)
    