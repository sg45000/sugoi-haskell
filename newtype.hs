newtype Pair b a = Pair {getPair :: (a, b)}
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)
-- 型引数が一つの型コンストラクタだけがファンクターのインスタンスになれる
-- newtypeの引数abを逆にすることでタプルの第二引数の型をPair c(cはb)で束縛して、第一引数xはファンクターおける型引数としてfmapの対象にできる
