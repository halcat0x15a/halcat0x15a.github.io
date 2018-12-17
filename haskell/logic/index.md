---
layout: article
title: LogicT
---

# LogicT

[Backtracking, Interleaving, and Terminating Monad Transformers](http://okmij.org/ftp/Computation/LogicT.pdf)を紹介します。

## Abstract

任意のモナドでバックトラッキングを可能にするモナドトランスフォーマーを設計、実装します。

このライブラリは次のような操作を提供します。

* 公平な選言: `interleave`
* 公平な連言: `>>-`
* 条件分岐: `ifte`
* 枝刈り: `once`

これらは`msplit`という一つプリミティブで実装することができます。

また、次の2つの実装を紹介します。

* 成功と失敗の継続を使ったもの
* 限定継続を使ったもの

## 1. Introduction

モナドは計算の観念を一般化します。

モナドプラスには実践的な応用があります。

* 非決定的な計算
* トランザクション
* パターンコンビネータ
* エラーハンドリング

[Hinzeのバックトラッキングモナドトランスフォーマー](https://wiki.ittc.ku.edu/lambda/images/9/9a/Hinze_-_Deriving_Backtracking_Monad_Transformers.pdf)は非決定的選択とProlog-likeなcutをサポートします。

これを含めた多くのバックトラッキングモナドトランスフォーマーには3つの欠点があります。

一番目は不公平による終わらない解の探索です。これは公平な選言と連言を実装することで解決されます。

二番目は`cut`を実装することによる否定と枝刈りの混同です。これはソフトカットとコミットチョイスに分離することで解決されます。

三番目は最終的な結果を得る機能が限定的であることです。これは表現力のある実行インターフェースを提供することで解決されます。

ストリームベースの実装に加えてCPSとControl-channelの二つの実装を示します。

CPSに基づく実装は成功と失敗の継続を使います。パターンマッチは行われず継続が実行されるだけです。このため、決定的なコードは高速に実行されます。

Control-channelに基づく実装は限定継続を使います。これはより洗礼された探索戦略で拡張可能です。

これら二つの実装に対して`msplit`を定義します。

```haskell
msplit :: (Monad m, LogicT t, MonadPlus (t m)) => t m a -> t m (Maybe (a, (t m a)))
```

`msplit`は最初の解を計算し残りの計算を停止します。

`m`は任意の作用を提供するモナドで、`t`はここで設計し実装されるモナド変換子です。

## 2. Basic Backtracking Computation: MonadPlus

`mplus`は選言を、`mzero`は失敗を意味します。

```haskell
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
```

モナドプラスは次のような法則を満たすべきです。

```haskell
-- 定義 2.1
mplus a mzero === a
mplus mzero a === a
mplus a (mplus b c) === mplus (mplus a b) c
mzero >>= k === mzero
(mplus a b) >>= k === mplus (a >>= k) (b >>= k)
```

連言`>>=`はゴールを左から右に評価します。

```haskell
t1, t2, t3 :: MonadPlus m => m Int
t1 = mzero
t2 = return 10 `mplus` return 20 `mplus` return 30
t3 = msum (map return [10, 20, 30])
```

`t1`は失敗を表現し、`t2`と`t3`は同一で3つの選択肢を表現します。

リストモナドはモナドプラスの適切な実装です。

* 空のリストは失敗を意味します
* 単一要素のリストは決定的な計算を意味します
* 複数要素のリストは複数の結果を意味します

複数の選択により生成された解はストリームに変換可能です。

```haskell
runList :: [a] -> [a]
runList = id
```

リストモナドは決定的計算でオーバーヘッドがあります。

また、次のプログラムは全ての解の列挙に`n^2`時間掛かります。

```haskell
( ... (return 1 `mplus` return 2) `mplus` ... return n)
```

効率的な実装には二継続モデルと限定継続を使います。

コンビネータを定義した後個々の実装に立ち戻ります。

## 3. A More Expressive Interface

4つのコンビネータでモナドプラスを拡張します。

最初に`runList`をリスト以外のモナドへ一般化します。

### 3.1 Running Computations

さしあたり次ような関数`runL`を考えます。

```haskell
runL :: Maybe Int -> L a -> [a]
```

`L`はバックトラッキングモナドです。

引数が`Nothing`のときすべての解が生成されます。

引数が`Just n`のとき多くとも`n`だけ解が生成されます。

### 3.2 Interleaving (Fair Disjunction)

プログラムは潜在的に無限の非決定的選択を作ります。

```haskell
odds :: MonadPlus m => m Int
odds = (return 1) `mplus` (odds >>= \a -> return (2 + a))
```

`runL (Just 5) odds`は`[1,3,5,7,9]`を返します。

`odds`は単純には他の計算と合成できません。

```haskell
runL (Just 1) (do x <- odds `mplus` t3
                  if even x then return x else mzero)
```

```odds `mplus` t3```は`t3`を決して考慮しません。

しかし、10と20と30は解として返すことが可能です。

そこで新しいプリミティブ`interleave`があると便利です。

```haskell
runL (Just 10) (odds `interleave` t3)
```

これは`[1,10,3,20,5,30,7,9,11,13]`を返します。

`interleave`は次の例を可能にします。

```haskell
runL (Just 1) (do x <- odds `interleave` t3
                  if even x then return x else mzero)
```

これは答え`10`を返します。

### 3.3 Fair Conjunction

不公平な選言は不公平な連言を引き起こします。

```haskell
let oddsPlus n = odds >>= \a -> return (a + n)
in runL (Just 1)
        (do x <- (return 0 `mplus` return 1) >>= oddsPlus
            if even x then return x else mzero)
```

`return 1 >>= oddsPlus`があるにも関わらず発散します。

公平な`mplus`に加えて公平な`>>=`が必要です。

`>>-`を使うことで次のプログラムは`2`を生成します。

```haskell
let oddsPlus n = odds >>= \a -> return (a + n)
in runL (Just 1)
        (do x <- (return 0 `mplus` return 1) >>- oddsPlus
            if even x then return else mzero)
```

### 3.4 Laws of interleave and `>>-`

`interleave`と`>>-`は`mplus`と`>>=`のアナロジーです。

非決定性計算は`mzero`か```return a `mplus` m```で表現できます。

```haskell
-- 定義 3.1
interleave mzero m === m
interleave (return a `mplus` m1) m2 === return a `mplus` (interleave m2 m1)
mzero >>- k === mzero
(mplus (return a) m) >>- k === interleave (k a) (m >>- k)
```

`interleave`と`>>-`の主な用途は発散を回避することです。

解の順序を考慮しなければ`mplus`と`>>=`に等しいです。

### 3.5 Soft cut (Conditional)

条件分岐は他の計算に依存する計算を表現できます。

次の例は他の数値で割り切れる奇数を生成します。

```haskell
iota n = msum (map return [1..n])
test_oc = runL (Just 10)
               (do n <- odds
                   guard (n > 1)
                   d <- iota (n - 1)
                   guard (d > 1 && n `mod` d == 0)
                   return n)
```

この結果は`[9,15,15,21,21,25,27,27,33,33]`です。

奇素数の生成には"有限失敗による否定"が必要です。

`ifte t th el`は論理条件分岐演算です。

1. 計算`t`を実行します
2. 一つでも結果があれば全体は`t >>= th`と等価です
3. さもなければ全体の計算は`el`と等価です

これは次の法則が得られます。

```haskell
-- 定義 3.2
ifte (return a) th el === th a
ifte mzero th el === el
ifte (return a `mplus` m) th el === th a `mplus` (m >>= th)
```

`ifte`は"失敗の説明"に便利で、静かな失敗を回避します。

`ifte`を使って奇素数を生成することができます。

```haskell
-- 例 3.1
test_op = runL (Just 10)
               (do n <- odds
                   guard (n > 1)
                   ifte (do d <- iota (n - 1)
                            guard (d > 1 && n `mod` d == 0))
                        (const mzero)
                        (return n))
```

結果は`[3, 5, 7, 11, 13, 17, 19, 23, 29, 31]`です。

### 3.6 Pruning (Once)

`ifte`はある意味で枝刈りのプリミティブです。

別の重要な枝刈りのプリミティブに`once`があります。

これはただ一つの解を選択します。

"どれでもよい非決定性"の表現に重要です。

`once`のための簡単な例を示します。

```haskell
bogosort l = do p <- permute l
                if sorted p then return p else mzero

sorted (e1:e2:r) = e1 <= e2 && sorted (e2:r)
sorted _ = True

permute [] = return []
permute (h:t) = do { t' <- permute t; insert h t' }

insert e [] = return [e]
insert e l@(h:t) = return (e:l) `mplus` do { t' <- insert e t; return (h:t') }
```

生成し、検査する手法は論理プログラミング特有です。

`bogosort`は要素が重複するとき複数の解を持ちます。

```haskell
runL Nothing (bogosort [5,0,3,4,0,1])
```

これは`[[0,0,1,3,4,5],[0,0,1,3,4,5]]`を生成します。

しかし、明らかに解のどれか一つだけを必要とします。

`bogosort`を次のように変更することで表現できます。

```haskell
bogosort' l = once (do p <- permute l
                       if sorted p then return p else mzero)
```

例3.1では素数の検査で全ての因数を計算します。

任意の因数があればそれ以上探索することは不要です。

```haskell
test_op' = runL (Just 10)
                (do n <- odds
                    guard (n > 1)
                    ifte (once (do d <- iota (n - 1)
                                   guard (d > 1 && n `mod` d == 0)))
                         (const mzero)
                         (return n))
```

`ifte`と`once`で"失敗による否定"が実装できます。

```haskell
gnot :: (Monad m, LogicT t, MonadPlus (t m)) => t m a -> t m ()
gnot m = ifte (once m) (const mzero) (return ())
```

`m`が成功するならば`gnot m`は失敗し、`m`が失敗するならば`gnot m`は成功します。

## 4. Splitting Computations

`interleave`と`>>-`と`ifte`と`once`は`msplit`で実装可能です。

`msplit`でインスタンス化できる型クラスを定義します。

### 4.1 The Monad LogicM

`LogicM`は前のセクションのインターフェースを持ちます。

これは`msplit`を使ったデフォルト実装を含みます。

```haskell
class MonadPlus m => LogicM m where
  msplit :: m a -> m (Maybe (a, m a))
  interleave :: m a -> m a -> m a
  interleave sg1 sg2 =
    do r <- msplit sg1
       case r of
         Nothing -> sg2
         Just (sg11, sg12) -> (return sg11) `mplus` (interleave sg2 sg12)
  (>>-) :: m a -> (a -> m b) -> m b
  sg >>- g =
    do r <- msplit
       case r of
         Nothing -> mzero
         Just (sg1, sg2) -> interleave (g sg1) (sg2 >>- g)
  ifte :: m a -> (a -> m b) -> m b -> m b
  ifte t th el =
    do r <- msplit t
       case r of
         Nothing -> el
         Just (sg1, sg2) -> (th sg1) `mplus` (sg2 >>= th)
  once :: m a -> m a
  once m =
    do r <- msplit m
      case r of
        Nothing -> mzero
        Just (sg1, _) -> return sg1
```

`msplit`は次の二つの法則を持ちます。

```
msplit mzero === return Nothing
msplit (return a `mplus` m) === return (Just (a, m))
```

* 失敗する計算は分割できません
* 成功する計算は結果と残りの計算に分割します

デフォルト実装から公理を確かめることができます。

```haskell
    ifte (return a) th el
=== do r <- msplit (return a)
       case r of
         Nothing -> el
         Just (sg1, sg2) -> (th sg1) `msplit` (sg2 >>= th)
=== do r <- msplit (return a `mplus` mzero)
       case r of
         Nothing -> el
         Just (sg1, sg2) -> (th sg1) `msplit` (sg2 >>= th)
=== do r <- return (Just (a, mzero))
       case r of
         Nothing -> el
         Just (sg1, sg2) -> (th sg1) `msplit` (sg2 >>= th)
=== case Just (a, mzero) of
       Nothing -> el
       Just (sg1, sg2) -> (th sg1) `msplit` (sg2 >>= th)
=== (th a) `mplus` (mzero >>= th)
=== th a
```

### 4.2 Implementing msplit Using Lists

この論文では継続モナドに対して`msplit`を実装します。

リストモナドの場合は`msplit`の実装が素直です。

```haskell
newtype SSG a = Stream [a]
unSSG (Stream str) = str

instance Monad SSG where
  return e = Stream [e]
  (Stream es) >>= f = Stream (concat (map (unSSG . f) es))

instance MonadPlus SSG where
  mzero = Stream []
  (Stream es1) `mplus` (Stream es2) = Stream (es1 ++ es2)

instance LogicM SSG where
  msplit (Stream []) = return Nothing
  msplit (Stream (h:t)) = return (Just (h, Stream t))
```

### 4.3 The Monad Transformer LogicT

任意のモナドにバックトラッキングを加えるため、モナドトランスフォーマーを使います。

```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

`lift`は次の法則を満たします。

```haskell
lift . return === return
lift (m >>= k) === lift m >>= lift . k
```

これは基底のモナド`m`を`t m`に持ち上げます。

`LogicM`に型変数を加えることで`LogicT`に一般化できます。

```haskell
class MonadTrans t => LogicT t where
  msplit :: (Monad m, MonadPlus (t m)) => t m a -> t m (Maybe (a, t m a))
  interleave :: (Monad m, MonadPlus (t m)) => t m a -> t m a -> t m a
  (>>-) :: (Monad m, MonadPlus (t m)) => t m a -> (a -> t m b) -> t m b
  ifte :: (Monad m, MonadPlus (t m)) => t m a -> (a -> t m b) -> t m b -> t m b
  once :: (Monad m, MonadPlus (t m)) => t m a -> t m a
```

`MonadTrans`を継承することで`LogicT`もまた`lift`することができます。

```haskell
lift (putStrLn "text") >> mzero
```

これは副作用を実行した後失敗する計算を表現します。

定義4.1は"持ち上げ"を制御することで一般化されます。

一番目の法則は`lift m >> mzero`を考慮すべきで、二番目の法則は```lift m `mplus` tm1```を考慮すべきです。

`tm1`は型`t m a`を持ち、`msplit`は型`t m (Maybe (a, t m a))`を持ちます。

これらの関係を表すために次の関数を定義します。

```haskell
reflect :: (Monad m, LogicT t, MonadPlus (t m)) => Maybe (a, t m a) -> t m a
reflect r = case r of
  Nothing -> mzero
  Just (a, tmr) -> return a `mplus` tmr

rr tm = msplit tm >>= reflect
```

`rr tm`は`tm`と同じ型を持ち、`reflect`はモナドの計算値に作用しません。

一般化された法則を示します。

```haskell
-- 定義 4.2
rr (lift m >> mzero) === lift m >> mzero
rr (lift m `mplus` tma) === lift m `mplus` (rr tma)
```

##  5. Implementations of LogicT

ここでは`LogicT`の実装に注目し、継続とコントロールオペレータの2つの実装を与えます。

### 5.1 CPS Implementation

CPS実装は成功と失敗の継続を持つ`SFKT`を導入します。

```haskell
newtype SFKT m a = SFKT (forall ans. SK (m ans) a -> FK (m ans) -> m ans)
unSFKT (SFKT a) = a

type FK ans = ans
type SK ans a = a -> FK ans -> ans
```

`SFKT m`は`Monad`かつ`MonadPlus`で、`SFKT`はモナドトランスフォーマーです。

```haskell
instance Monad m => Monad (SFKT m) where
  return e = SFKT (\sk fk -> sk e fk)
  m >>= f = SFKT (\sk -> unSFKT m (\a -> unSFKT (f a) sk))

instance Monad m => MonadPlus (SFKT m) where
  mzero = SFKT (\_ fk -> fk)
  m1 `mplus` m2 = SFKT (\sk fk -> unSFKT m1 sk (unSFKT m2 sk fk))

instance MonadTrans SFKT where
  lift m = SFKT (\sk fk -> m >>= (\a -> sk a fk))
```

"コンテキストパッシング"実装は項の解釈を除きます。

継続の抽象表現を維持し`msplit`をサポートします。

```haskell
instance LogicT SFKT where
  msplit tma = lift (unSFKT tma ssk (return Nothing))
    where ssk a fk = return (Just (a, (lift fk >>= reflect)))
```

成功と失敗の継続を与えることで`tma`を分割します。

1. 失敗したら`lift (return Nothing)`を返します
2. 成功したら`a`と停止状態を返します

### 5.2 Implementation Using Delimited Control

コントロールオペレータ実装は[限定継続](http://www.cs.indiana.edu/~dyb/pubs/monadicDC.pdf)を利用します。

これは限定継続計算`CC`とデリミタ`Prompt`を提供します。

最初に`CC`をモナドトランスフォーマーに拡張し、次のコントロールオペレータを定義します。

```haskell
promptP :: Monad m => (Prompt r a -> CC r m a) -> CC r m a
abortP :: Monad m => Prompt r a -> CC r m a -> CC r m b
shiftP :: Monad m => Prompt r b -> ((CC r m a -> CC r m b) -> CC r m b) -> CC r m a
```

* `promptP`はデリミタを作成します
* `abortP`は実行を中断します
* `shiftP`は継続を保存します

`Prompt`と`CC`のcontrol regionは`r`でパラメタ化されます。

型変数`r`は次の関数を使ってカプセル化されます。

```haskell
runCC :: Monad m => (forall r. CC r m a) -> m a
```

```haskell
runIdentity (runCC (promptP $ \p ->
                    do x <- shiftP p (\k -> k (k (return 2)))
                       return (x + 1)))
```

上の式は次のように進行します。

1. プロンプトを作りスタックにプッシュします
2. コンテキスト`do{ }`をスタックにプッシュします
3. `shiftP`は`p`までの継続を保存します
4. 継続に`2`を二回適用します

`CC`を使って`LogicT`のインスタンス`SR`を定義します。

```haskell
newtype SR r m a = SR (forall ans. ReaderT (Prompt r (Tree r m ans)) (CC r m) a)
unSR (SR f) = f

data Tree r m a = HZero | HOne a | HChoice a (CC r m (Tree r m a))
```

計算は`ReaderT`の環境で実行されます。

次の型を持つ`ask`関数で環境にアクセスできます。

```haskell
ReaderT (Prompt r (Tree r m ans)) (CC r m) (Prompt r (Tree r (Tree r m ans)))
```

`SR r m`は二継続モデルのダイレクトスタイル版です。

成功継続は値だけでなく継続を取り実行します。

* 成功継続は計算のスタックで表現されます
* 非決定的選択は分岐で継続を使うことで表現されます
* 失敗は現在の限定継続を中断することで表現されます

二継続モデルでは決定的計算の特別な表現はありません。

このモデルでは`Tree r m a`で決定的な結果を区別します。

* 決定的な結果は`HOne`を付けます
* 失敗は`HZero`により表現されます
* 選択は`HChoice a r`により表現されます

`SR r m`の`Monad`のインスタンスを示します。

```haskell
instance Monad m => Monad (SR r m) where
  return e = SR (return e)
  (SR m) >>= f = SR (m >>= (unSR . f))
```

決定的計算は基底モナド`m`を"普通に"実行します。

`SR r m`の`MonadPlus`のインスタンスを示します。

```haskell
instance Monad m => MonadPlus (SR r m) where
  mzero =
    SR (ask >>= \pr -> lift (abortP pr (return HZero)))
  m1 `mplus` m2 =
    SR (ask >>= \pr ->
        lift $ shiftP pr $ \sk ->
        do f1 <- sk (runReaderT (unSR m1) pr)
           let f2 = sk (runReaderT (unSR m2) pr)
           compose_trees f1 f2)

compose_trees HZero r = r
compose_trees (HOne a) r = return $ HChoice a r
compose_trees (HChoice a r') r =
  return $ HChoice a $ r' >>= (\v -> compose_trees v r)
```

`mzero`は結果`HZero`でデリミタを中断します。

`mplus`はより複雑です。

1. デリミタまでの継続`sk`を保存します
2. 選択肢`m1`は継続で実行されます
3. 選択肢`m2`は一時停止されます
4. `compose_trees`は`Tree`を構築します
    - 最初の結果と停止状態を結合します

`msplit`に必要な関数を検討します。

`reify`はデータ型でバックトラッキングを表現します。

```haskell
reify :: Monad m => SR r m a -> CC r m (Tree r m a)
reify m = promptP (\pr -> runReaderT (unSR m) pr >>= (return . HOne))
```

* `m`が決定的に実行されるなら`HOne a`を得ます
* `m`が失敗するなら`HZero`を得ます

次の例は計算`m`が`mplus m1 m2`の場合です。

```haskell
reify (m0 >>= (\x -> k1 x `mplus` k2 x) >>= k3)
```

この例は次に等しいです。

```haskell
do HOne x <- reify m0
   f1val <- reify (k1 x >>= k3)
   let f2comp = reify (k2 x >>= k3)
   compose_trees f1val f2comp
```

ここで`m0`は決定的計算です。

`f1val`は一番目の選択肢の結果です。

`f2comp`は二番目の選択肢に対応する計算です。

* `f1val`が`HOne a`なら`HChoice a f2comp`を結果とします
* `f1val`が`HChoice a r'`なら`r'`と`f2comp`を合成します

`compose_trees`は選択点の制御を柔軟にします。

* 例えば`f1val`が`HZero`であると`f2comp`を実行しない
* `f2comp`に`Incomplete`を付けて返す
    * `f2comp`を実行する
    * 他の選択肢を試す

```haskell
(odds >> mzero) `mplus` m
```

で発散を回避できます。

`SR r`に`msplit`を実装します。

```haskell
instance MonadTrans (SR r) where
  lift m = SR (lift (lift m))

instance LogicT (SR r) where
  msplit m = SR (lift (reify m >>= (return . reflect_sr)))
    where reflect_sr HZero = Nothing
          reflect_sr (HOne a) = Just (a, mzero)
          reflect_sr (HChoice a r1) =
            Just (a, SR (lift r1) >>= (return . reflect_sr) >>= reflect)
```

ダイレクトスタイルのための`LogicT`を実装しました。

`SR r m a`は先進的なシステムの実装に適しています。

## 6. Running Computations

解のリストを結果とする`runL`を実装します。

単純な方法は`SFKT`などに`solve`関数を定義することです。

```haskell
solve :: (Monad m) => SFKT m a -> m [a]
solve (SFKT m) = m (\a fk -> fk >>= (return . (a:))) (return [])
```

しかし、この関数では十分ではありません。

これは`m`が非正格なときだけ動作します。

`m`が正格なとき`SFKT m a`の全ての解が集められます。

無限の解を持つ`SFKT IO a`への`solve`の適用は発散します。

`runL`は観測したい数だけの答えを返し、それ以上のバックトラッキングは行われません。

従って、`runL`は正格なモナドの上でも計算を安全に扱えます。

`SFKT`を実行するとき継続を通す必要があります。

次の関数は最初の解を返した後に継続を無視します。

```haskell
observe :: Monad m => SFKT m a -> m a
observe (SFKT m) = m (\a fk -> return a) (fail "no anser")
```

`fk`を実行することで`solve`のように全ての解を得ます。

しかし、一定の回数だけ`fk`を実行する方法が見えません。

`msplit`を使うことで`runL`を実装します。

これはより一般的な操作`bagofN`と`unfold`を実装することが可能です。

`bagofN`はバックトラッキングの実装に依存しません。

Prologの`bagof`はリストにゴールの全ての解を集めますが、`bagofN`は望まれる数だけの解を得ます。

```haskell
bagofN :: (Monad m, LogicT t, MonadPlus (t m)) => Maybe Int -> t m a -> t m [a]
bagofN (Just n) _ | n <= 0 = return []
bagofN n m = msplit m >>= bagof N'
  where bagofN' Nothing = return []
        bagofN' (Just (a, m')) = bagofN (fmap pred n) m' >>= (return . (a:))
```

最初の引数が`Just n`なら多くとも`n`だけ解を選択します。

`bagofN`の結果は`t m [a]`で、`m`に戻すには`observe`が必要です。

観測関数には明確なバックトラッキング実装が必要です。

SRモナドでは次のようになります。

```haskell
observe_sr :: Monad m => (forall r. SR r m a) -> m a
observe_sr m = runCC (reify m >>= pick1)
  where pick1 HZero = fail "no answer"
        pick1 (HOne a) = return a
        pick1 (HChoice a r) = return a
```

計算`m`を`Tree r m a`にreifyし最初の解を取り出します。

`observe`により`runL`関数を書くことができます。

```haskell
type L a = forall r. SR r Identity a
runL :: Maybe Int -> L a -> [a]
runL n m = runIdentity (observe_sr (bagofN n m))
```

恒等モナド上の`SR r`はバックトラッキングモナドLです。

`SFKT`でも実行可能です。

例3.1を変更します。

```haskell
test_opio = print =<< observe (bagofN (Just 10))
              do n <- odds
                 guard (n > 1)
                 ifte (do d <- iota (n - 1)
                          guard (d > 1 && n `mod` d == 0)
                          liftIO (print d))
                      (const mzero)
                      (return n)
```

`IO`モナドは中間結果をプリントします。

このアプローチは`Debug.Trace`を使うよりロバストです。

例3.1と比較しても大部分は同じコードです。

## 9. Conclusion

バックトラッキングモナドの次の機能を紹介しました。

* 公平な連言と選言
* 論理的な条件分岐と枝刈り
* 任意個の解の選択

次のトランスフォーマーの実装を説明しました。

* ２継続のCPS
* 限定継続ライブラリを元にしたダイレクトスタイル

`msplit`でバックトラッキングオペレータが実装されます。

これはストリームとしてトランスフォーマーを扱えます。

ダイレクトスタイルを使うことで、洗礼されたバックトラッキングを実装できます。
