---
layout: default
title: Deriving Lenses with Metaprogramming
---

# Deriving Lenses with Metaprogramming

Java, Scala, Haskellを使用してLens導出を試み, 各言語のメタプログラミングを紹介していきます.

ここで考えるLensは以下のようにgetterとsetterの組であるデータ型とします.

```java
public interface Lens<A, B> {
    public B get(A a);
    public A set(A a, B b);
}
```

```scala
trait Lens[A, B] {
  def get(a: A): B
  def set(a: A, b: B): A
}
```

```haskell
data Lens a b = Lens {
  get :: a -> b,
  set :: a -> b -> a
}
```

## Java: Reflection

Javaのリフレクションを利用してLensを導出します.

以下のようなコンストラクタとgetterを備えるデータ型を対象とします.

```java
public final class Person {
    private final String name;
    private final int age;
    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
    public String getName() {
        return name;
    }
    public int getAge() {
        return age;
    }
}
```

`lens`はクラスとフィールド名を引数にとりLensを返します.

```java
public static String toCamelCase(String name) {
    return Character.toUpperCase(name.charAt(0)) + name.substring(1);
}

public static <A, B> Lens<A, B> lens(Class<A> c, String field) throws NoSuchMethodException {
    Constructor ctor = c.getConstructors()[0];
    Parameter[] params = ctor.getParameters();
    Method method = c.getMethod("get" + toCamelCase(field));
    Method[] methods = new Method[params.length];
    for (int i = 0; i < params.length; i++)
            methods[i] = c.getMethod("get" + toCamelCase(params[i].getName()));
    return new Lens<A, B>() {
        public B get(A a) {
            try {
                return (B) method.invoke(a);
            } catch (IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        }
        public A set(A a, B b) {
            try {
                Object[] args = new Object[methods.length];
                for (int i = 0; i < methods.length; i++)
                    args[i] = methods[i].equals(method) ? b : methods[i].invoke(a);
                return (A) ctor.newInstance(args);
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
                throw new RuntimeException(e);
            }
        }
    };
}
```

`method`はフィールド名から取得したgetterです.

`methods`はコンストラクタのパラメータのgetterの配列です.

`get`メソッドは`method`にデータ型のインスタンスを適用しその値を取得します.

`set`メソッドは`methods`にデータ型のインスタンスを適用し, `method`をフィールドの値で置き換えたものを実引数としてインスタンスを作成します.

`lens`は次のように使えます.

```scala
scala> val age = Lens.lens[Person, Int](classOf, "age")
age: Lens[Person,Int] = Lens$1@4adfd03

scala> val p = new Person("halcat0x15a", 21)
p: Person = Person@2b1e314b

scala> age.get(p)
res0: Int = 21

scala> age.get(age.set(p, 22))
res1: Int = 22
```

この手法には次のような特徴があります.

* 型安全でない
* getとsetがリフレクションの呼び出しで遅い
* Javaのデータ型の定義が難しい

## Scala: Macro

Scalaのマクロを利用してLensを導出します.

以下のようにcase classで定義されたデータ型を対象とします.

```scala
case class Person(name: String, age: Int)
```

`lens`メソッドはデータ型とフィールドの型と名前を引数にとりLensを返します.

```scala
def lens[A, B](field: String): Lens[A, B] = macro lensImpl[A, B]

def lensImpl[A, B](c: Context)(field: c.Expr[String])(implicit A: c.WeakTypeTag[A], B: c.WeakTypeTag[B]): c.Tree = {
  import c.universe._
  val Lens = symbolOf[Lens[_, _]]
  val name = TermName(c.eval(field))
  q"""new $Lens[$A, $B] {
        def get(a: $A): $B = a.$name
        def set(a: $A, b: $B): $A = a.copy($name = b)
      }"""
}
```

`lensImpl`はフィールド名を`eval`に適用して値を取り出しLensの式木を組み立てます.

`lens`は次のように使えます.

```scala
scala> val age = Lens.lens[Person, Int]("age")
age: Lens[Person,Int] = $anon$1@4d0a3812

scala> val p = Person("halcat0x15a", 21)
p: Person = Person(halcat0x15a,21)

scala> age.get(p)
res0: Int = 21

scala> age.get(age.set(p, 22))
res1: Int = 22
```

この手法には次のような特徴があります.

* 型安全
* getとsetが速い
* 実装が単純
* experimental

## Haskell: Generics

HaskellのGenericを利用してLensを導出します.

以下のようなGenericをderivingに指定したデータ型を対象とします.

```haskell
data Person = Person { name :: String, age :: Int } deriving (Eq, Show, Generic)
```

Lensの構築にはLensの合成関数を使います.

```haskell
compose :: Lens b c -> Lens a b -> Lens a c
compose f g = Lens (get f . get g) (\a -> (set g a) . (set f (get g a)))
```

フィールドは型レベルの自然数を用いてインデックスで指定します.

```haskell
data Nat = Zero | Succ Nat

data N a where
  Z :: N Zero
  S :: N a -> N (Succ a)
```

`Nat`は種として利用し, `N`は型レベルの自然数を表します.

各Rep typeに対して`GenericLens`を定義します.

```haskell
class GenericLens n f where
  type Nth n f
  genLens :: N n -> Lens (f a) (Nth n f)

instance GenericLens n (K1 i c) where
  type Nth n (K1 i c) = c
  genLens n = Lens unK1 (const K1)

instance GenericLens n f => GenericLens n (M1 i t f) where
  type Nth n (M1 i t f) = Nth n f
  genLens n = compose (genLens n) (Lens unM1 (const M1))

instance GenericLens Zero f => GenericLens Zero (f :*: g) where
  type Nth Zero (f :*: g) = Nth Zero f
  genLens n = compose (genLens n) (Lens (\case a :*: _ -> a) (\case _ :*: b -> \a -> a :*: b))

instance GenericLens n g => GenericLens (Succ n) (f :*: g) where
  type Nth (Succ n) (f :*: g) = Nth n g
  genLens (S n) = compose (genLens n) (Lens (\case _ :*: b -> b) (\case a :*: _ -> \b -> a :*: b))
```

`Nth`は型レベルで`Nat`とGenericのRep typeを対応させます.

`genLens`はRep typeに対する`Nth`で指定されたフィールドのLensを返します.

`lens`は型レベルの自然数を引数にとり, Genericであるデータ型に対する`Nth`で指定されたフィールドのLensを返します.

```haskell
lens :: (Generic a, GenericLens n (Rep a)) => N n -> Lens a (Nth n (Rep a))
lens n = Lens (get (genLens n) . from) (\a -> to . (set (genLens n) (from a)))
```

データ型をRep typeに変換し, `genLens`を用いてLensを構成します.

`lens`は次のように使えます.

```haskell
*Main> let age = lens (S Z) :: Lens Person Int
*Main> let p = Person "halcat0x15a" 21
*Main> get age $ p
21
*Main> get age $ (set age) p 22
22
```

この手法には次のような特徴があります.

* 型安全
* 型システムの上で完結する
