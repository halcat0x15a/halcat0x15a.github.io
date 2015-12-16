---
layout: default
title: Hello, world!
---

# Hello, world!

初歩的なプログラムをいろいろな言語を使って記述していきます。

Hello worldは標準出力に"Hello, world!"と出力します。

## C

```c
#include <stdio.h>

int main() {
  puts("Hello, world!");
  return 0;
}
```

## Go

```go
package main

import "fmt"

func main() {
    fmt.Println("Hello, world!")
}
```

## C++

```cpp
#include <iostream>

int main() {
  std::cout << "Hello, world!" << std::endl;
  return 0;
}
```

## Java

```java
public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }
}
```

## C# 

```csharp
class Program
{
    static void Main()
    {
        System.Console.WriteLine("Hello, world!");
    }
}
```

## Scala

```scala
object Main extends App {
  println("Hello, world!")
}
```

## Haskell

```hs
main = putStrLn "Hello, world!"
```

## Python

```py
print("Hello, world!")
```

## Ruby

```rb
puts "Hello, world!"
```

# Fizz Buzz

1から100までのFizz Buzzを出力します。

## C

```c
#include <stdio.h>

int main() {
  for (int i = 1; i <= 100; i++)
    if (i % 15 == 0)
      puts("FizzBuzz");
    else if (i % 3 == 0)
      puts("Fizz");
    else if (i % 5 == 0)
      puts("Buzz");
    else
      printf("%d\n", i);
  return 0;
}
```

## Go

```go
package main

import "fmt"

func main() {
    for i := 1; i <= 100; i++ {
        if i % 15 == 0 {
            fmt.Println("FizzBuzz")
        } else if i % 3 == 0 {
            fmt.Println("Fizz")
        } else if i % 5 == 0 {
            fmt.Println("Buzz")
        } else {
            fmt.Println(i)
        }
    }
}
```

## C++

```cpp
#include <iostream>

int main() {
  for (int i = 1; i <= 100; i++)
    if (i % 15 == 0)
      std::cout << "FizzBuzz" << std::endl;
    else if (i % 3 == 0)
      std::cout << "Fizz" << std::endl;
    else if (i % 5 == 0)
      std::cout << "Buzz" << std::endl;
    else
      std::cout << i << std::endl;
  return 0;
}
```

## Java

```java
public class Main {
    public static void main(String[] args) {
        for (int i = 1; i <= 100; i++)
            if (i % 15 == 0)
                System.out.println("FizzBuzz");
            else if (i % 3 == 0)
                System.out.println("Fizz");
            else if (i % 5 == 0)
                System.out.println("Buzz");
            else
                System.out.println(i);
    }
}
```

## C# 

```csharp
class Program
{
    static void Main()
    {
        for (int i = 1; i <= 100; i++)
            if (i % 15 == 0)
                System.Console.WriteLine("FizzBuzz");
            else if (i % 3 == 0)
                System.Console.WriteLine("Fizz");
            else if (i % 5 == 0)
                System.Console.WriteLine("Buzz");
            else
                System.Console.WriteLine(i);
    }
}
```

## Scala

```scala
object Main extends App {
  for (i <- 1 to 100)
    if (i % 15 == 0)
      println("FizzBuzz")
    else if (i % 3 == 0)
      println("Fizz")
    else if (i % 5 == 0)
      println("Buzz")
    else
      println(i)
}
```

## Haskell

```hs
import Data.Foldable (for_)

main :: IO ()
main = for_ [1..100] $ putStrLn . fizzbuzz
  where
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod`  3 == 0 = "Fizz"
      | n `mod`  5 == 0 = "Buzz"
      | otherwise       = show n
```

## Python

```py
for i in range(1, 101):
    if i % 15 == 0:
        print("FizzBuzz")
    elif i % 3 == 0:
        print("Fizz")
    elif i % 5 == 0:
        print("Buzz")
    else:
        print(i)
```

## Ruby

```rb
for i in 1..100
  if i % 15 == 0
    puts "FizzBuzz"
  elsif i % 3 == 0
    puts "Fizz"
  elsif i % 5 == 0
    puts "Buzz"
  else
    puts i
  end
end
```

# Fibonacci numbers

フィボナッチ数列を先頭から50個出力します。

## C

```c
#include <stdio.h>

int main() {
  long buf[50];
  for (int i = 0; i < 50; i++) {
    buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2];
    printf("%ld\n", buf[i]);
  }
  return 0;
}
```

## Go

```go
package main

import "fmt"

func main() {
    var buf = make([]int, 50)
    for i := range buf {
        if i < 2 {
            buf[i] = i
        } else {
            buf[i] = buf[i - 1] + buf[i - 2]
        }
        fmt.Println(buf[i])
    }
}
```

## C++

```cpp
#include <iostream>
#include <array>

int main() {
  std::array<long, 50> buf;
  for (int i = 0; i < buf.size(); i++) {
    buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2];
    std::cout << buf[i] << std::endl;
  }
  return 0;
}
```

## Java

```java
public class Main {
    public static void main(String[] args) {
        long[] buf = new long[50];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2];
            System.out.println(buf[i]);
        }
    }
}
```

## C# 

```csharp
class Program
{
    static void Main()
    {
        var buf = new long[50];
        for (int i = 0; i < buf.Length; i++)
        {
            buf[i] = i < 2 ? i : buf[i] = buf[i - 1] + buf[i - 2];
            System.Console.WriteLine(buf[i]);
        }
    }
}
```

## Scala

```scala
object Main extends App {
  val fib: Stream[Long] = 0L #:: fib.scanLeft(1L)(_ + _)
  fib.take(50).foreach(println)
}
```

## Haskell

```hs
import Data.Foldable (for_)

main :: IO ()
main = for_ (take 50 fib) (putStrLn . show)
  where
    fib = 0 : 1 : zipWith (+) fib (tail fib)
```

## Python

```python
buf = []
for i in range(0, 50):
    buf.append(i if i < 2 else buf[i - 1] + buf[i - 2])
    print(buf[i])
```

## Ruby

```ruby
buf = []
for i in 0...50
  buf[i] = i < 2 ? i : buf[i - 1] + buf[i - 2]
  puts buf[i]
end
```
