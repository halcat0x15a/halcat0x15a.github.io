---
layout: default
title: Hello, world!
---

# Hello, world!

初歩的なプログラムをいろいろな言語を使って記述していきます.

Hello worldは標準出力に"Hello, world!"と出力します.

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

1から100までのFizz Buzzを出力します.

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
import Control.Monad (forM_)

fizzbuzz :: Int -> String
fizzbuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod`  3 == 0 = "Fizz"
  | n `mod`  5 == 0 = "Buzz"
  | otherwise       = show n

main :: IO ()
main = forM_ [1..100] $ putStrLn . fizzbuzz
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

フィボナッチ数列を先頭から50個出力します.

## C

```c
#include <stdio.h>

int main() {
  puts("0");
  puts("1");
  long buf[50] = {0, 1};
  for (int i = 2; i < 50; i++) {
    buf[i] = buf[i - 1] + buf[i - 2];
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
    fmt.Println(0)
    fmt.Println(1)
    var buf = [50]int{0, 1}
    for i := 2; i < 50; i++ {
        buf[i] = buf[i - 1] + buf[i - 2]
        fmt.Println(buf[i])
    }
}
```

## C++

```cpp
#include <iostream>

int main() {
  std::cout << 0 << std::endl << 1 << std::endl;
  long buf[50] = {0, 1};
  for (int i = 2; i < 50; i++) {
    buf[i] = buf[i - 1] + buf[i - 2];
    std::cout << buf[i] << std::endl;
  }
  return 0;
}
```

## Java

```java
public class Main {
    public static void main(String[] args) {
        System.out.println(0);
        System.out.println(1);
        long[] buf = new long[50];
        buf[0] = 0;
        buf[1] = 1;
        for (int i = 2; i < 50; i++) {
            buf[i] = buf[i - 1] + buf[i - 2];
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
        System.Console.WriteLine(0);
        System.Console.WriteLine(1);
        var buf = new long[50];
        buf[0] = 0;
        buf[1] = 1;
        for (int i = 2; i < 50; i++)
        {
            buf[i] = buf[i - 1] + buf[i - 2];
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
import Control.Monad (forM_)

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

main :: IO ()
main = forM_ (take 50 fib) (putStrLn . show)
```

## Python

```python
print(0)
print(1)
buf = [0, 1]
for i in range(2, 50):
    buf.append(buf[i - 1] + buf[i - 2])
    print(buf[i])
```

## Ruby

```ruby
puts 0, 1
buf = [0, 1]
for i in 2...50
  buf[i] = buf[i - 1] + buf[i - 2]
  puts buf[i]
end
```
