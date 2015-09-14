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
