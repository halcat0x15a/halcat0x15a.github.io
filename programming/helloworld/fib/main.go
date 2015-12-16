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
