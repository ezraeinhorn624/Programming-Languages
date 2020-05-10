package main

import (
       "fmt"
       "container/list"
)

func everyNth(lst *list.List, n int) *list.List {
     l := list.New()
     i := 1
     for e := lst.Front(); e != nil; e = e.Next() {
     	 if i % n == 0 {
	    l.PushBack(e.Value)
	 }
	 i++
     }
     return l
}

func main() {
     test := list.New()
     result := list.New()

     for i := 0; i < 1000; i++ {
     	 test.PushBack(i)
     }

     result = everyNth(test, 109)
     for e := result.Front(); e != nil; e = e.Next() {
     	 fmt.Println(e.Value)
     }
}