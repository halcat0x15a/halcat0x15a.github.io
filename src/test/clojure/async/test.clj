(ns async.test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<! >! <!! >!! go chan timeout alt! alts! alt!! alts!!]]))

(deftest channel
  (let [c (chan)
        r (future (<!! c))]
    (>!! c "hello")
    (is (= @r "hello")))
  (let [n (System/nanoTime)]
    (is (= (<!! (timeout 1000)) nil))
    (is (< 1000000000 (- (System/nanoTime) n)))))

(deftest alternative
  (let [c1 (chan)
        c2 (chan)]
    (future (>!! c1 :foo)
            (>!! c2 :bar))
    (is (= (alts!! [c1 c2]) [:foo c1]))
    (is (= (alts!! [c1 c2]) [:bar c2])))
  (let [c1 (chan)
        c2 (chan)]
    (future (>!! c1 :foo)
            (>!! c2 :bar))
    (is (= (alt!! c1 ([v c] [v c])
                  c2 ([v c] [v c]))
           [:foo c1]))
    (is (= (alt!! c1 ([v c] [v c])
                  c2 ([v c] [v c]))
           [:bar c2])))
  (is (= (alt!! (timeout 10000) ([] :foo)
                (timeout 100) ([] :bar))
         :bar)))

(deftest goroutine
  (let [c (chan)]
    (go (loop [n 0]
          (>! c n)
          (recur (inc n))))
    (go (is (= (<! c) 0))
        (is (= (<! c) 1))
        (is (= (<! c) 2))))
  (let [c (chan)]
    (dotimes [n 100]
      (go
        (Thread/sleep 1000)
        (>! c (.getId (Thread/currentThread)))))
    (is (= (count (distinct (repeatedly 100 #(<!! c)))))
        (+ (* (.availableProcessors (Runtime/getRuntime)) 2) 42))))
