(ns generic.complex
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.algo.generic.arithmetic :refer (+ - * / zero one)]))

(defrecord Complex [re im])

(defmethod + [Complex Number] [c n] (+ c (Complex. n 0)))
(defmethod + [Number Complex] [n c] (+ (Complex. n 0) c))
(defmethod + [Complex Complex] [c c']
  (Complex. (+ (.re c) (.re c')) (+ (.im c) (.im c'))))

(defmethod - [Complex Number] [c n] (- c (Complex. n 0)))
(defmethod - [Number Complex] [n c] (- (Complex. n 0) c))
(defmethod - [Complex Complex] [c c']
  (Complex. (- (.re c) (.re c')) (- (.im c) (.im c'))))

(defmethod * [Complex Number] [c n] (* c (Complex. n 0)))
(defmethod * [Number Complex] [n c] (* (Complex. n 0) c))
(defmethod * [Complex Complex] [c c']
  (Complex. (- (* (.re c) (.re c')) (* (.im c) (.im c')))
            (+ (* (.re c) (.im c')) (* (.im c) (.re c')))))

(defmethod / [Complex Number] [c n] (/ c (Complex. n 0)))
(defmethod / [Number Complex] [n c] (/ (Complex. n 0) c))
(defmethod / [Complex Complex] [c c']
  (let [d (+ (* (.re c') (.re c')) (* (.im c') (.im c')))]
    (Complex. (/ (+ (* (.re c) (.re c')) (* (.im c) (.im c'))) d)
              (/ (- (* (.im c) (.re c')) (* (.re c) (.im c'))) d))))
