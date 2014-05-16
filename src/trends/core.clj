(ns trends.core
  (:gen-class)
  (:require [clojure.pprint]
            [clojure.string :refer [split join]]))

(defn slice
  "Takes a slice of length l, starting from given index (inclusively)"
  [index l s]
  (take l (drop index s)))

; clojure awesomness... lazy sequence made from infinite sequence :)
; something like python generator programming
(defn slices
  "Generates sequence of slices of length l from s.
  Last slice may be shorter than l."
  [l s]
  (take-while (comp not empty?) (map #(slice (* % l) l s) (range))))

(defn avg
  "Calculates average of collection. Will return nil if empty."
  [s]
  (if (empty? s)
    nil
    (/ (apply + s) (count s))))

; these is taken from http://www.staff.amu.edu.pl/~zcht/pliki/Regresja%20liniowa.pdf
; note how we use higher order function (function returning function) instead of
; just calculating values. This is neat api for further use :)

; let's define some helper private functions, that works on a "sample" which
; in essence is a pair where first is index (x) and second is value (y).
;
; oh and btw, clojure is so awesome, that we can write functions that look
; like arithmetic expressions!
(defn- sum-x [samples] (apply + (map first samples)))
(defn- sum-y [samples] (apply + (map second samples)))
(defn- sum-x*x [samples] (apply + (map #(* (first %) (first %)) samples)))
(defn- sum-x*y [samples] (apply + (map #(* (first %) (second %)) samples)))

(defn lin-reg
  "Returns a map with :a representing a, :b representing b and :function being
  the actual function that is the result of linear regression over sequence"
  [xs ys]
  (let [samples (map vector xs ys)
        n (count samples)]
    (if (> n 1)
      (let [x (sum-x samples)
            y (sum-y samples)
            x*x (sum-x*x samples)
            x*y (sum-x*y samples)
            a (with-precision 16 (/ (- (* n x*y) (* x y)) (- (* n x*x) (* x x))))
            b (with-precision 16 (/ (- (* y x*x) (* x x*y)) (- (* n x*x) (* x x))))]
        {:a a :b b :function #(-> % (* a) (+ b))})
      nil)))

(defn error-for
  "Returns an error function that has signature [x expected]
  The error function will then return error for given sample given by:
  `estimator(x) - expected`"
  [estimator]
  (fn [x expected] (- (estimator x) expected)))

(defn variance
  "Given function `error` will calculate variance treating xs as x-axis data
  and ys as expected values on y-axis"
  [error xs ys]
  (let [errs(map #(let [e (error %1 %2)] (* e e)) xs ys)
        err-square-sum (apply + errs)]
    (with-precision 16 (/ err-square-sum (count errs)))))

(defn std-dev
  "Given function `error` will calculate standard deviation treating xs
  as x-axis data and ys as expected values on y-axis"
  [error xs ys]
  (let [v (variance error xs ys)]
    (Math/sqrt v)))

(defn trend-lines
  "Given xs and ys as data samples will return a map containing keys
  :middle, :high, :low. Under each of this key, there is a map containing
  :a :b :function, where :a and :b represent line coefficients and
  :function represents f(x)"
  [xs ys]
  (let [m (lin-reg xs ys)
        estimator (:function m)
        a (:a m)
        b (:b m)
        error (error-for estimator)
        s*2 (* 2 (std-dev error xs ys))
        h {:a a :b (+ b s*2) :function #(-> % (* a) (+ b) (+ s*2))}
        l {:a a :b (- b s*2) :function #(-> % (* a) (+ b) (- s*2))}]
    {:middle m :high h :low l}))

; ok, the code below is actually not so pretty, but this
; is a dirty input -> data transformation.

;some private helpers again
(defn- records-to-xs
  [records]
  (take (count records) (range)))

(defn- records-to-ys
  [records]
  (map #(BigDecimal. (:price %)) records))

(defn- records-to-trend-lines
  [records]
  (let [xs (records-to-xs records)
        ys (records-to-ys records)]
    (trend-lines xs ys)))

(defn- line-row
  [period trend]
  (let [last-idx (- (count period) 1)
        high (get-in trend [:high :function])
        middle (get-in trend [:middle :function])
        low (get-in trend [:low :function])
        start-date (-> period (first) (:date))
        stop-date (-> (nth period last-idx) (:date))]
    [start-date (middle 0) (high 0) (low 0)
     stop-date (middle last-idx) (high last-idx) (low last-idx)]))

(defn -main
  "I don't do a whole lot ... yet."
  [n]
  (let [lines (take-while identity (repeatedly #(.readLine *in*)))
        rows (map #(split % #",") lines)
        data (map #(hash-map :date (first %) :price (nth % 5)) rows)
        slice-len (/ (count data) (Integer. n))
        periods (slices slice-len data)
        trends-for-periods (map records-to-trend-lines periods)
        results (map line-row periods trends-for-periods)]
    (doseq [l (map #(join "," %) results)]
      (println l))))