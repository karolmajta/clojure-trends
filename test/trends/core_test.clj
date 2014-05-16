(ns trends.core-test
  (:require [clojure.test :refer :all]
            [trends.core :refer :all]))

(deftest slice-test
  (testing "takes proper slices"
    (is (= [1 2] (slice 0 2 [1 2 3 4 5])))
    (is (= [3 4] (slice 2 2 [1 2 3 4 5])))
    (is (= [4 5] (slice 3 2 [1 2 3 4 5]))))
  (testing "behaves well at the end of sequence"
    (is (= [5] (slice 4 6 [1 2 3 4 5])))
    (is (= [] (slice 5 10 [1 2 3 4 5])))))

(deftest slices-test
  (testing "splits sequence into slices of given length"
    (is (= [[1 2] [3 4] [5 6] (slices 2 [1 2 3 4 5 6])])))
  (testing "truncates last slice if s is not multiple of l"
    (is (= [[1 2 3] [4 5]] (slices 3 [1 2 3 4 5])))))

(deftest avg-test
  (testing "calculates average of items in sequence"
    (is (= 1 (avg [1 1 1])))
    (is (= (/ 8 3) (avg [1 2 5]))))
  (testing "empty sequence has no average"
    (is (nil? (avg [])))))

(deftest lin-reg-test
  (testing "linear regression cannot be done for 0 or 1 samples"
    (is (nil? (lin-reg [] [])))
    (is (nil? (lin-reg [5] [10]))))
  (testing "a test case that we know results for..."
    (is (=
          [(/ 1252 785) (/ 387 157)]
          ((juxt :a :b) (lin-reg [0 1 2 3 4 10 30] [2 2 10 8 5 20 50]))))))

(deftest error-for-test
  (testing "will return a function"
           "that uses given estimator to calculate error"
    (is (= 0 ((error-for (fn [x] 10)) 0 10)))
    (is (= -9 ((error-for (fn [x] (+ 1 x))) 0 10)))))

(deftest variance-test
  (testing "will calculate variance of samples using given error function"
    (is (= 1 (variance (fn [x expected] 1) [1 2 3] [18 18 9 13])))))