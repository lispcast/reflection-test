(ns reflection-test.core-test
  (:require [clojure.test :refer :all]
            [reflection-test.core :refer :all]))

(deftest parse-test
  (let [rows (parse-csv (clojure.java.io/resource "test1.csv"))]
    (is (= rows
           [["1","2","3"]
            ["A","B","C"]]))))

(deftest gen-parse-test
  (let [rows (for [x (range 3)]
               (map str (range (* 1000 x) (+ 3 (* 1000 x)))))
        tmpfile (str "resources/temp.csv")]
    (try
      (output-csv rows tmpfile)
      (let [read-rows (parse-csv tmpfile)]
        (is (= rows read-rows)))
      (finally
        (.delete (java.io.File. tmpfile))))))
