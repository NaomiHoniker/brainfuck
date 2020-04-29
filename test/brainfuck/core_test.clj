(ns brainfuck.core-test
  "Tests for brainfuck interpreter

   Champlain College
   CSI-380 Spring 2019"
  (:require [clojure.test :refer :all]
            [brainfuck.core :refer :all]
            [brainfuck.engine :refer :all]
            [clojure.string :as str]))

(deftest tokenize-test
  "Tests of tokenizer"
  (testing "tokenize (1 line)"
    (let [tokens (tokenize "-,+[-.*+]")]
        (is (vector? tokens))
        (is (= [{:symbol \-, :line 1, :column 1} {:symbol \,, :line 1, :column 2}
                {:symbol \+, :line 1, :column 3} {:symbol \[, :line 1, :column 4}
                {:symbol \-, :line 1, :column 5} {:symbol \., :line 1, :column 6}
                {:symbol \*, :line 1, :column 7} {:symbol \+, :line 1, :column 8}
                {:symbol \], :line 1, :column 9}]
              tokens))))

  (testing "tokenize (multi-line)"
    (let [tokens (tokenize (slurp "resources/hello_world_verbose.bf"))]
        (is (vector? tokens))
        (is (= (read-string (slurp "resources/hello_world.tok"))
              tokens)))))

(deftest find-matchings-standalone-test
  "Tests of find-matchings, which do not rely on tokenize."
  (testing "find-matchings (no errors)"
    (is (= (find-matchings 
             [{:symbol \+, :line 1, :column 1} {:symbol \+, :line 1, :column 2} 
              {:symbol \+, :line 1, :column 3} {:symbol \+, :line 1, :column 4}
              {:symbol \[, :line 4, :column 1} {:symbol \[, :line 4, :column 2}
              {:symbol \-, :line 4, :column 3} {:symbol \+, :line 4, :column 4}
              {:symbol \[, :line 4, :column 5} {:symbol \], :line 4, :column 6}
              {:symbol \], :line 4, :column 7} {:symbol \], :line 4, :column 8}]) 
             {8 9, 9 8, 5 10, 10 5, 4 11, 11 4})))

  (testing "find-matchings (no errors2)"
    (is (= (find-matchings 
             [{:symbol \+, :line 1, :column 1} {:symbol \+, :line 1, :column 2}
              {:symbol \+, :line 1, :column 3} {:symbol \+, :line 1, :column 4}
              {:symbol \[, :line 4, :column 1} {:symbol \+, :line 4, :column 2}
              {:symbol \+, :line 4, :column 3} {:symbol \+, :line 4, :column 4}
              {:symbol \], :line 4, :column 5} {:symbol \[, :line 6, :column 1}
              {:symbol \-, :line 6, :column 2} {:symbol \+, :line 6, :column 3}
              {:symbol \[, :line 6, :column 4} {:symbol \], :line 6, :column 5}
              {:symbol \], :line 6, :column 6}]) 
             {4 8, 8 4, 12 13, 13 12, 9 14, 14 9})))



  (testing "find-matchings (unmatched [)"
    (is (thrown-with-msg? RuntimeException #".*4.*1.*"
          (find-matchings
            [{:symbol \+, :line 1, :column 1} {:symbol \+, :line 1, :column 2}
             {:symbol \+, :line 1, :column 3} {:symbol \+, :line 1, :column 4}
             {:symbol \[, :line 4, :column 1} {:symbol \[, :line 4, :column 2}
             {:symbol \-, :line 4, :column 3} {:symbol \+, :line 4, :column 4}
             {:symbol \[, :line 4, :column 5} {:symbol \], :line 4, :column 6}
             {:symbol \], :line 4, :column 7} {:symbol \+, :line 4, :column 8}
             {:symbol \+, :line 4, :column 9} {:symbol \+, :line 4, :column 10}
             {:symbol \+, :line 4, :column 11}]))))

  (testing "find-matchings (unmatched ])"
    (is (thrown-with-msg? RuntimeException #".*6.*7.*"
          (find-matchings 
            [{:symbol \+, :line 1, :column 1} {:symbol \+, :line 1, :column 2}
             {:symbol \+, :line 1, :column 3} {:symbol \+, :line 1, :column 4}
             {:symbol \[, :line 1, :column 5} {:symbol \[, :line 4, :column 1}
             {:symbol \[, :line 4, :column 2} {:symbol \-, :line 4, :column 3}
             {:symbol \+, :line 4, :column 4} {:symbol \[, :line 4, :column 5}
             {:symbol \], :line 4, :column 6} {:symbol \], :line 4, :column 7}
             {:symbol \], :line 4, :column 8} {:symbol \+, :line 4, :column 9}
             {:symbol \], :line 4, :column 10} {:symbol \+, :line 4, :column 11}
             {:symbol \+, :line 6, :column 1} {:symbol \., :line 6, :column 2}
             {:symbol \., :line 6, :column 3} {:symbol \., :line 6, :column 4}
             {:symbol \., :line 6, :column 5} {:symbol \., :line 6, :column 6}
             {:symbol \], :line 6, :column 7} {:symbol \+, :line 6, :column 8} 
             {:symbol \+, :line 6, :column 9}])))))


(deftest tokenize-and-find-matchings-test
  "Tests of tokenizing and then finding matchings"
  (testing "find-matchings (no errors)"
    (is (= (find-matchings (tokenize "++++\n\n\n[[-+[]]]")) {8 9, 9 8, 5 10, 10 5, 4 11, 11 4})))
    
  (testing "find-matchings (no errors 2)"
    (is (= (find-matchings (tokenize "++++\n\n\n[+++]\n\n[-+[]]")) {4 8, 8 4, 12 13, 13 12, 9 14, 14 9})))

  (testing "find-matchings (unmatched [)"
    (is (thrown-with-msg? RuntimeException #".*4.*1.*"
          (find-matchings (tokenize "++++\n\n\n[[-+[]]++++")))))

  (testing "find-matchings (unmatched ])"
    (is (thrown-with-msg? RuntimeException #".*6.*7.*"
          (find-matchings (tokenize "++++[\n\n\n[[-+[]]]+]+\n\n+.....]++"))))))


(deftest code-to-string-test
  "Tests of code-to-string macro"
  (testing "code-to-string (1 arg)"
    (is (= "++++----+++++<>" (code-to-string ++++----+++++<>))))

  (testing "code-to-string (multiple args)"
    (is (= "+++---+++---<+++>[]" (str/replace (code-to-string +++ --- +++ --- < +++ > []) " " "")))))


(deftest brainfuck-test
  "Tests of the complete interpreter"
  (testing "hello world"
    (is (= "Hello World!\n"
          (with-out-str
            (run-source "resources/hello_world_verbose.bf")))))

  (testing "cat (as str)"
    (is (= "foobar"
          (with-in-str "foobar"
            (with-out-str
              (run "-,+[-.,+]"))))))

  (testing "cat (as str with *s)"
    (is (= "foobar"
          (with-in-str "foobar"
            (with-out-str
              (run "-*+[-.*+]"))))))

  (testing "cat (using bf macro)"
    (is (= "foobar"
          (with-in-str "foobar"
            (with-out-str
              (bf -*+[-.*+]))))))

  (testing "rot13"
    (is (= "sbbone"
          (with-in-str "foobar"
            (with-out-str
              (run-source "resources/rot13.bf"))))))

  (testing "fibonacci"
    (is (= "1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89"
          (with-out-str
            (run-source "resources/fibonacci.bf")))))

  (testing "cell-size"
    (is (= "8 bit cells\n"
          (with-out-str
            (run-source "resources/cell_size.bf")))))

  (testing "beer"
    (is (= (slurp "resources/beer.out")
          (with-out-str
            (run-source "resources/beer.bf"))))))
