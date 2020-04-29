(ns brainfuck.engine
  "Engine for Clojure interpreter.
  
   Champlain College
   CSI-380 Spring 2019"
  (:gen-class)
  (:require [clojure.string :as str]
            [brainfuck.utils :refer [inc-byte dec-byte]]))

(defn tokenize
  "Tokenize the given code: return a vector of tokens, where each token is a 
  map with the following entries
    :symbol the symbol (character)
    :line the line number the token is on (starting at 1)
    :column the column in the line the token is on (starting at 1)
  
  Note: for efficiency the only tokens that should be returned are those containing
  valid (augmented) brainfuck symbols: > < + - . , * [ ]
  "
  [code]
  ;; Code goes here
  nil
  )

(defn find-matchings
  "Parse the given tokens returning a map that contains an entry for every [ and ]
  mapping its index in the given tokens to it matching symbol and vice-versa.
  
  throws a RuntimeException any unmatched [ or ]
  The RuntimeException's message will have a useful message including the line and column
  that the error occurred.
  "
  [tokens]
  (loop [index 0
       nodes '()
       stack '()
       matchings {}]
       
    (if (>= index (count tokens))
      (if (not-empty stack)
        (let [node (:node (peek stack))]
            (throw (RuntimeException. (str "Unmatched [ at " (:line node) ":" (:column node)))))
        matchings)
      (let [node (nth tokens index)]
        (cond 
          (= (node :symbol) \[)
            (recur (inc index) (conj nodes node) (conj stack {:node node :index index}) matchings)
          (= (node :symbol) \])
            (if (not-empty stack)
              (let [match ((peek stack) :index)]
                (recur (inc index) (conj nodes node) (pop stack) (assoc matchings match index index match)))
              (throw (RuntimeException. (str "Unmatched ] at " (:line node) ":" (:column node)))))
          :else (recur (inc index) (conj nodes node) stack matchings))))))
          

(defn interpret
  "Interpret the given instructions (tokens) with the given matchings map
   that maps the index of each [ and ] to the index of its matching symbol.
   
   Reads from *in* (stdin) and prints the output to *out* (stdout; default behavior of print), 
   returns the final state of the
   machine, a map with
    :data the current data cells (a map from indices to byte values)
    :data-pointer the current data pointer (index)
    :instruction-pointer the current instruction pointer (index)"
  [instructions matchings]

  (loop [data {}
         data-pointer 0
         instruction-pointer 0]
    (if (>= instruction-pointer (count instructions))
       ;; if we are past the last instruction, we are done so return machine state
       {:data data :data-pointer data-pointer :instruction-pointer instruction-pointer}

       ;; otherwise process the current instruction
       (let [instruction (nth instructions instruction-pointer)
             symbol  (instruction :symbol)
             datum (data data-pointer 0)]
        (cond 
          ;; Code goes here
          
          ;; you implement other cases
          
          ;; we are providing the input case for you
          (or (= symbol \,) (= symbol \*))
            ;; accept one byte of input, storing its value in the byte at the data pointer. 
            (let [input (.read *in*)]
              (recur (assoc data data-pointer input) data-pointer (inc instruction-pointer)))

          :else (recur data data-pointer (inc instruction-pointer)))))))
