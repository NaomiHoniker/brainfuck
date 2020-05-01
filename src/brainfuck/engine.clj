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
  ;; Split the code by newlines
  (let [split-by-line (str/split-lines code)]
    ;; Comp vec flatten vector will flatten the map-indexed's sequences into a vector.
    ((comp vec flatten vector) (map-indexed (fn [line-num line]
                                              ;; Remove all invalid symbols, which will be added as nil, then
                                              ;; map-index to run through each character of each line.
                                              (remove nil? (map-indexed (fn [col-num symbol]
                                                                          (if (clojure.string/includes? "><+-.,*[]" (str symbol))
                                                                            ;; If the symbol is valid Brainfuck symbols:
                                                                            ;; Add the symbol, line number, and column number to the map.
                                                                            (assoc {} :symbol symbol :line (+ line-num 1) :column (+ col-num 1)
                                                                                      )
                                                                            ;; If not:
                                                                            ;; do nothing
                                                                            )) line))) split-by-line))
    )
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
          
          ;; https://en.wikipedia.org/wiki/Brainfuck#Language_design
          ;;"increment the data pointer (to point to the next cell to the right)."
          (= symbol \>)
            (recur data (inc-byte data-pointer) (inc instruction-pointer))

          ;; "decrement the data pointer (to point to the next cell to the left)."
          (= symbol \<)
            (recur data (dec-byte data-pointer) (inc instruction-pointer))

          ;; "increment (increase by one) the byte at the data pointer."
          (= symbol \+)
            (recur (assoc data data-pointer (inc-byte datum)) data-pointer (inc instruction-pointer))

          ;; "decrement (decrease by one) the byte at the data pointer."
          (= symbol \-)
            (recur (assoc data data-pointer (dec-byte datum)) data-pointer (inc instruction-pointer))

          ;; "output the byte at the data pointer."
          (= symbol \.)
            (do (println (char datum)) (recur data data-pointer (inc instruction-pointer)))

          ;; "accept one byte of input, storing its value in the byte at the data pointer."
          (or (= symbol \,) (= symbol \*))
            ;; accept one byte of input, storing its value in the byte at the data pointer. 
            (let [input (.read *in*)]
              (recur (assoc data data-pointer input) data-pointer (inc instruction-pointer)))

          ;; "if the byte at the data pointer is zero, then instead of moving the instruction
          ;; pointer forward to the next command, jump it forward to the command after the matching ] command."
          (= symbol \[)
            (if (= datum 0)
              ;; If the byte at the data pointer is zero...
              (let [jump-point (get matchings instruction-pointer)]
                (recur data data-pointer jump-point))
              ;; Otherwise...
              (recur data data-pointer (inc instruction-pointer))
              )

          ;; "if the byte at the data pointer is nonzero, then instead of moving the instruction
          ;; pointer forward to the next command, jump it back to the command after the matching [ command."
          (= symbol \])
            (if (not= datum 0)
              ;; If the byte at the data pointer is zero...
              (let [jump-point (get matchings instruction-pointer)]
                (recur data data-pointer jump-point))
              ;; Otherwise...
              (recur data data-pointer (inc instruction-pointer))
              )

          :else (recur data data-pointer (inc instruction-pointer)))))))
