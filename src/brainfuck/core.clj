(ns brainfuck.core
  "Clojure Brainfuck Interpreter. Usage:
   lein run [-- src ...]

   Or, from the REPL/other code: (bf valid brainfuck code)

   See: https://en.wikipedia.org/wiki/Brainfuck for details
   
   Note: to make our bf macro work, we augment the language by considering
   * to be equivalent to , and any code using the bf macro must use *s instead
   of ,s since the Clojure reader treats ,s as whitespace

   Champlain College
   CSI-380 Spring 2019"
  (:gen-class)
  (:require [brainfuck.engine :refer [tokenize interpret find-matchings]]))
  

(defmacro code-to-string
  "Return a string containing the given raw code
  
  Ex (code-to-string + - + < >) -> \"+-+<>\"
  
  Whether the string contains whitespace is optional.
  "
  [first & rest]
  ;; code goes here
  nil
  )

(defmacro bf
   "Run the given raw code"
   [& code]
   `(run (code-to-string ~@code)))

(defn run
  "Run the given code, where code is a string" 
  [code]
  (let [tokens (tokenize code)]
    (interpret tokens (find-matchings tokens))))

(defn run-source
  "Run the given source file"
  [src]
  (run (slurp src)))

(defn -main
  "Usage: lein run [-- src ...]
  
  Run given brainfuck src files or run example programs if no src provided.
  "
  [& args]
  (if (not-empty args)
    ;; if any args run those files
    (doseq [arg args] (run-source arg))
    (do
      ;; hello world!
      (bf ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>  ->> + [ < ]<-]>>.>---.+  +  +  
          cool stuff, eh?
          ++++..+++.>>.<-.<.+++.------.--------.>>+.>++.)
   
      (with-in-str "this will be \"catted\"\n"
        (bf -*+[-.*+]))
  
      (bf     the following will print the first 11 fibonacci numbers!
          
        +++++++++++ number of digits to output
        > 1
        + initial number
        >>>> 5
        ++++++++++++++++++++++++++++++++++++++++++++ (comma)
        > 6
        ++++++++++++++++++++++++++++++++ (space)
        <<<<<< 0
        [
          > 1
          copy 1 to 7
          [>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]

          <
          divide 7 by 10 (begins in 7)
          [
          >
          ++++++++++  set the divisor 8
          [
            subtract from the dividend and divisor
            -<-
            if dividend reaches zero break out
            copy dividend to 9
            [>>+>+<<<-]>>>[<<<+>>>-]
            set 10
            +
            if 9 clear 10
            <[>[-]<[-]]
            if 10 move remaining divisor to 11
            >[<<[>>>+<<<-]>>[-]]
            jump back to 8 (divisor possition)
            <<
          ]
          if 11 is empty (no remainder) increment the quotient 12
          >>> 11
          copy to 13
          [>>+>+<<<-]>>>[<<<+>>>-]
          set 14
          +
          if 13 clear 14
          <[>[-]<[-]]
          if 14 increment quotient
          >[<<+>>[-]]
          <<<<<<< 7
          ]

          quotient is in 12 and remainder is in 11
          >>>>> 12
          if 12 output value plus offset to ascii 0
          [++++++++++++++++++++++++++++++++++++++++++++++++.[-]]
          subtract 11 from 10
          ++++++++++  12 is now 10
          < 11
          [->-<]
          > 12
          output 12 even if it's zero
          ++++++++++++++++++++++++++++++++++++++++++++++++.[-]
          <<<<<<<<<<< 1

          check for final number
          copy 0 to 3
          <[>>>+>+<<<<-]>>>>[<<<<+>>>>-]
          <- 3
          if 3 output (comma) and (space)
          [>>.>.<<<[-]]
          << 1

          [>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-
        ])
  ))
  
  
  ;; always print two new lines for clean output
  (print "\n\n")
)
