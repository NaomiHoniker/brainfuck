(ns brainfuck.utils
  "Utils for Clojure interpreter.
  
   Champlain College
   CSI-380 Spring 2019"
  (:gen-class))


(defn inc-byte
    "Increment the given value wrapping around from 255 to 0.  
    e.g. (inc-byte 255) -> 0
         (int-byte 6) -> 7"
    [byte]
    (if (< byte 255)
        (inc byte)
        0))

(defn dec-byte
    "Decrement the given value wrapping around from 0 to 255.  
    e.g. (dec-byte 0) -> 255
         (dec-byte 6) -> 5"
    [byte]
    (if (> byte 0)
        (dec byte)
        255))

