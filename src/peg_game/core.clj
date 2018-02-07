(ns peg-game.core
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
     (let [new-sum (+ sum n)]
       (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))
(prn (take 5 tri))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))


(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))


;; Connect takes a current board, a max-pos, a current pos
;; The middle-ish neighbor pos, and a destination
(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))
(connect {} 15 1 2 4)
;; => { 1 {:connections {4 2}}
;;      4 {:connections {1 2}} }
;; these are good examples of how assoc-in works
(assoc-in {} [:cookie :monster :vocals] "Finntroll")
; => {:cookie {:monster {:vocals "Finntroll"}}}

(get-in {:cookie {:monster {:vocals "Finntroll"}}} [:cookie :monster])
; => {:vocals "Finntroll"}

(assoc-in {} [1 :connections 4] 2)
; => {1 {:connections {4 2}}}


;; This is some impressive stuff. Because there are many ways to move in
;; the peg game we are going to create functions that also work as mirrors
;; for the moves you can make right(also goes left) down-left (also goes
;; up-right)
(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

;; This function adds legal moves/connection from the current position
;; of the peg
(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

;; I feel these function are pretty well defined and self explanatory
(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

;; Also self-documented
(defn valid-moves
  "Return a map of all valid moves for pos where the key is the destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

;; If as this function says 4 :pegged you get something
  ;;       1
 ;;      2  3
 ;;     O  5  6
 ;;   7  8  9  10
 ;; 11 12 13 14 15
(def my-board (assoc-in (new-board 5) [4 :pegged] false))
;; Therefore: in that board at position 1 the only valid is to
;; JUMP TO:  4   4   4    these are the multiple moves
;; FROM POS: 2   5   7
;; (valid-moves my-board 1)  ; => {4 2}
;; (valid-moves my-board 6)  ; => {4 5}
;; (valid-moves my-board 11) ; => {4 7}

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged position have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

;; Rendering and printing the Board
(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(defn render-pos
  "Renders the 0 or the - for if there is a peg or not"
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         "0"
         "-"
         ;; (style "0" :blue)
         ;; (style "-" :red)
         )))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))

(defn print-board
  "Prints the board"
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))


(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
     (let [input (clojure.string/trim (read-line))]
       (if (empty? input)
         default
         (clojure.string/lower-case input)))))


(defn user-entered-invalid-move
  "Handles the next step after a user has entered an invalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))


(defn user-entered-valid-move
  "Handles the next step after a user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

;; I custom wrote these equations with the help of Ryan to fill in for chars-as-strings
(defn is-char?
  [c]
  (when-not (= c \space)
    c))

(defn characters-as-strings
  [string]
  (map str (filter is-char? string)))


(defn prompt-move
  "Provide the player with a board and a prompt to make a move"
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn prompt-empty-peg
  "Print a new board with an empty peg at some position"
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))


(defn prompt-rows
  "How many rows should be in this new board?"
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))


(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))






(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
