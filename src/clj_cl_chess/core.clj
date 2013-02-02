( ns clj-cl-chess.core
  (:refer-clojure :exclude [== >= <= > < =])
  (:use clojure.core.logic
        [clojure.core.logic.arithmetic :only [>= <= > < =]])
)
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn knight-moves 
"Returns the available moves for a knight (on a 8x8 grid) given its current position." 
 [x y]
(let [xmax 8 ymax 8]
 (run* [q] ;bring back all possible solutions
 (fresh [a b] ;like 'let' but for logic variables
  (conde ;;like OR
    [(< (+ x 1) xmax) (< (+ y 2) ymax) (== a (+ x 1)) (== b (+ y 2))] ;1st possibility
    [(< (+ x 2) xmax) (< (+ y 1) ymax) (== a (+ x 2)) (== b (+ y 1))] ;2nd possibility
    [(< (+ x 2) xmax) (>= (- y 1)   0) (== a (+ x 2)) (== b (- y 1))] ;3rd possibility
    [(< (+ x 1) xmax) (>= (- y 2)   0) (== a (+ x 1)) (== b (- y 2))] ;4th possibility
    [(>= (- x 1)   0) (>= (- y 2)   0) (== a (- x 1)) (== b (- y 2))] ;5th possibility
    [(>= (- x 2)   0) (>= (- y 1)   0) (== a (- x 2)) (== b (- y 1))] ;6th possibility
    [(>= (- x 2)   0) (< (+ y 1) ymax) (== a (- x 2)) (== b (+ y 1))] ;7th possibility
    [(>= (- x 1)   0) (< (+ y 2) ymax) (== a (- x 1)) (== b (+ y 2))] ;8th possibility
  ) 
  (== q [a b]))))) ;return each solution in a vector [x, y]



(def ^:const board (vec (range 8)))

(defn rook-moves 
"Returns the available moves for a rook (on a 8x8 grid) given its current position."
[x y]
 (run* [q]
 (fresh [a b]
 (conde 
  [(membero a board) (!= a x) (== b y)]  ;y remains constant
  [(membero b board) (!= b y) (== a x)]) ;x remains constant
 (== q [a b]))))



(defn bishop-moves 
"Returns the available moves for a bishop (on a 8x8 grid) given its current position and direction."
[x y]
(run* [q] 
(fresh [a b] 
  (membero a board) 
  (membero b board)
   (!= a x) 
   (!= b y)
   (project [x y a b]
    (== (Math/abs (- x a)) 
        (Math/abs (- y b)))
    (== q [a b])))))

(defn c2dto1d [v]
  (let [[x y] v]
    (clojure.core/+ x (clojure.core/* 8 y))))

(defn c1dto2d [i]
  (vector (int (/ i 8)) (mod i 8)))

;;(c2dto1d [1 1])
;;(c1dto2d 63)




(defn render-board [board-state]
  (let [line "+----+----+----+----+----+----+----+----+"
        pieces-pos board-state ;(into {} board-state)
        ]
    (apply str "\n" line "\n"
           (map #(let [pos (c1dto2d (dec %))
                       c (get pieces-pos pos "  ")]
                   (if (zero? (mod % 8))
                           (format "| %s |\n%s\n" c line)
                           (format "| %s " c))) (range 1 65)))))

(defn display-board [board-state]
  (print (render-board board-state)))

(display-board '([[1 0] "p"] [[2 2] "*"] [[0 1] "&"]))

(defn moves2state [f-mov pos c]
  (into {} (cons [pos c] (map #(vector % "* ") (apply f-mov pos)))))

(defn char2state [pieces-list]
                  (into {} (filter #(not= " " (second %)) (map #(vector (c1dto2d %1) %2 ) (range 64) pieces-list))))

(def init-board-state (char2state ["br" "bb" "bn" "bq" "bk" "bn" "bb" "br"
                             "bp" "bp" "bp" "bp" "bp" "bp" "bp" "bp"
                             " " " " " " " " " " " " " " " "
                             " " " " " " " " " " " " " " " "
                             " " " " " " " " " " " " " " " "
                             " " " " " " " " " " " " " " " "
                             "wp" "wp" "wp" "wp" "wp" "wp" "wp" "wp"
                             "wr" "wb" "wn" "wq" "wk" "wn" "wb" "wr"]))


;
(display-board init-board-state)

(display-board (moves2state knight-moves [2 1] "k "))

(display-board (into {} (cons [[0 1] "k "] (map #(vector % "* ") (knight-moves 0 1)))))


(knight-moves 0 1)
(bishop-moves 5 5)

(defn get-type [piece-pos] (let [res (second (seq (second piece-pos)))]
                             (println res)
                             res))

(defn get-moves [state pos]
  (let [piece (get state pos)] 1))

(defn rook-moves2 
"Returns the available moves for a rook (on a 8x8 grid) given its current position."
[state x y]
 (run* [q]
 (fresh [a b]
 (conde 
  [(membero a board) (!= a x) (== b y)]  ;y remains constant
  [(membero b board) (!= b y) (== a x)]) ;x remains constant
 (== q [a b]))))

(defmulti get-available-moves (fn [state pos] (get-type (vector pos (get state pos)))))
(defmethod get-available-moves \n [state pos] (let [piece (get state pos)] (apply knight-moves pos)))
(defmethod get-available-moves \b [state pos] (let [piece (get state pos)] (apply bishop-moves pos)))
(defmethod get-available-moves \r [state pos] (let [piece (get state pos)] (apply rook-moves2 state pos)))
;(defmethod get-available-moves \p [state pos] (let [piece (get state pos)] (apply pawn-moves pos)))

(into '() init-board-state)
                                        ;(doc everyg)
(comment
(defn memo [x l out]
  (conde
    [(emptyo l) u#]
    [(firsto l x) (== l out)]
    [(fresh (d)
            (resto l d)
            (memo x d out))])
  )

(defn rembero [x l out]
  (conde
   [(emptyo l) (== '() out)]
   [(firsto l x) (resto l out)]
   [(fresh (res)
           (fresh (d)
                  (resto l d)
                  (rembero x d res))
           (fresh (a)
                  (firsto l a)
                  (conso a res out)))]))

(run 1 [out]
     (fresh (y)
            (rembero 'peas (list 'a 'b y 'd 'peas 'e) out)))

(run 1 [out]
     (memo 'tofu (list 'a 'b 'tofu 'd 'tofu 'e) out))
;(comment
  (run* [q]
        (let [                          ;line (repeatedly 8 lvar)
              color \w
              oponent \b
              line-state (list 0 \w \w 0 \s 0 \b 0)
              constraints (reverse (reduce #(cons (cond (clojure.core/= %2 color) \m
                                                        (clojure.core/= %2 oponent) \o
                                                        :else %2) %) '() line-state))]
          (memo \s constraints q)))
)

(run* [q]
      (== q 1))

(reduce #(cons %2 %) '() '(1 2 3 4))

((fn [state pos] (get-type (vector pos (get state pos)))) init-board-state [0 1])

(display-board (into {} (map #(vector % "* ") (get-available-moves  init-board-state [0 0]))))
(get-available-moves  init-board-state [0 0])


(defn make-move [state from to]
  (let [piece (get state from)] (assoc (dissoc state from) to piece)))



;(defn valid-move? [state from to]
;)

(display-board (make-move init-board-state [1 1] [2 1]))


;(ns clj-cl-chess.core)
;(use 'clj-cl-chess.core :reload)