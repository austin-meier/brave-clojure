(ns fwpd.functional)

(defn sum
  ([vals] (sum vals 0))
  ([vals accumulator]
   (if (empty? vals)
       accumulator
       (recur (rest vals) (+ (first vals) accumulator)))))

(sum '(1 2 3))
(sum [1 2 3])
(sum [0])


;; You can combine functions together with comp
;; It will apply the arguments to the last function and pass the result to the previous
(def character
  {:name "Evetsalker"
   :attributes {:intelligence 10
                :strength 7
                :defense 12}})

(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-def (comp :defense :attributes))

(c-int character)
(c-str character)
(c-def character)

(def spell-slots (comp int inc #(/ % 2) c-int))
(spell-slots character)

;; memoize
;; abuse referential transparency and store the arguments passed to a function along with the
;; result for future lookup to avoid computing it again
(defn sleepy-identity
  "Returns the given value after 1 second"
  [x]
  (Thread/sleep 1000)
  x)

;; This will always take one second to run
(sleepy-identity "Austin")

(def mem-sleepy-identify (memoize sleepy-identity))
;; This takes one second to run the first time, is instant after :D
(mem-sleepy-identify "Austin")
