(ns fwpd.core)
(def filename "suspects.csv")

(slurp filename)

(def vamp-keys [:name :glitter-index])

(defn str->int [str]
  (Integer. str))

(def conversions {:name identity :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(convert :glitter-index "3")

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(parse (slurp filename))

;; WORK ON, passing map to multiple sequences, it will take the value at the index of each
;; sequence and pass them as parameters to the mapped function
(map vector vamp-keys ["Austin" "10"])
;; Maybe this is how the `zip` style functions work??
(map vector [:key1 :key2 :key3] ["val1" "val2" "val3"])

(defn mapify
  "Return a seq of maps"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(first (mapify (parse (slurp filename))))


(def suspect-map
  (->> (slurp filename)
       (parse)
       (mapify)))

(doall suspect-map)

(defn glitter-filter
  [minimum-glitter records]
  (filter #(> (:glitter-index %) minimum-glitter) records))

;; Using filter to get the name key
(defn glitter-filter2
  [minimum-glitter records]
  (->> records
       (filter #(> (:glitter-index %) minimum-glitter))
       (reduce (fn [seq record]
                 (conj seq (:name record)))
               '())))

;; Using map to get the name key
(defn glitter-filter3
  [minimum-glitter records]
  (->> records
       (glitter-filter minimum-glitter)
       (map #(:name %))))


(->> (slurp filename)
     (parse)
     (mapify)
     (glitter-filter3 3))

(defn add-suspect
  [suspects name glitter-index]
  (assoc suspects :name name :glitter-index glitter-index))

(add-suspect (mapify (parse (slurp filename))) "Austin" "10")

;; Check if suspect is valid
(defn valid-suspect?
  [name glitter-index]
  (if (and (some? name) (some? glitter-index))
    true
    false))


;;(def valid-suspect {:name contentful-string :glitter-index contentful-string})

(defn valid-suspect?
  [validator suspect]
  (->> (vec suspect)
       (map (fn [[key val]] ((get validator key) val)))
       (every? true?)))

(vec {:name "Austin" :glitter-index "10"})

(valid-suspect? valid-suspect {:name "Austin" :glitter-index "10"})

(defn to-csv [suspects]
  (map #(clojure.string/join "," (vals %)) suspects))

;; Open the CSV file, parse to map, then write back to CSV
(->> (slurp filename)
     (parse)
     (mapify)
     (to-csv)
     (map #(str % "\n"))
     (clojure.string/join "")
     (spit filename))



;; Add a suspect to the file
(defn add-suspect-file
  [name glitter-index]
  (if (valid-suspect? name glitter-index)
    (spit filename (str name "," glitter-index "\n") :append true)))

(add-suspect-file "Michael" "22")

;; doall will realize a lazy list. You can also `apply list` or `vec` the lazy list
(def sus (mapify (parse (slurp filename))))
(doall sus)

(defn contentful-string
  [string]
  (and (string? string) (some? string?)))

(def valid-person {:name contentful-string :age contentful-string})

(defn valid-map?
  [validator items]
  (->> (vec items)
       (map (fn [[key val]] ((get validator key) val)))
       (every? true?)))

(valid-map? valid-person {:name "Austin" :age "23"})
