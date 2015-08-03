(ns utilities.shortest-substrings)

(defn- get-coll-superstring [cand coll] 
  "Given a string and a collection of strings, return the first element of the
  collection that is a superstring of the candidate or, if there is no element
  with a superstring, nil."
  (first (drop-while #(or (= % cand) (not (.contains % cand))) coll)))

(defn- coll-has-substring? [cand coll]
  "Given a string and a collection of strings, return true if the collection
  contains a substring of the candidate string."
  (reduce #(if (.contains cand %2) true %) false coll))

(defn- swap-out-element [new-value old-value coll]
  "Given a collection of strings, remove old-value and insert new-value. If new
  value is already in the collection, return the collection with the old value 
  removed (if it was present)."
  (let [dropped-value-list (remove #(= old-value %) coll)]
    (if (not (some #{new-value} coll))
      (cons new-value dropped-value-list)
      dropped-value-list)))

(defn- handle-one-cand [cand coll]
  "Given a candidate string, carries out the following action:
  1. If the candidate is a substring of a collection element, replace the
     list element with the candidate;
  2. If the collection contains a substring of the candidate, do nothing;
  3. Otherwise, add the candidate."
  (let [coll-superstring (get-coll-superstring cand coll)]
    (if coll-superstring 
      (swap-out-element cand coll-superstring coll)
      (if-not (coll-has-substring? cand coll)
        (cons cand coll)
        coll))))

(defn shortest-substrings [coll]
  "Given a collection, returns the collection after removing all strings that
  are superstrings of other strings. Thus, [\"foo\" \"bar\" \"fo\"] returns 
  (\"fo\" \"bar\")."
  (reduce #(handle-one-cand %2 %) [] coll))