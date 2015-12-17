(ns assignment2)


;(select [columns]
; from #{table}
; where [:column op value]
; orderby :column)

(defn orderby 
	[column coll]
	(sort-by column coll))


;where
(defn infix
	[form n]
	(cons (second form) (cons (n (first form)) (nnext form))))

(defn where
	[operator coll]
	 (filter (fn [n] (eval (infix operator n))) coll))

(defn select 
	[columns coll]

 (map (fn [n] (select-keys n columns)) coll))

(defn selectSQL
	[columns operator column coll]
(map (fn [n] (select-keys n columns))
	(filter (fn [n] (eval (infix operator n)))
		(sort-by column coll)
	)
))

(defmacro selectSQL
	[columns _ table _ operator _ column]
	`(map (fn [n#] (select-keys n# ~columns))
		(filter (fn [n#] (eval (infix ~operator n#)))
			(sort-by ~column ~table)
		)
	)
)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def persons `({:id 1 :name "olle"} {:id 2 :name "anna"} {:id 3 :name
"isak"} {:id 4 :name "beatrice"}))

