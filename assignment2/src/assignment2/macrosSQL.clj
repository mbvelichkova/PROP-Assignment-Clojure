(ns assignment2)


(defn eval_operator
	"Change the infix to prefix form ( [:column op value] -> [op :column value] ).
	Then get the :column value from the map m and evaluate the expression (op m[:column] value)."
	[form m]
	(eval (cons (second form) (cons (m (first form)) (nnext form)))))


(defmacro select
	"(select [columns]
    	from #{table}
		where [:column op value]
		orderby :column)"
	[columns _ table _ operator _ column]
	`(map (fn [n#] (select-keys n# ~columns))
		(filter (fn [n#] (eval_operator ~operator n#))
			(sort-by ~column ~table)
		)
	)
)


