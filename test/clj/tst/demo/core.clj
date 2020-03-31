(ns tst.demo.core
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.string :as str]
    ))

(defn sep [& args]
  (newline)
  (println :-----------------------------------------------------------------------------)
  (let [lbl (first args)]
    (when lbl (println lbl \newline))) )

(comment
  (sep :m1)
  (defn m1-fn
    [arg]
    (assert (symbol? arg))
    (sym->str arg))

  (defn m1-impl
    [arg]
    (assert (symbol? arg))
    `(m1-fn (quote ~arg)))

  (defmacro m1
    " Convert a symbol to a string"
    [arg]
    (assert (symbol? arg))
    (m1-impl arg))

  (dotest
    (sep)
    (is= (m1-impl (quote hello))
      '(tst.demo.core/m1-fn (quote hello)))
    (is= (m1 bye) "bye")))


(do
  (sep :m2-def)
  (defn m2-fn
    [s1 s2]
    (spyx :m2-fn [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (str (sym->str s1) ", " (sym->str s2)))

  (defn m2-impl
    [s1 s2]
    (spyx :m2-impl [s1 s2])
    (assert (every? symbol? [s1 s2]))
    `(m2-fn '~s1 '~s2))

  (defmacro m2
    " Convert a symbol to a string"
    [s1 s2]
    (spyx :m2 [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (m2-impl s1 s2))

  (dotest
    (sep :m2-run)
    (is= (spyx-pretty (m2-impl 'one 'two))
      '(tst.demo.core/m2-fn (quote one) (quote two)))
    (is= (spyx (m2 hi bye))
      "hi, bye")))

(comment
  (sep)

  (defn mb-fn
  [a b]
  (str/join (interpose ", " (mapv sym->str [a b]))))

(defn mb-impl
  [args]
  `(let [[a# b#] ~args]
     (mb-fn a# b#)))

(defmacro mb
  " Convert a symbol to a string"
  [& args]
  (spyx args)
  (mb-impl args))

(dotest
  (println \newline :----------------------------------------------------------------------------- )
  (spyx-pretty (mb-impl '[one two]))
  ;  (spyx (mb one two))
  )

  )
