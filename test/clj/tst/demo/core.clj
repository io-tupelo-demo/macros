(ns tst.demo.core
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.string :as str]
    ))

(defn sep [& args]
  (newline) (println (first-or-nil args) "-----------------------------------------------------------------------------"))

(comment
  (sep :m1-def)
  (defn m1-fn
    [arg]
    (assert (symbol? arg))
    (sym->str arg))

  (defn m1-impl
    [arg]
    (assert (symbol? arg))
    `(m1-fn (quote ~arg)))

  (defmacro m1
    "Convert a symbol to a string"
    [arg]
    (assert (symbol? arg))
    (m1-impl arg))

  (dotest
    (sep :m1-run)
    (is= (m1-impl (quote hello))
      '(tst.demo.core/m1-fn (quote hello)))
    (is= (m1 bye) "bye")))


(comment  ; do
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
    "Convert a symbol to a string"
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

(comment  ; do
  (sep :m2b-def)
  (defn m2b-fn
    [s1 s2]
    (spyx :m2b-fn [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (str (sym->str s1) ", " (sym->str s2)))

  (defn m2b-impl
    [[s1 s2]]
    (spyx :m2b-impl [s1 s2])
    (assert (every? symbol? [s1 s2]))
    `(m2b-fn '~s1 '~s2))

  (defmacro m2b
    "Convert a symbol to a string"
    [& args]
    (spyx :m2b args)
    (assert (every? symbol? args))
    (m2b-impl args))

  (dotest
    (sep :m2b-run)
    (is= (spyx-pretty (m2b-impl '[one two]))
      '(tst.demo.core/m2b-fn (quote one) (quote two)))
    (is= (spyx (m2b hi bye)) "hi, bye")))

(comment  ; do
  ; (sep :m2c-def)
  (defn m2c-fn
    [s1 s2]
    ; (spyx :m2c-fn [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (str (sym->str s1) ", " (sym->str s2)))

  (defn m2c-impl
    [args]
    ; (spyx :m2c-impl args)
    (assert (every? symbol? args))
    `(apply m2c-fn (quote ~args)))

  (defmacro m2c
    "Convert a symbol to a string"
    [& args]
    ; (spyx :m2c args)
    (assert (every? symbol? args))
    (m2c-impl args))

  (dotest
    ; (sep :m2c-run)
    (is= (m2c-impl '[one two])
      '(clojure.core/apply tst.demo.core/m2c-fn '[one two]))
    (is= (m2c hi bye) "hi, bye")))

(do
  (sep :m2d-def)
  (defn m2d-fn
    [s1 s2]
    (spyx :m2d-fn [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (str (sym->str s1) ", " (sym->str s2)))

  (defn m2d-impl
    ; [[a b] args] ; ***** cannot destructure here - crashes ****
    [args]
    (let [[a b] args] ; destructure here works normally
      (spyx :m2d-impl [args a b])
      (assert (every? symbol? args))
      `(m2d-fn '~a '~b)))

  (defmacro m2d
    "Convert a symbol to a string"
    [& args]
    (spyx :m2d args)
    (assert (every? symbol? args))
    (m2d-impl args))

  (dotest
    (sep :m2d-run)
    (is= (spyx (m2d-impl '[one two]))
      '(tst.demo.core/m2d-fn (quote one) (quote two)))
    (is= (m2d hi bye) "hi, bye")))

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
  "Convert a symbol to a string"
  [& args]
  (spyx args)
  (mb-impl args))

(dotest
  (println \newline :----------------------------------------------------------------------------- )
  (spyx-pretty (mb-impl '[one two]))
  ;  (spyx (mb one two))
  )

  )
