(ns tst.demo.core
  (:use tupelo.core
        tupelo.test)
  (:require
    [clojure.string :as str]
    )
  (:import [clojure.java.api Clojure]
           [com.sun.tools.javac.util Convert]))

(defn sep [& args]
  (newline) (println (first-or-nil args) "-----------------------------------------------------------------------------"))

(do
  ; (sep :m1-def)
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
    ; (sep :m1-run)
    (is= (m1-impl (quote hello))
      '(tst.demo.core/m1-fn (quote hello)))
    (is= (m1 bye) "bye")))


(do
  ; (sep :m2-def)
  (defn m2-fn
    [s1 s2]
    ; (spyx :m2-fn [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (str (sym->str s1) ", " (sym->str s2)))

  (defn m2-impl
    [s1 s2]
    ; (spyx :m2-impl [s1 s2])
    (assert (every? symbol? [s1 s2]))
    `(m2-fn '~s1 '~s2))

  (defmacro m2
    "Convert a symbol to a string"
    [s1 s2]
    ; (spyx :m2 [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (m2-impl s1 s2))

  (dotest
    ; (sep :m2-run)
    (is= (m2-impl 'one 'two)
      '(tst.demo.core/m2-fn (quote one) (quote two)))
    (is= (m2 hi bye)
      "hi, bye")))
    "
    :m2-def -----------------------------------------------------------------------------
    :m2 [s1 s2] => [hi bye]
    :m2-impl [s1 s2] => [hi bye]

    :m2-run -----------------------------------------------------------------------------
    :m2-impl [s1 s2] => [one two]
    (m2-impl (quote one) (quote two)) =>
    (tst.demo.core/m2-fn 'one 'two)
    :m2-fn [s1 s2] => [hi bye] "

(do
  ; (sep :m2b-def)
  (defn m2b-fn
    [s1 s2]
    ; (spyx :m2b-fn [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (str (sym->str s1) ", " (sym->str s2)))

  (defn m2b-impl
    [[s1 s2]]
    ; (spyx :m2b-impl [s1 s2])
    (assert (every? symbol? [s1 s2]))
    `(m2b-fn '~s1 '~s2))

  (defmacro m2b
    " Convert a symbol to a string"
    [& args]
    ; (spyx :m2b args)
    (assert (every? symbol? args))
    (m2b-impl args))

  (dotest
    ; (sep :m2b-run)
    (is= (m2b-impl '[one two])
      '(tst.demo.core/m2b-fn (quote one) (quote two)))
    (is= (m2b hi bye) "hi, bye")))
    "
    :m2b-def -----------------------------------------------------------------------------
    :m2b args => (hi bye)
    :m2b-impl [s1 s2] => [hi bye]

    :m2b-run -----------------------------------------------------------------------------
    :m2b-impl [s1 s2] => [one two]
    (m2b-impl (quote [one two])) =>
    (tst.demo.core/m2b-fn 'one 'two)
    :m2b-fn [s1 s2] => [hi bye] "

(do
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
    "
    :m2c-def -----------------------------------------------------------------------------
    :m2c args => (hi bye)
    :m2c-impl args => (hi bye)

    :m2c-run -----------------------------------------------------------------------------
    :m2c-impl args => [one two]
    :m2c-fn [s1 s2] => [hi bye] "

(do
  ; (sep :m2d-def)
  (defn m2d-fn
    [s1 s2]
    ; (spyx :m2d-fn [s1 s2])
    (assert (every? symbol? [s1 s2]))
    (str (sym->str s1) ", " (sym->str s2)))

  (defn m2d-impl
    [[a b]]
    (assert (every? symbol? [a b]))
    ; (spyx :m2d-impl [a b])
    `(m2d-fn '~a '~b))

  (defmacro m2d
    "Convert a symbol to a string"
    [& args]
    ; (spyx :m2d args)
    (assert (every? symbol? args))
    (m2d-impl args))

  (dotest
    ; (sep :m2d-run)
    (is= (m2d-impl '[one two])
      '(tst.demo.core/m2d-fn (quote one) (quote two)))
    (is= (m2d hi bye) "hi, bye")))
    "
    :m2d-def -----------------------------------------------------------------------------
    :m2d args => (hi bye)
    :m2d-impl [a b] => [hi bye]

    :m2d-run -----------------------------------------------------------------------------
    :m2d-impl [a b] => [one two]
    (m2d-impl (quote [one two])) => (tst.demo.core/m2d-fn (quote one) (quote two))
    :m2d-fn [s1 s2] => [hi bye] "

(do
  ; (sep :m9a-def)
  (defn m9a-fn
    [pat vals]
    ; (spyx :m9a-fn [pat vals])
    (str/join
      (interpose ", "
        (for [val vals]
          (str (sym->str pat) "+" (sym->str val))))))

  (defn m9a-impl
    [[pat & vals]]
    ; (spyx :m9a-impl [pat vals])
    `(m9a-fn (quote ~pat) (quote ~vals) ))

  (defmacro m9a
    "Convert a symbol to a string"
    [& args]
    ; (spyx :m9a args)
    (assert (every? symbol? args))
    (m9a-impl args))

  (dotest
    ; (sep :m9a-run)
    (is= (m9a-impl '[a b c d])
      '(tst.demo.core/m9a-fn (quote a) (quote (b c d))))
    (is= (m9a anda one two three) "anda+one, anda+two, anda+three")))
    "
    :m9a-def -----------------------------------------------------------------------------
    :m9a args => (anda one two three)
    :m9a-impl [pat vals] => [anda (one two three)]

    :m9a-run -----------------------------------------------------------------------------
    :m9a-impl [pat vals] => [a (b c d)]
    (m9a-impl (quote [a b c d])) => (tst.demo.core/m9a-fn (quote a) (quote (b c d)))
    :m9a-fn [pat vals] => [anda (one two three)] "

(do
  ; (sep :m9b-def)
  (defn m9b-fn
    [args]
    (let [[pat & vals] args]
      ; (spyx :m9b-fn [pat vals])
      (str/join
        (interpose ", "
          (for [val vals]
            (str (sym->str pat) "+" (sym->str val)))))))

  (defn m9b-impl
    [args]
    (assert (every? symbol? args))
    (let [[pat & vals] args]
      ; (spyx :m9b-impl [pat vals args])
      `(m9b-fn (quote ~args) )))

  (defmacro m9b
    "Convert a symbol to a string"
    [& args]
    ; (spyx :m9b args)
    (assert (every? symbol? args))
    (m9b-impl args))

  (dotest
    ; (sep :m9b-run)
    (is= (m9b-impl '[a b c d])
      '(tst.demo.core/m9b-fn (quote [a b c d])) )
    (is= (m9b anda one two three) "anda+one, anda+two, anda+three")))
    "
    :m9b-def -----------------------------------------------------------------------------
    :m9b args => (anda one two three)
    :m9b-impl [pat vals args] => [anda (one two three) (anda one two three)]

    :m9b-run -----------------------------------------------------------------------------
    :m9b-impl [pat vals args] => [a (b c d) [a b c d]]
    (m9b-impl (quote [a b c d])) => (tst.demo.core/m9b-fn (quote [a b c d]))
    :m9b-fn [pat vals] => [anda (one two three)] "


