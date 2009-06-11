(defpackage psos-test
  (:use :parenscript :paren-psos :paren-test))

(in-package :psos-test)

(defsuite :psos-test)
(in-suite :psos-test)

(deftest five-is-five ()
  (is (== 5 5)))
