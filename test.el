#!/bin/sh
":"; exec emacs -batch -L . -l ert -l test.el -f ert-run-tests-batch-and-exit # -*- mode: emacs-lisp; -*-

(require 'rng-cmpct)
(require 'rng-xml)

(ert-deftest rng-x-test-tut-01-01 ()
  (should (equal (rng-c-parse-file "tut01-01.rnc")
		 (rng-x-parse-file "tut01-01.rng"))))

(ert-deftest rng-x-test-tut-01-02 ()
  (should (equal (rng-c-parse-file "tut01-02.rnc")
		 (rng-x-parse-file "tut01-02.rng"))))

(ert-deftest rng-x-test-tut-01-03 ()
  (should (equal (rng-c-parse-file "tut01-03.rnc")
		 (rng-x-parse-file "tut01-03.rng"))))

(ert-deftest rng-x-test-tut-01-04 ()
  (should (equal (rng-c-parse-file "tut01-04.rnc")
		 (rng-x-parse-file "tut01-04.rng"))))

(ert-deftest rng-x-test-tut-02-01 ()
  (should (equal (rng-c-parse-file "tut02-01.rnc")
		 (rng-x-parse-file "tut02-01.rng"))))

(ert-deftest rng-x-test-tut-03-01 ()
  (should (equal (rng-c-parse-file "tut03-01.rnc")
		 (rng-x-parse-file "tut03-01.rng"))))

(ert-deftest rng-x-test-tut-03-02 ()
  (should (equal (rng-c-parse-file "tut03-02.rnc")
		 (rng-x-parse-file "tut03-02.rng"))))

(ert-deftest rng-x-test-tut-03-03 ()
  (should (equal (rng-c-parse-file "tut03-03.rnc")
		 (rng-x-parse-file "tut03-03.rng"))))

(ert-deftest rng-x-test-tut-03-04 ()
  (should (equal (rng-c-parse-file "tut03-04.rnc")
		 (rng-x-parse-file "tut03-04.rng"))))

(ert-deftest rng-x-test-tut-03-05 ()
  (should (equal (rng-c-parse-file "tut03-05.rnc")
		 (rng-x-parse-file "tut03-05.rng"))))


(ert-deftest rng-x-test-tut-04-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut04-01.rnc")
		 (rng-x-parse-file "tut04-01.rng"))))

(ert-deftest rng-x-test-tut-04-02 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut04-02.rnc")
		 (rng-x-parse-file "tut04-02.rng"))))

(ert-deftest rng-x-test-tut-04-03 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut04-03.rnc")
		 (rng-x-parse-file "tut04-03.rng"))))

(ert-deftest rng-x-test-tut-05-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut05-01.rnc")
		 (rng-x-parse-file "tut05-01.rng"))))

(ert-deftest rng-x-test-tut-05-02 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut05-02.rnc")
		 (rng-x-parse-file "tut05-02.rng"))))

(ert-deftest rng-x-test-tut-06-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut06-01.rnc")
		 (rng-x-parse-file "tut06-01.rng"))))

(ert-deftest rng-x-test-tut-07-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut07-01.rnc")
		 (rng-x-parse-file "tut07-01.rng"))))

(ert-deftest rng-x-test-tut-07-02 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut07-01.rnc")
		 (rng-x-parse-file "tut07-02.rng"))))

(ert-deftest rng-x-test-tut-07-03 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut07-03.rnc")
		 (rng-x-parse-file "tut07-03.rng"))))

(ert-deftest rng-x-test-tut-08-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut08-01.rnc")
		 (rng-x-parse-file "tut08-01.rng"))))

(ert-deftest rng-x-test-tut-09-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut09-01.rnc")
		 (rng-x-parse-file "tut09-01.rng"))))

(ert-deftest rng-x-test-tut-09-02 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut09-02.rnc")
		 (rng-x-parse-file "tut09-02.rng"))))

(ert-deftest rng-x-test-tut-09-03 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut09-03.rnc")
		 (rng-x-parse-file "tut09-03.rng"))))

(ert-deftest rng-x-test-tut-09-04 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut09-04.rnc")
		 (rng-x-parse-file "tut09-04.rng"))))

(ert-deftest rng-x-test-tut-09-05 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut09-05.rnc")
		 (rng-x-parse-file "tut09-05.rng"))))

(ert-deftest rng-x-test-tut-09-06 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut09-06.rnc")
		 (rng-x-parse-file "tut09-06.rng"))))

(ert-deftest rng-x-test-tut-10-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut10-01.rnc")
		 (rng-x-parse-file "tut10-01.rng"))))

(ert-deftest rng-x-test-tut-10-02 ()
  (should (equal (rng-c-parse-file "tut10-02.rnc")
		 (rng-x-parse-file "tut10-02.rng"))))

(ert-deftest rng-x-test-tut-10-03 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut10-03.rnc")
		 (rng-x-parse-file "tut10-03.rng"))))

(ert-deftest rng-x-test-tut-10-04 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut10-04.rnc")
		 (rng-x-parse-file "tut10-04.rng"))))

(ert-deftest rng-x-test-tut-11-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut11-01.rnc")
		 (rng-x-parse-file "tut11-01.rng"))))

(ert-deftest rng-x-test-tut-11-02 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut11-02.rnc")
		 (rng-x-parse-file "tut11-02.rng"))))

(ert-deftest rng-x-test-tut-11-03 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut11-03.rnc")
		 (rng-x-parse-file "tut11-03.rng"))))

(ert-deftest rng-x-test-tut-12-01 ()
  (should (equal (rng-c-parse-file "tut12-01.rnc")
		 (rng-x-parse-file "tut12-01.rng"))))

(ert-deftest rng-x-test-tut-12-02 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut12-02.rnc")
		 (rng-x-parse-file "tut12-02.rng"))))

(ert-deftest rng-x-test-tut-13-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut13-01.rnc")
		 (rng-x-parse-file "tut13-01.rng"))))

(ert-deftest rng-x-test-tut-14-01 ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "tut14-01.rnc")
		 (rng-x-parse-file "tut14-01.rng"))))


(ert-deftest rng-x-test-whitespace ()
  :expected-result :failed
  (should (equal (rng-c-parse-file "whitespace.rnc")
		 (rng-x-parse-file "whitespace.rng"))))
