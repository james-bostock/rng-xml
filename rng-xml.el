;; rng-xml.el --- Parsing of RELAX NG schemas -*- lexical-binding:t -*-

;; Copyright (C) 2018 James Bostock

;; Author: James Bostock
;; Keywords: wp, hypermedia, languages, XML, RelaxNG

(require 'nxml-util)
(require 'rng-loc)
(require 'rng-parse)
(require 'rng-pttrn)
(require 'seq)

(defconst rng-x-rng-namespace-url
  (nxml-make-namespace "http://relaxng.org/ns/structure/1.0")
  "The RELAX NG namespace URL.")

(defun rng-x--tag-p (tag)
  "Is TAG in the RELAX NG namespace?"
    (and (consp tag)
	 (eq (car tag) rng-x-rng-namespace-url)))

(defun rng-x--attr-p (attr)
  "Is ATTR in the RELAX NG namespace?"
  (let ((name (car attr)))
    (if (consp name)
	(eq (car name) rng-x-rng-namespace-url)
      t)))

(defun rng-x--child-p (child)
  "Is CHILD in the RELAX NG namespace. CHILD can be either a
string or an element. If it is a string then it is in the RELAX
NG namespace. If it is an element, it is in the RELAX NG
namespace if its tag is in the RELAX NG namespace."
  (if (consp child)
      (rng-x--tag-p (car child))
    t))

(defun rng-x--remove-annotations (item)
  "Remove foreign attributes and elements.

If ITEM is not a cons, it is returned as is.

If ITEM is a cons, it is assumed to be an element in the RELAX
NG (i.e. it is assumed not to be a foreign element)."
  (if (consp item)
      (let ((tag (car item))
	    (attrs (seq-filter #'rng-x--attr-p (cadr item)))
	    (children (seq-map #'rng-x--remove-annotations (seq-filter #'rng-x--child-p (cddr item)))))
	(cons tag (cons attrs children)))
    item))

(defun rng-x--simplify (pttrn)
  "Simplify PTTRN according to the steps described in the RELAX
  NG specification."
  (rng-x--remove-annotations pttrn))

(defun rng-x--make-name (name)
  "Make a name pattern from NAME."
  (rng-make-name nil name))

(defun rng-x--body (body)
  "If BODY has more than one element, create a group containing a
pattern for each element; otherwise just create a pattern.

TODO: handle choice patterns as well as groups?"
  (if (> (length body) 1)
      (rng-make-group (mapcar (lambda (x) (rng-x--main x)) body))
      (rng-x--main (car body))))

(defun rng-x--attribute (attrs body)
  (rng-make-attribute (rng-make-name-name-class (rng-x--make-name (cdr (assoc "name" attrs))))
		      (rng-x--body body)))

(defun rng-x--choice (body)
  (rng-make-choice (mapcar (lambda (x) (rng-x--main x)) body)))

(defun rng-x--element (attrs body)
  (rng-make-element (rng-make-name-name-class (rng-x--make-name (cdr (assoc "name" attrs))))
		    (rng-x--body body)))

(defun rng-x--empty ()
  (rng-make-empty))

(defun rng-x--grammar ()
  ())

(defun rng-x--group (body)
  (rng-make-group (mapcar (lambda (x) (rng-x--main x)) body)))

(defun rng-x--one-or-more (body)
  (rng-make-one-or-more (rng-x--body body)))

(defun rng-x--optional (body)
  (rng-make-optional (rng-x--main (car body))))

(defun rng-x--text ()
  (rng-make-text))

(defun rng-x--zero-or-more (body)
  (rng-make-zero-or-more (rng-x--body body)))

(defun rng-x--main (pttrn)
    (let* ((elem  (car pttrn))
	   (attrs (cadr pttrn))
	   (body  (seq-filter (lambda (x) (listp x)) (cddr pttrn)))
	   (elem-name (cdr elem)))
      (cond
       ((string-equal elem-name "attribute") (rng-x--attribute attrs body))
       ((string-equal elem-name "choice") (rng-x--choice body))
       ((string-equal elem-name "element") (rng-x--element attrs body))
       ((string-equal elem-name "empty") (rng-x--empty))
       ((string-equal elem-name "grammar") (rng-x--grammar attrs body))
       ((string-equal elem-name "group") (rng-x--group body))
       ((string-equal elem-name "oneOrMore") (rng-x--one-or-more body))
       ((string-equal elem-name "optional") (rng-x--optional body))
       ((string-equal elem-name "text") (rng-x--text))
       ((string-equal elem-name "zeroOrMore") (rng-x--zero-or-more body))
       )))

(defun rng-x-parse-file (filename)
  (let ((parsed-file (rng-parse-validate-file (rng-load-schema (rng-locate-schema-file "RELAX NG")) filename)))
    (rng-x--main (rng-x--simplify parsed-file))))

(provide 'rng-xml)
