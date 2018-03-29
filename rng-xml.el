;; rng-xml.el --- Parsing of RELAX NG schemas -*- lexical-binding:t -*-

;; Copyright (C) 2018 James Bostock

;; Author: James Bostock
;; Keywords: wp, hypermedia, languages, XML, RelaxNG

(require 'nxml-util)
(require 'rng-loc)
(require 'rng-parse)
(require 'rng-pttrn)
(require 'seq)

(defun rng-x-namespacep (attr)
  "Returns true if ATTR is a namespace attribute. An attribute is
represented as a dotted pair. If it is a namespace attribute, the
first element of the pair will, itself, be a dotted pair, the
first element of which will be the keyword
:http://www\\.w3\\.org/2000/xmlns/."
  (and (listp (car attr)) (eq (caar attr) :http://www\.w3\.org/2000/xmlns/)))

(defun rng-x-get-namespaces (attrs)
  "Return only the namespace attributes from the list of
attributes ATTRS."
  (seq-filter 'rng-x-namespacep attrs))

(defun rng-x-add-namespaces (nslist attrs)
  "Append the namespace attributes from ATTRS to NSLIST."
  (append (rng-x-get-namespaces attrs) nslist))

(defun rng-x-find-namespace (ns nslist)
  "If NS refers to a namespace in NSLIST, return the namespace URL."
  (let ((match (seq-filter (lambda (x) (equal (cdar x) ns)) nslist)))
    (if match
	(nxml-make-namespace (cdar match))
	match)))


(defconst rng-rng-namespace :http://relaxng.org/ns/structure/1.0
  "The RELAX NG namespace.")

(defun rng-tag-p (tag)
  "Is TAG in the RELAX NG namespace?"
    (and (consp tag)
	 (eq (car tag) rng-rng-namespace)))

(defun rng-attr-p (attr)
  "Is ATTR in the RELAX NG namespace?"
  (let ((name (car attr)))
    (if (consp name)
	(eq (car name) rng-rng-namespace)
      t)))

(defun rng-child-p (child)
  "Is CHILD in the RELAX NG namespace. CHILD can be either a
string or an element. If it is a string then it is in the RELAX
NG namespace. If it is an element, it is in the RELAX NG
namespace if its tag is in the RELAX NG namespace."
  (if (consp child)
      (rng-tag-p (car child))
    t))

(defun rng-remove-annotations (item)
  "Remove foreign attributes and elements.

If ITEM is not a cons, it is returned as is.

If ITEM is a cons, it is assumed to be an element in the RELAX
NG (i.e. it is assumed not to be a foreign element)."
  (if (consp item)
      (let ((tag (car item))
	    (attrs (seq-filter 'rng-attr-p (cadr item)))
	    (children (seq-map 'rng-remove-annotations (seq-filter 'rng-child-p (cddr item)))))
	(cons tag (cons attrs children)))
    item))

(defun rng-x-simplify (pttrn)
  "Simplify PTTRN according to the steps described in the RELAX
  NG specification."
  (rng-remove-annotations pttrn))

(defun rng-x-make-name (name nslist)
  "Make a name pattern from NAME."
  (let ((s (split-string name ":")))
    (if (eq (length s) 2)
	(rng-make-name (rng-x-find-namespace (car s) nslist) (cadr s))
      (rng-make-name nil name))))

(defun rng-x-body (body nslist)
  "If BODY has more than one element, create a group containing a
pattern for each element; otherwise just create a pattern.

TODO: handle choice patterns as well as groups?"
  (if (> (length body) 1)
      (rng-make-group (mapcar (lambda (x) (rng-x-main x nslist)) body))
      (rng-x-main (car body) nslist)))

(defun rng-x-attribute (attrs body nslist)
  (rng-make-attribute (rng-make-name-name-class (rng-x-make-name (cdr (assoc "name" attrs)) nslist))
		      (rng-x-body body nslist)))

(defun rng-x-choice (body nslist)
  (rng-make-choice (mapcar (lambda (x) (rng-x-main x nslist)) body)))

(defun rng-x-element (attrs body nslist)
  (rng-make-element (rng-make-name-name-class (rng-x-make-name (cdr (assoc "name" attrs)) nslist))
		    (rng-x-body body nslist)))

(defun rng-x-empty ()
  (rng-make-empty))

(defun rng-x-grammar ()
  ())

(defun rng-x-group (body nslist)
  (rng-make-group (mapcar (lambda (x) (rng-x-main x nslist)) body)))

(defun rng-x-one-or-more (body nslist)
  (rng-make-one-or-more (rng-x-body body nslist)))

(defun rng-x-optional (body nslist)
  (rng-make-optional (rng-x-main (car body) nslist)))

(defun rng-x-text ()
  (rng-make-text))

(defun rng-x-zero-or-more (body nslist)
  (rng-make-zero-or-more (rng-x-body body nslist)))

(defun rng-x-main (pttrn nslist)
    (let* ((elem  (car pttrn))
	   (attrs (cadr pttrn))
	   (nslist (rng-x-add-namespaces nslist attrs))
	   (body  (seq-filter (lambda (x) (listp x)) (cddr pttrn)))
	   (elem-name (cdr elem)))
      (cond
       ((string-equal elem-name "attribute") (rng-x-attribute attrs body nslist))
       ((string-equal elem-name "choice") (rng-x-choice body nslist))
       ((string-equal elem-name "element") (rng-x-element attrs body nslist))
       ((string-equal elem-name "empty") (rng-x-empty))
       ((string-equal elem-name "grammar") (rng-x-grammar attrs body nslist))
       ((string-equal elem-name "group") (rng-x-group body nslist))
       ((string-equal elem-name "oneOrMore") (rng-x-one-or-more body nslist))
       ((string-equal elem-name "optional") (rng-x-optional body nslist))
       ((string-equal elem-name "text") (rng-x-text))
       ((string-equal elem-name "zeroOrMore") (rng-x-zero-or-more body nslist))
       )))

(defun rng-x-parse-file (filename)
  (let ((parsed-file (rng-parse-validate-file (rng-load-schema (rng-locate-schema-file "RELAX NG")) filename)))
    (rng-x-main (rng-x-simplify parsed-file) nil)))

(provide 'rng-xml)
