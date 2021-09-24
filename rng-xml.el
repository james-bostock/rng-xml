;; rng-xml.el --- Parsing of RELAX NG schemas -*- lexical-binding:t -*-

;; Copyright (C) 2018, 2021 James Bostock

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

(defconst rng-x-xml-namespace-url
  (nxml-make-namespace "http://www.w3.org/XML/1998/namespace")
  "The XML namespace URL.")

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

;; Surprising that Emacs lisp doesn't provide these or something
;; similar. Maybe I didn't look in the right place(s).
(defun rng-x--ltrim (string)
  "Remove leading space from STRING."
  (replace-regexp-in-string "^[[:space:]]+" "" string))

(defun rng-x--rtrim (string)
  "Remove trailing space from STRING."
  (replace-regexp-in-string "[[:space:]]+$" "" string))

(defun rng-x--trim (string)
  "Remove leading and trailing space from STRING"
  (rng-x--ltrim (rng-x--rtrim string)))

(defun rng-x--child-whitespace-filter (child)
  "Returns true if CHILD is a not string consisting entirely of
whitespace."
  (not (and (stringp child)
	    (string-match "^[[:space:]]*$" child))))

(defun rng-x--remove-attr-whitespace (attr)
  "If ATTR is name, type or combine attribute, remove leading and
  trailing whitespace from its value."
  (let* ((name (car attr))
	 (value (cdr attr))
	 (new-value (if (or (string= name "combine")
			    (string= name "name")
			    (string= name "type"))
			(rng-x--trim value)
		      value)))
    (cons name new-value)))


(defun rng-x--new-children (name children)
   (cond ((or (string= name "param")
	     (string= name "value"))
	 children)
	((string= name "name")
	 (mapcar #'rng-x--trim children))
	(t
	 (mapcar #'rng-x--remove-whitespace (seq-filter #'rng-x--child-whitespace-filter
				 children)))))

(defun rng-x--remove-whitespace (elem)
  "If ELEM is not a value or a param element, remove any child
elements that are strings containing only whitespace.

Remove leading and trailing whitespace from the value of each
name, type and combine attribute and from the content of each
name element."
  (let* ((tag (car elem))
	 (name (cdr tag))
	 (attrs (mapcar #'rng-x--remove-attr-whitespace (cadr elem)))
	 (children (cddr elem))
	 (new-children (cond ((or (string= name "param")
				  (string= name "value"))
			      children)
			     ((string= name "name")
			      (mapcar #'rng-x--trim children))
			     (t
			      (mapcar #'rng-x--remove-whitespace
				      (seq-filter #'rng-x--child-whitespace-filter
						  children))))))
    (cons tag (cons attrs new-children))))

(defun rng-x--datatype-library-attr (item dtns)
  "Transform ITEM so that, if it is a data or value element, it
has a datatypeLibrary attribute (with the value DTNS, if it does
not already have such an attribute); and, if it is not (a data or
value element), that it does not have a datatypeLibrary
attribute."
  (if (consp item)
      (let* ((tag (car item))
	     (data_or_value (or (equal (cdr tag) "data")
				(equal (cdr tag) "value")))
	     (attrs (cadr item))
	     (thistdns (assoc "datatypeLibrary" attrs))
	     (newdtns (if thistdns (cdr thistdns) dtns))
	     (newattrs (cond ((and data_or_value (not thistdns))
			      (append attrs
				      (list
				       (cons "datatypeLibrary" newdtns))))
			     ((and (not data_or_value) thistdns)
			      (seq-filter (lambda (x)
					    (not
					     (equal
					      (car x) "datatypeLibrary")))
					  attrs))
			     (t attrs)))
	     (children (mapcar
			(lambda (arg)
			  (rng-x--datatype-library-attr arg newdtns))
			(cddr item))))
	(cons tag (cons newattrs children)))
    item))

(defun rng-x--simplify (pttrn)
  "Simplify PTTRN according to the steps described in the RELAX
  NG specification."
  (rng-x--datatype-library-attr
   (rng-x--remove-whitespace
    (rng-x--remove-annotations pttrn)) ""))

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

(defun rng-x--data (attrs body)
  (rng-make-data (cons (intern (cdr (assoc "datatypeLibrary" attrs)))
                       (intern (cdr (assoc "type" attrs))))
                 (mapcar (lambda (param)
                           (cons (intern (cdr (assoc "name" (cadr param))))
                                 (caddr param))) body)))

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

(defun rng-x--value (attrs body)
  (rng-make-value (cons (intern (cdr (assoc "datatypeLibrary" attrs)))
                        (intern (cdr (assoc "type" attrs))))
                  (car body)
                  ;; For now, we hard code the context
                  `(nil ,(cons "xml" rng-x-xml-namespace-url))))

(defun rng-x--zero-or-more (body)
  (rng-make-zero-or-more (rng-x--body body)))

(defun rng-x--main (pttrn)
    (let* ((elem  (car pttrn))
	   (attrs (cadr pttrn))
	   (body (cddr pttrn))
	   (elem-name (cdr elem)))
      (cond
       ((string-equal elem-name "attribute") (rng-x--attribute attrs body))
       ((string-equal elem-name "choice") (rng-x--choice body))
       ((string-equal elem-name "data") (rng-x--data attrs body))
       ((string-equal elem-name "element") (rng-x--element attrs body))
       ((string-equal elem-name "empty") (rng-x--empty))
       ((string-equal elem-name "grammar") (rng-x--grammar attrs body))
       ((string-equal elem-name "group") (rng-x--group body))
       ((string-equal elem-name "oneOrMore") (rng-x--one-or-more body))
       ((string-equal elem-name "optional") (rng-x--optional body))
       ((string-equal elem-name "text") (rng-x--text))
       ((string-equal elem-name "value") (rng-x--value attrs body))
       ((string-equal elem-name "zeroOrMore") (rng-x--zero-or-more body))
       )))

(defun rng-x-parse-file (filename)
  (let ((parsed-file (rng-parse-validate-file (rng-load-schema (rng-locate-schema-file "RELAX NG")) filename)))
    (rng-x--main (rng-x--simplify parsed-file))))

(provide 'rng-xml)
