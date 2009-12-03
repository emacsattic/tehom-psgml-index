;;; tehom-psgml-index.el --- Create table-of-contents for html pages.

;; Copyright (C) 1999 by Tom Breton

;; Author: Tom Breton <Tehom@localhost>
;; Keywords: hypermedia, extensions
;; Version: 1.0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code is intended to make it easier to create and maintain
;; tables of contents of html files.  It's potentially extendable to
;; other SGML and XML files.

;; It's fairly slow, because it does a lot of redundant parsing.
;; However, that was neccessary in order to edit the buffer and keep
;; elements' positions meaningful in a simple way.

;;; Usage:

;; Load this file.  Go to the place you want the table of contents to
;; be inserted and call tehom-psgml-do-indexing.  Now follow the
;; prompts.

;; Design notes:

;; This code assumes that the structure of a document is indicated as
;; absolute levels (H1, H2, etc in HTML), and not relative levels.  It
;; also assumes that these indications of level occur at a specific
;; level in the document, eg, body level in HTML.  

;; Using random was a quick-and-dirty way of creating unique anchor
;; IDs.  It will practically never fail, but purists may want a
;; "clean" solution, which would involve checking each newly created
;; ID against every existing ID in the document.  Personally I'm
;; satisfied with a chance of duplication on the order of 1 in 26^10
;; per ID pair, but code it if you want it.

;; To find which elements to index, we grab a list of elements But not
;; neccessarily all the elements.  We start from a path, such as
;; ("HTML" "BODY"), that includes only the section that should be
;; indexed.  Then we determine which ones are indexable, and then
;; which ones are subordinate to which others.

;; In order to deal with changes in the buffer, we mostly represent
;; elements as numberpaths, which are lists of numbers representing
;; each successive branch to take to reach the element.  If we stored
;; elements as such, their positions in the buffer would become
;; misleading every time we edited an element earlier in the buffer.
;; This *would not* be sufficient if we inserted or deleted nodes in
;; such a way that any path's meaning changed.  Fortunately we don't.

;; The stages of getting each element's appropriate info and building
;; the controller could have been two separate stages.  Perhaps that
;; would have been clearer, but it's written now and changing it would
;; mostly just introduce bugs.  We could treeify only after preparing
;; each element, and store the element's contents and retrieve them
;; when building the controller.

;; el-spec-list format:

;;Each el-spec is (NUMBERPATH [SUB-ELEMENT-EL-SPEC-LIST [DATA...]]),
;;where SUB-ELEMENT-EL-SPEC-LIST is a list of zero or more elements
;;the same format.  Examples:

;;((el)) is a single element

;;((el) (el1) (el2)) are 3 elements at the same level

;;((el ((sub-el1) (sub-el2)))) is an element with 2 subordinate
;;elements.

;;((el ((sub-el1) (sub-el2)) some-data)) is an element with 2
;;subordinate elements and some other data indicating how to find its
;;anchor or something.  Reserved, not used.


;;; Further possibilities, which I don't plan to code:

;; We could make anouther entry point that finds a "good place to
;; insert" instead of using point-marker or being explicitly told
;; where to insert.  It would prolly just use a path from the top of
;; the document.

;; The various parameter variables could be more flexible, eg to adapt
;; to various dtds.  But it isn't clear how this should work.  Perhaps
;; an alist by doctype name?  NB, source and target could be different
;; dtds.

;; We could handle recursive relative outline levels as well as
;; absolute levels.  The elements of tehom-psgml-outline-alist would
;; need to indicate this.  I've reserved the symbol t to indicate a
;; recursive structure element.  More fine-grained indications would
;; prolly have to be defined.

;; We could let it find or build a "Contents" heading.  This would be
;; convenient within the same document, but extra work when indexing a
;; second document.  It would be done last so it wouldn't mess up the
;; numberpaths.  But it's so easy to just build one by hand that
;; there's little payoff.

;; We could allow smarter ways of finding a node's existing
;; heading-text and anchor-id, if any.  The elements of
;; tehom-psgml-outline-alist would prolly indicate this as data.

;; We could allow indexing only the portions under an indicated node.
;; This could be done by post-processing the list of structure
;; elements after it's collected.  The element most closely before the
;; point would be found, and only it and its subordinate nodes would
;; be indexed.  It's probably not worth it because few pages are so
;; big that they need more than one index.  A page that big should
;; prolly be split instead, so there's little payoff.

;;; Prerequisites:

;; psgml, Lennart Staflin's SGML/XML package for emacs.

;; cl, Dave Gillespie's Common Lisp-alike in emacs.

;; Version 1.3 or better of tehom-psgml.  Version 1.2 *will not* be
;; sufficient.  The changes are just too large, because they were
;; mostly made to help this package.

;; rtest.el is needed for the test suite, but the package will run
;; without it.

;;; Code:

(require 'cl)
(require 'psgml)
(require 'tehom-psgml)

;;;;;;;;;;;;;;;;;;;
;;Parameters.

;;Parameters for recognition

(defconst tehom-psgml-outline-path '("BODY") 
  "Path from top level to the level of the document where outline
structure begins." ) 

(defconst tehom-psgml-anchor-path '("A") 
  "Path from a heading element to its anchor element, if any." )

(defconst tehom-psgml-anchor-attribute "NAME"
  "Attribute of the anchor element, if any, that is the ID." )

(defconst tehom-psgml-outline-alist
  '( 
     ("H1" . 1)
     ("H2" . 2)
     ("H3" . 3)
     ("H4" . 4)
     ("H5" . 5)
     ("H6" . 6))
  
  "Elements to recognize as indicating the structure of an outline.

Each element is in the form \(name . indicator \).  A number as an
indicator indicates an absolute outline level, ie, one that's the same
regardless of its context.

The indicator t is reserved to mean: This element indicates recursive
relative outlining, rather than absolute outlining as in HTML.  It is
not supported in this version.")


;;Parameters for insertion:

(defconst tehom-psgml-index-whole-list-structure 
  '`("OL" (sub-nodes ,@entries ))
  "The format controller of an index list as a whole.
,@ENTRIES places the list of individual entries." )


(defconst tehom-psgml-index-entry-structure
  '`("LI"  
      (sub-nodes 
	("A"  
	  ("HREF" . ,anchor-id)
	  (sub-nodes
	    ("#PCDATA" ,heading-text)))
	,sublist))
  "The format controller of an index entry.
,ANCHOR-ID places the id of the anchor being indexed.
,HEADING-TEXT places the visible text.
,SUBLIST places all the subordinate entries." )

(defconst tehom-psgml-index-new-anchor-structure 
  '`("A" 
      ("NAME" . ,anchor-id)
      (sub-nodes
	("#PCDATA" ,heading-text)))
  "The format controller of a new anchor entry.
,ANCHOR-ID places the id of the anchor being indexed.
,HEADING-TEXT places the visible text." )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Utility functions

;;;###autoload
(defun tehom-make-random-string (&optional len)
  ""
  (setq len (or len 10))

  (loop
    repeat len
    concat 
    (format "%c" (+ ?A (random* 26)))))


;;;;;;;;;;;;;
;;Deal with anchors.

(defun tehom-psgml-set-anchor-id (el anchor-id)
  "Set EL's name attribute to ANCHOR-ID.

The actual attribute set is determined by
tehom-psgml-anchor-attribute."
  
  (let* 
    ( 
      (asl 
	(cons 
	  (sgml-make-attspec tehom-psgml-anchor-attribute anchor-id)
	  (sgml-element-attribute-specification-list el))))
	  
    (sgml-change-start-tag el asl)))


(defun tehom-psgml-index-get-anchor-info (numberpath)
  "Return a cons of the indicated element's \( id . text \).

If it has no anchor, or has an anchor with no id, fix it.
NUMBERPATH is a numberpath which indicates the element."
  
  (let*
    (
      ;;Find the anchor.  This is quick and dirty, based on the
      ;;assumption that headings will rarely have any markup besides
      ;;anchors.
      (anchor-el 
	(tehom-psgml-find-element-by-path 
	  tehom-psgml-anchor-path 
	  (tehom-psgml-find-element-by-numberpath numberpath)))
      heading-text
      anchor-id)
    
    (if anchor-el
      (progn
	(setq anchor-id
	  (sgml-element-attval anchor-el tehom-psgml-anchor-attribute))

	(setq
	  heading-text
	  (tehom-psgml-index-get-el-contents anchor-el))
	
	;;If the anchor exists but has no name attribute, give it one.
	(if (null anchor-id)
	  (progn
	    (setq anchor-id (tehom-make-random-string))
	    (tehom-psgml-set-anchor-id anchor-el anchor-id)))

	;;If the heading-text is empty, change it.  We could also fix
	;;it in the buffer, but for now that's too rare and there's
	;;too little payoff.
	(if (string= heading-text "")
	  (setq heading-text "An unnamed element")))
      

      ;;...if (not anchor-el)...
      (progn
	(setq anchor-id (tehom-make-random-string))

	;;Clip the heading text so we can move it.
	(setq 
	  heading-text 
	  (tehom-psgml-index-get-el-contents 
	    (tehom-psgml-find-element-by-numberpath numberpath) t))

	;;Add an anchor element, with the original text as its text.
	;;We re-find the element instead of re-using it, because we
	;;just changed it by clipping the text.
	(tehom-psgml-add-els-to-element 
	  (list (eval tehom-psgml-index-new-anchor-structure))
	  (tehom-psgml-find-element-by-numberpath numberpath))))
    

    (cons anchor-id heading-text)))


;;;;;;;;;;;;;;;;;;;;
;;Build controllers

(defun tehom-psgml-index-build-el-controller (el-spec)
  "Return a controller corresponding to an element and its subordinates."
  
  (let*
    (
      (numberpath (nth 0 el-spec))

      (cell
	(tehom-psgml-index-get-anchor-info numberpath))

      (anchor-id    
	(concat anchor-prefix "#" (car cell)))
      
      (heading-text (cdr cell))

      ;;This code may change as the element format expands.
      (sub-el-spec-list
	(if (> (length el-spec) 1)
	  (nth 1 el-spec)
	  nil))

      ;;Recurse if there are sub-elements to index.
      (sublist 
	(if sub-el-spec-list
	  (tehom-psgml-index-build-subtree-controller sub-el-spec-list)
	  nil)))
    
    (eval tehom-psgml-index-entry-structure)))


(defun tehom-psgml-index-build-subtree-controller (el-spec-list)
  "Return a controller corresponding to a list of elements."
  
  (let*
    (
      (entries
	(mapcar
	  'tehom-psgml-index-build-el-controller
	  el-spec-list)))
    
    (eval tehom-psgml-index-whole-list-structure)))



;;;;;;;;;;;;;;;;;;;;;
;;Code to find the proper elements to include in an index.  This
;;assumes that only the top-level elements after the path are ever
;;used.  Recursive elements would need more smarts.

(defun tehom-psgml-find-outlineable-contents-aux (el numberpath)

  "Find the immediate content elements that make an outline form.
EL is the element to start from.  NUMBERPATH is the path to el, and
should match el."

  (loop
    for c = (sgml-element-content el) then (sgml-element-next c)
    for n from 0
    while c
    for gi   = (sgml-element-gi c)

    ;;Remember only the elements that make an outline form.  If this
    ;;code ever were to handle recursive outlining, we'd do it here.
    for cell = (assoc gi tehom-psgml-outline-alist)

    if cell
    collect 
    (cons 
      (append numberpath (list n)) 
      (cdr cell))))


(defun tehom-psgml-find-outlineable-contents (cell)
  "Forwarding function for tehom-psgml-find-outlineable-contents-aux.

The difference is that it takes a cell, the same type that
tehom-psgml-find-numberpath-by-path returns."
  
  (tehom-psgml-find-outlineable-contents-aux (car cell) (cdr cell)))



;;;;;;;;;;;;;;;;;;;;;;;
;;Code to turn a list indicating depths into a tree.

;;Special variables that remain valid across levels of recursion.
(defvar *el-list*    nil "" )
(defvar *next-depth* nil "" )


(defun tehom-psgml-index-treeify-recurse ()
  "Return a list of sibling elements.

The list is in a form tehom-psgml-index-build-subtree-controller can
use.  The elements' children are placed appropriately."

  (declare (special *el-list* *next-depth*))

  (let*
    (done sib-list)

    (while (and *el-list* (not done))
      
      (let* 
	(
	  (cell  (nth 0 *el-list*))
	  (depth (cdr cell))

	  ;;Look ahead one place to determine whether to recurse,
	  ;;return, or continue.
	  (next-cell (nth 1 *el-list*))
	  (next-depth
	    (if
	      (null next-cell)
	      0
	      (cdr next-cell))))

	;;Store the special object for next depth.  This must be done
	;;before recursion, because if done later it would overwrite
	;;the final value given by recursion. 
	(setq *next-depth* next-depth)
	
	;;Immediately advance the list, before any recursion can
	;;happen, because further calls must look at new elements.
	(setq *el-list* (cdr *el-list*))

	(let* 
	  ((children
	     (cond
	       ((> next-depth depth)
		 (tehom-psgml-index-treeify-recurse))

	       ((<= next-depth depth)
		 nil)))
	    
	    ;;Build the structure.
	    (data (list (car cell) children)))

	  ;;This collects it in reversed order.  We'll reverse it when
	  ;;we're done, before we return.
	  (push data sib-list))

	;;If the next one is not as deep, it's a parent, so we'll be
	;;done with this level of recursion.  Important: Test against
	;;the special value in case we recursed.  Don't use the cached
	;;value. 
	(if (< *next-depth* depth)
	  (setq done t))))
    
    (reverse sib-list)))



(defun tehom-psgml-index-treeify-list-top (el-list)
  "Return EL-LIST as a tree according to depth.

The return value is in a form
tehom-psgml-index-build-subtree-controller can use."

  (setq *el-list* el-list)
  (tehom-psgml-index-treeify-recurse))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;The overall indexing function and entry point

;;;###autoload
(defun tehom-psgml-do-indexing 
  (source-buf anchor-prefix &optional target-marker indexable-path)
  "Index an HTML document in SOURCE-BUF. 

ANCHOR-PREFIX is a string to prepend to anchors, so that they refer to
the proper file or page.

TARGET-MARKER indicates the buffer and position to insert into.  If
not given, it defaults to the current point.

INDEXABLE-PATH is the path to the top indexing level.  It defaults to
tehom-psgml-outline-path."

  (interactive
    
    (let* 
      ((buf
	 (call-interactively
	  (lambda (buf)
	    (interactive "bBuffer to index: ")
	    (get-buffer buf))))

	;;Make nice defaults.  Making a smarter comparison of
	;;directories would be nice but extravagant to code.
	(anchor-prefix-default
	  (cond
	    ((eq buf (current-buffer)) "")
	    ( (string= 
		(file-name-directory (buffer-file-name buf))
		(file-name-directory (buffer-file-name (current-buffer))))
	      (file-name-nondirectory (buffer-file-name buf)))

	    (t buf)))
	
	(anchor-prefix
	  (read-from-minibuffer
	    "Anchor prefix: " anchor-prefix-default
	    nil nil nil anchor-prefix-default)))
      
      (list
	buf anchor-prefix)))

  
  ;;Handle some defaults.
  (if (not target-marker)
    (setq target-marker (point-marker)))

  (if (not indexable-path)
    (setq indexable-path tehom-psgml-outline-path))


  ;;Do the work.
  (with-current-buffer source-buf
    (sgml-need-dtd)

    (let
      (
	(body 
	  (tehom-psgml-find-numberpath-by-path indexable-path)))

      (if
	body
	(let* 
	  (
	    (outlineable-list
	      (tehom-psgml-find-outlineable-contents body))

	    (outlineable-tree
	      (tehom-psgml-index-treeify-list-top outlineable-list))

	    (controller 
	      (tehom-psgml-index-build-subtree-controller
		outlineable-tree)))

	  ;;We're slow, so don't let the user panic.
	  (message "Writing index...") 

	  (with-current-buffer (marker-buffer target-marker)
	    (goto-char (marker-position target-marker))
	    (tehom-psgml-insert-els (list controller)))

	  )))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tests

;;Some of these tests *cannot be run* if psgml is not set up for html.

(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file

  ;;;;
  ;;Test-support functions

  ;;User: Configure this according to where you put the test file.
  (defconst tehom-psgml-rtest-test-file 
    "~/projects/test-psgml.html" 
    "The location of the file containing the known document for the tests." )

  (defvar tehom-psgml-rtest-buf      nil 
    "A buffer with known contents for tests" )
  (defvar tehom-psgml-rtest-contents nil 
    "The known contents, saved between tests." )

  (defun tehom-psgml-rtest-setup ()
    ""
    (if (not tehom-psgml-rtest-contents)
      (let* 
	((buf 
	   (find-file-noselect tehom-psgml-rtest-test-file )))
   
	;;Acquire file contents.
	(with-current-buffer buf
	  (setq tehom-psgml-rtest-contents (buffer-string))

	  ;;Set up the test buffer in a known place so we don't have
	  ;;multiple copies if this code is executed in multiple
	  ;;places.  We set it up as a file because otherwise
	  ;;normal-mode won't do anything.
	  (setq tehom-psgml-rtest-buf 
	    (find-file-noselect "test-psgml-tmp.html")))

	;;Done with this buffer and we don't want to risk overwriting
	;;it, so get rid of it.
	(kill-buffer buf)

	;;Fill the test buffer for the first time.
	(tehom-psgml-rtest-refresh-buf-aux))))



  (defun tehom-psgml-rtest-refresh-buf-aux ()
    ""
    (with-current-buffer tehom-psgml-rtest-buf
      (erase-buffer)
      (insert tehom-psgml-rtest-contents)
      (normal-mode)
      (setq buffer-undo-list nil)))
  
  
  (defun tehom-psgml-rtest-refresh-buf ()
    "Refresh the test buffer if it is munged."

    (tehom-psgml-rtest-setup)
    (with-current-buffer tehom-psgml-rtest-buf
      (if buffer-undo-list
	(tehom-psgml-rtest-refresh-buf-aux))))

  (defmacro tehom-psgml-rtest-with-known-buf (&rest forms)
    ""
   
    `(progn 
       (tehom-psgml-rtest-refresh-buf)
       (with-current-buffer tehom-psgml-rtest-buf
	 ,@forms)))
  
  (defun tehom-psgml-rtest-all-els-from-list (l)
    ""
  
    (mapcar
      (function
	(lambda (x)
	  (tehom-psgml-find-element-by-numberpath (car x))))
      l))


  (defun tehom-psgml-rtest-all-titles-from-list (l)
    ""

    (mapcar
      (function
	(lambda (el)
	  (tehom-string-middle 
	    (tehom-psgml-index-get-el-contents el))))
    
      (tehom-psgml-rtest-all-els-from-list l)))


  (defun tehom-psgml-rtest-all-gi-from-list (l)
    ""
  
    (mapcar
      'sgml-element-gi
      (tehom-psgml-rtest-all-els-from-list l)))


  ;;;;;;
  ;;Regression tests proper.
  (put 'tehom-psgml-index-rtest 'rtest-suite t)
  (setq tehom-psgml-index-rtest
   '("tehom-psgml-index-rtest"


      ;;tehom-psgml-index-treeify-recurse
      ((progn
	 (setq *el-list* '((A . 1) (B . 2)))
	 (tehom-psgml-index-treeify-recurse))

	'((A ((B nil)))))

      ((progn
	 (setq *el-list* '((A . 1) (B . 2) (C . 1)))
	 (tehom-psgml-index-treeify-recurse))

	'((A ((B nil))) (C nil)))

      ((progn
	 (setq *el-list* '((A . 1) (B . 2) (C . 2) (D . 3) (E . 1)))
	 (tehom-psgml-index-treeify-recurse))

	'((A ((B nil) (C ((D nil))))) (E nil)))
      
      ("Test that we get the proper elements"
	(tehom-psgml-rtest-with-known-buf
	 (tehom-psgml-rtest-all-gi-from-list
	   (tehom-psgml-find-outlineable-contents 
	     (tehom-psgml-find-numberpath-by-path
	       tehom-psgml-outline-path))))

	'("H1" "H2" "H3" "H1" "H1"))

      ("Test acquiring titles"
	(tehom-psgml-rtest-with-known-buf
	 (tehom-psgml-rtest-all-titles-from-list 
	   (tehom-psgml-find-outlineable-contents 
	     (tehom-psgml-find-numberpath-by-path
	       tehom-psgml-outline-path))))

	'("Main heading" 
	   "[No title, element was complex]" 
	   "Tertiary heading" 
	   "Second primary heading" 
	   "[No title, element was complex]"))
      

      )))

(provide 'tehom-psgml-index)

;;; tehom-psgml-index.el ends here