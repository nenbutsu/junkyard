;;; isl.el --- tiny lisp on Emacs

;; Copyright (C) 2011 Yuji Minejima <bmonkey@nifty.com>

;; Author: Yuji Minejima <bmonkey@nifty.com>
;; Version
;; Keywords:
;; $Id: isl.el 1.1 2011/02/09 03:46:38 USER Exp USER $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Tiny lisp on Emacs
;;
;;

;;; Commentary:

;;
;;
;;


;;; Requirements:

;; Tested with GNU Emacs 23.2.1 on Windows XP SP3
;;
;;


;;; Distribution:

;; You can find the latest version of this package at:
;; http://homepage1.nifty.com/bmonkey/emacs/elisp/isl.el


;;; Change Log:

;; Version 1.1 (9 Feb 2011)
;;
;;

;;; Code:
(provide 'isl)

(defvar isl-global-variables
  nil)

(defvar isl-lexical-variables
  nil)

(defvar isl-lexical-functions
  nil)

(defvar isl-dynamic-variables
  nil)

(defvar isl-catchers
  nil)

(defvar isl-blocks
  nil)

(defvar isl-tagbodies
  nil)

(defvar isl-go-flag
  (make-symbol "*GO-FLAG*"))

(defvar isl-current-tagbody
  nil)

(defun isl-toplevel-eval (form)
  (cond
   ((consp form)
    (let ((operator (car form)))
      (cond
       ((eq operator 'defglobal) (isl-eval-defglobal form))
       ((eq operator 'defconstant) (isl-eval-defconstant form))
       ((eq operator 'defdynamic) (isl-eval-defdynamic form))
       ((eq operator 'defun) (isl-eval-defun form))
       (t (isl-eval-compound-form form)))))
   ((symbolp form) (isl-lexical-variable-reference form))
   (t form)))

(defun isl-eval (form)
  (cond
   ((consp form) (isl-eval-compound-form form))
   ((symbolp form) (isl-lexical-variable-reference form))
   (t form)))

(defun isl-eval-compound-form (form)
  (let ((operator (car form)))
    (cond
     ((or (eq operator 'defglobal)
          (eq operator 'defconstant)
          (eq operator 'defdynamic))
      (error "ISL: Not a toplovel definition: %S" form))
     ((eq operator 'let) (isl-eval-let form))
     ((eq operator 'let*) (isl-eval-let* form))
     ((eq operator 'setq) (isl-eval-setq form))
     
     ((eq operator 'dynamic-let) (isl-eval-dynamic-let form))
     ((eq operator 'set-dynamic) (isl-eval-set-dynamic form))

     ((eq operator 'function) (isl-eval-function form))
     ((eq operator 'functionp) (isl-eval-functionp form))

     ((eq operator 'setf) (isl-eval-setf form))
     ((eq operator 'quote) (cadr form))
     ((eq operator 'progn) (isl-eval-progn form))
     ((eq operator 'dynamic) (isl-dynamic-variable-reference form))

     ((eq operator 'funcall) (isl-eval-funcall form))
     ((eq operator 'apply) (isl-eval-apply form))
     ((eq operator 'lambda) (isl-eval-lambda form))
     ((eq operator 'flet) (isl-eval-flet form))
     ((eq operator 'labels) (isl-eval-labels form))
     ((and (consp operator) (car operator) 'lambda)
      (isl-eval-funcall `(funcall ,@form)))

     ((eq operator 'unwind-protect) (isl-eval-unwind-protect form))
     ((eq operator 'catch) (isl-eval-catch form))
     ((eq operator 'throw) (isl-eval-throw form))
     ((eq operator 'block) (isl-eval-block form))
     ((eq operator 'return-from) (isl-eval-return-from form))
     ((eq operator 'tagbody) (isl-eval-tagbody form))
     ((eq operator 'go) (isl-eval-go form))

     ((eq operator 'if) (isl-eval-if form))
     
     ((eq operator '+) (isl-eval-+ form))
     ((eq operator '-) (isl-eval-- form))
     ((eq operator '=) (isl-eval-= form))
     ((eq operator 'list) (isl-eval-list form))

     ((symbolp operator)
      (if (isl-fboundp operator)
          (isl-eval-funcall `(funcall (function ,operator) ,@(cdr form)))
        (error "ISL: Unbound function: %S" operator)))

     (t (error "ISL: Illegal function call %S" operator))
     )))

;; lexical variable stuff
(defun isl-lexical-variable-reference (name)
  (cond
   ((eq name nil) nil)
   ((eq name t) t)
   ((keywordp name) name)
   (t (let ((binding (assoc name `(,@isl-lexical-variables
                                   ,@isl-global-variables))))
        (if binding
            (cadr binding)
          (error "ISL: Unbound variable: %S" name))))))

(defun isl-eval-defglobal (defglobal-form)
  (pop defglobal-form)
  (let ((name (pop defglobal-form))
        (value (isl-eval (pop defglobal-form))))
    (push (cons name (cons value :changeable)) isl-global-variables)
    value))

(defun isl-eval-defconstant (defconstant-form)
  (pop defconstant-form)
  (let ((name (pop defconstant-form))
        (value (isl-eval (pop defconstant-form))))
    (push (cons name (cons value :constant)) isl-global-variables)
    value))

(defun isl-eval-setq (setq-form)
  (pop setq-form)
  (let* ((name (pop setq-form))
         (binding (assoc name `(,@isl-lexical-variables
                                ,@isl-global-variables))))
    (unless binding
      (error "ISL: Unbound variable: %S" name))
    (when (eq (cddr binding) :constant)
      (error "ISL: Can't modify constant: %S" name))
    (let ((value (isl-eval (pop setq-form))))
      (setcar (cdr binding) value)
      value)))

(defun isl-eval-let (let-form)
  (pop let-form)
  (let ((bindings (pop let-form))
        (body let-form)
        name value stack)
    (dolist (spec bindings)
      (setq name (pop spec))
      (setq value (isl-eval (pop spec)))
      (push (cons name (cons value :changeable)) stack))
    (let ((isl-lexical-variables `(,@stack ,@isl-lexical-variables)))
      (isl-eval-body body))))

(defun isl-eval-let* (let*-form)
  (pop let*-form)
  (let ((isl-lexical-variables isl-lexical-variables))
    (let ((bindings (pop let*-form))
          (body let*-form)
          name value)
      (dolist (spec bindings)
        (setq name (pop spec))
        (setq value (isl-eval (pop spec)))
        (push (cons name (cons value :changeable)) isl-lexical-variables))
      (isl-eval-body body))))

;; dynamic variable stuff
(defun isl-dynamic-variable-reference (dynamic-form)
  (let* ((name (cadr dynamic-form))
         (binding (assoc name isl-dynamic-variables)))
    (if binding
        (cdr binding)
      (error "ISL: Unbound dynamic variable: %S" name))))

(defun isl-eval-defdynamic (defdynamic-form)
  (pop defdynamic-form)
  (let ((name (pop defdynamic-form))
        (value (isl-eval (pop defdynamic-form))))
    (push (cons name value) isl-dynamic-variables)
    value))

(defun isl-eval-set-dynamic (set-dynamic-form)
  (pop set-dynamic-form)
  (let* ((value-form (pop set-dynamic-form))
         (name (pop set-dynamic-form))
         (binding (assoc name isl-dynamic-variables)))
    (unless binding
      (error "ISL: Unbound dynamic variable: %S" name))
    (let ((value (isl-eval value-form)))
      (setcdr binding value)
      value)))

(defun isl-eval-dynamic-let (dynamic-let-form)
  (pop dynamic-let-form)
  (let ((bindings (pop dynamic-let-form))
        (body dynamic-let-form)
        name value stack)
    (dolist (spec bindings)
      (setq name (pop spec))
      (setq value (isl-eval (pop spec)))
      (push (cons name value) stack))
    (let ((isl-dynamic-variables `(,@stack ,@isl-dynamic-variables)))
      (isl-eval-body body))))

;;setf
(defun isl-eval-setf (setf-form)
  (pop setf-form)
  (let ((place (pop setf-form))
        (form (pop setf-form)))
    (cond
     ((and (consp place) (eq (car place) 'dynamic))
      (isl-eval-set-dynamic `(set-dynamic ,form ,(cadr place))))
     )))


;; function stuff
(defun isl-function (name)
  (or (isl-fboundp name)
      (error "ISL: Undefined function: %S" name)))

(defun isl-eval-function (form)
  (pop form)
  (isl-function (car form)))

(defun isl-fboundp (name)
  (let ((f (assoc name isl-lexical-functions)))
    (or (cdr f)
        (get name :isl-function))))

(defun isl-functionp (object)
  (and (cons object)
       (eq :function (plist-get object :type))))

(defun isl-eval-functionp (form)
  (pop form)
  (isl-functionp (car form)))

(defun isl-eval-lambda (lambda-form)
  (pop lambda-form)
  (isl-eval-defun `(defun nil ,@lambda-form)))

(defun isl-eval-defun (defun-form)
  (pop defun-form)
  (let* ((name (pop defun-form))
         (args (pop defun-form))
         (body defun-form))
    (put name :isl-function
         (list :type :function
               :lambda `(lambda ,args ,@body)
               :name name
               :lexical-variables isl-lexical-variables
               :lexical-functions isl-lexical-functions
               :blocks isl-blocks))))

(defun isl-eval-flet (flet-form)
  (pop flet-form)
  (let* ((functions (pop flet-form))
         (body flet-form)
         (stack nil))
    (dolist (f functions)
      (let* ((name (pop f))
             (args (pop f))
             (body f))
        (push (list name
                    :type :function
                    :lambda `(lambda ,args ,@body)
                    :name name
                    :lexical-variables isl-lexical-variables
                    :lexical-functions isl-lexical-functions
                    :blocks isl-blocks)
              stack)))
    (let ((isl-lexical-functions `(,@stack ,@isl-lexical-functions)))
      (isl-eval-body body))))

(defun isl-eval-labels (labels-form)
  (pop labels-form)
  (let* ((functions (pop labels-form))
         (body labels-form)
         (stack nil))
    (dolist (f functions)
      (let* ((name (pop f))
             (args (pop f))
             (body f))
        (push (list name
                    :type :function
                    :lambda `(lambda ,args ,@body)
                    :name name
                    :lexical-variables isl-lexical-variables
                    :blocks isl-blocks
                    :tagbodies isl-tagbodies)
              stack)))
    (let ((isl-lexical-functions `(,@stack ,@isl-lexical-functions)))
      (dolist (record stack)
        (setcdr record
                (plist-put (cdr record)
                           :lexical-functions isl-lexical-functions)))
        
      (isl-eval-body body))))



(defun isl-eval-funcall (funcall-form)
  (pop funcall-form)
  (let ((function-form (car funcall-form))
        (args (cdr funcall-form)))
    (isl-eval-apply `(apply ,function-form (list ,@args)))))

(defun isl-eval-apply-args (args)
  (let* ((args (mapcar #'isl-eval args))
         (len (length args)))
    (cond
     ((= 0 len) nil)
     ((= 1 len) (car args))
     (t (setcdr (last args 2) (car (last args)))
        args))))

(defun isl-eval-apply (apply-form)
  (pop apply-form)
  (let* ((function (isl-eval (car apply-form)))
         (args (isl-eval-apply-args (cdr apply-form)))
         (lambda (plist-get function :lambda))
         (vars (cadr lambda))
         (body (cddr lambda))
         (isl-lexical-variables (plist-get function :lexical-variables))
         (isl-lexical-functions (plist-get function :lexical-functions))
         (isl-blocks (plist-get function :blocks))
         (isl-tagbodies (plist-get function :tagbodies)))
    (dolist (v vars)
      (push (cons v (cons (pop args) :changeable)) isl-lexical-variables))
    (isl-eval-body body)))

;; non local exit stuff
(defun isl-eval-cleanup (cleanup)
  (setq isl-lexical-variables (plist-get cleanup :lexical-variables))
  (setq isl-lexical-functions (plist-get cleanup :lexical-functions))
  (setq isl-dynamic-variables (plist-get cleanup :dynamic-variables))
  (setq isl-catchers (plist-get cleanup :catchers))
  (setq isl-blocks (plist-get cleanup :blocks))
  (setq isl-tagbodies (plist-get cleanup :tagbodies))
  (isl-eval-body (plist-get cleanup :body)))

(defun isl-eval-catch (catch-form)
  (pop catch-form)
  (let* ((catcher `(:catch ,(isl-eval (pop catch-form))))
         (body catch-form)
         (isl-lexical-variables isl-lexical-variables)
         (isl-lexical-functions isl-lexical-functions)
         (isl-dynamic-variables isl-dynamic-variables)
         (isl-catchers `(,catcher ,@isl-catchers))
         (isl-blocks isl-blocks)
         (result (catch catcher (isl-eval-body body))))
    result))

(defun isl-find-catcher (tag)
  (catch 'found
    (dolist (c isl-catchers)
      (when (and (eq (car c) :catch) (eq (cadr c) tag))
        (throw 'found c)))))

(defun isl-eval-throw (throw-form)
  (pop throw-form)
  (let* ((tag (isl-eval (pop throw-form)))
         (result (isl-eval (pop throw-form)))
         (catcher (isl-find-catcher tag)))
    (unless catcher (error "ISL: Undefined catch tag: %S" tag))
    (catch 'end
      (dolist (c isl-catchers)
        (when (eq c catcher) (throw 'end nil))
        (when (eq (plist-get c :type) :unwind-cleanup)
          (isl-eval-cleanup c))))
    (throw catcher result)))

(defun isl-eval-block (block-form)
  (pop block-form)
  (let* ((block `(:block ,(pop block-form)))
         (body block-form)
         (isl-lexical-variables isl-lexical-variables)
         (isl-lexical-functions isl-lexical-functions)
         (isl-dynamic-variables isl-dynamic-variables)
         (isl-catchers isl-catchers)
         (isl-blocks `(,block ,@isl-blocks))
         (result (catch block (isl-eval-body body))))
    result))

(defun isl-find-block (tag)
  (catch 'found
    (dolist (b isl-blocks)
      (when (and (eq (car b) :block) (eq (cadr b) tag))
        (throw 'found b)))))

(defun isl-eval-return-from (return-from-form)
  (pop return-from-form)
  (let* ((tag (pop return-from-form))
         (result (isl-eval (pop return-from-form)))
         (block (isl-find-block tag)))
    (unless block (error "ISL: Undefined block tag: %S" tag))
    (catch 'end
      (dolist (b isl-blocks)
        (when (eq b block) (throw 'end nil))
        (when (eq (plist-get b :type) :unwind-cleanup)
          (isl-eval-cleanup b))))
    (throw block result)))

(defun isl-remove-tags (tagbody)
  (let* ((top (cons nil nil))
         (splice top)
         tag-or-form)
    (while tagbody
      (setq tag-or-form (pop tagbody))
      (when (not (symbolp tag-or-form))
        (setcdr splice (cons tag-or-form nil))
        (setq splice (cdr splice))))
    (cdr top)))

(defun isl-eval-tagbody (tagbody-form)
  (pop tagbody-form)
  (let* ((here tagbody-form)
         (top-tag (make-symbol "*TOP-TAG*"))
         (isl-current-tagbody `(:tag ,top-tag :body ,here :top-tag ,top-tag))
         (stack (list isl-current-tagbody)))
    (while here
      (when (symbolp (car here))
          (push `(:tag ,(car here) :body ,here :top-tag ,top-tag)
                stack))
      (pop here))
    (dolist (tb stack)
      (plist-put tb :body (isl-remove-tags (plist-get tb :body))))
    (let* ((result isl-go-flag))
      (while (eq result isl-go-flag)
        (setq result
              (catch top-tag
                (let ((isl-lexical-variables isl-lexical-variables)
                      (isl-lexical-functions isl-lexical-functions)
                      (isl-dynamic-variables isl-dynamic-variables)
                      (isl-catchers isl-catchers)
                      (isl-blocks isl-blocks)
                      (isl-tagbodies `(,@stack ,@isl-tagbodies)))
                  (isl-eval-body (plist-get isl-current-tagbody :body)))))))
    nil))

(defun isl-find-tagbody (tag)
  (catch 'found
    (dolist (tb isl-tagbodies)
      (when (and (eq (car tb) :tag) (eq (cadr tb) tag))
        (throw 'found tb)))))

(defun isl-eval-go (go-form)
  (pop go-form)
  (let* ((tag (pop go-form))
         (tagbody (isl-find-tagbody tag)))
    (unless tagbody (error "ISL: Undefined tagbody tag: %S" tag))
    (catch 'end
      (dolist (tb isl-tagbodies)
        (when (eq tb tagbody) (throw 'end nil))
        (when (eq (plist-get tb :type) :unwind-cleanup)
          (isl-eval-cleanup tb))))
    (setq isl-current-tagbody tagbody)
    (throw (plist-get tagbody :top-tag) isl-go-flag)))


(defun isl-eval-unwind-protect (unwind-protect-form)
  (pop unwind-protect-form)
  (let* ((protected-form (pop unwind-protect-form))
         (cleanup-body unwind-protect-form)
         (cleanup `(:type :unwind-cleanup
                    :lexical-variables ,isl-lexical-variables
                    :lexical-functions ,isl-lexical-functions
                    :dynamic-variables ,isl-dynamic-variables
                    :catchers ,isl-catchers
                    :blocks ,isl-blocks
                    :tagbodies ,isl-tagbodies
                    :body ,cleanup-body)))
    (push cleanup isl-catchers)
    (push cleanup isl-blocks)
    (push cleanup isl-tagbodies)
    (isl-eval protected-form)
    (pop isl-catchers)
    (pop isl-blocks)
    (pop isl-tagbodies)
    (isl-eval-body cleanup-body)))

;; conditionals
(defun isl-eval-if (if-form)
  (pop if-form)
  (let* ((test-form (pop if-form))
         (then-form (pop if-form))
         (else-form (pop if-form)))
    (if (isl-eval test-form)
        (isl-eval then-form)
      (isl-eval else-form))))

;;
(defun isl-eval-progn (progn-form)
  (isl-eval-body (cdr progn-form)))

(defun isl-eval-body (forms)
  (let (return-value)
    (while forms
      (let ((f (pop forms)))
        (setq return-value (isl-eval f))))
    return-value))

(defun isl-eval-last-sexp ()
  ""
  (interactive)
  (print (isl-toplevel-eval (preceding-sexp))))

(defun isl-eval-list (list-form)
  (pop list-form)
  (mapcar #'isl-eval list-form))

(defun isl-eval-+ (form)
  (pop form)
  (let ((result 0))
    (dolist (num-form form)
      (setq result (+ result (isl-eval num-form))))
    result))

(defun isl-eval-- (form)
  (pop form)
  (let ((n (length form)))
    (cond
     ((= n 0) 0)
     ((= n 1) (- (isl-eval (pop form))))
     (t  (let ((result (isl-eval (pop form))))
           (dolist (num-form form)
             (setq result (- result (isl-eval num-form))))
           result)))))

(defun isl-eval-= (form)
  (pop form)
  (let* ((a (isl-eval (pop form)))
         (b (isl-eval (pop form))))
    (= a b)))

;;; isl.el ends here
