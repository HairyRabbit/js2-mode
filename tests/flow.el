;;; tests/flow.el --- Some tests for js2-mode with flow syntax.

;; Copyright (C) 2009, 2011-2017  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'js2-mode)
(require 'cl-lib)

(defmacro js2-deftest (name buffer-contents &rest body)
  (declare (indent defun))
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (with-temp-buffer
       (save-excursion
         (insert ,buffer-contents))
       (unwind-protect
           (progn
             ,@body)
         (fundamental-mode)))))

(defun js2-mode--and-parse ()
  (js2-mode)
  (js2-reparse))

(defun js2-test-string-to-ast (s)
  (insert s)
  (js2-mode--and-parse)
  (should (null js2-mode-buffer-dirty-p))
  js2-mode-ast)

(cl-defun js2-test-parse-string (code-string &key syntax-error errors-count
                                             reference warnings-count)
  (ert-with-test-buffer (:name 'origin)
    (let ((ast (js2-test-string-to-ast code-string)))
      (if syntax-error
          (let ((errors (js2-ast-root-errors ast)))
            (should (= (or errors-count 1) (length errors)))
            (cl-destructuring-bind (_ pos len) (car (last errors))
              (should (string= syntax-error (substring code-string
                                                       (1- pos) (+ pos len -1))))))
        (should (= 0 (length (js2-ast-root-errors ast))))
        (ert-with-test-buffer (:name 'copy)
          (js2-print-tree ast)
          (skip-chars-backward " \t\n")
          (should (string= (or reference code-string)
                           (buffer-substring-no-properties
                            (point-min) (point)))))
        (when warnings-count
          (should (= warnings-count
                     (length (js2-ast-root-warnings ast)))))))))

(cl-defmacro js2-deftest-parse (name code-string &key bind syntax-error errors-count
                                     reference warnings-count)
  "Parse CODE-STRING.  If SYNTAX-ERROR is nil, print syntax tree
with `js2-print-tree' and assert the result to be equal to
REFERENCE, if present, or the original string.  If SYNTAX-ERROR
is passed, expect syntax error highlighting substring equal to
SYNTAX-ERROR value.  BIND defines bindings to apply them around
the test."
  (declare (indent defun))
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append bind '((js2-basic-offset 2)))
       (js2-test-parse-string ,code-string
                              :syntax-error ,syntax-error
                              :errors-count ,errors-count
                              :warnings-count ,warnings-count
                              :reference ,reference))))

(defun js2-find-node (node predicate)
  "Find the first descendant of NODE meeting PREDICATE."
  (let (target)
    (js2-visit-ast node (lambda (n end-p)
                          (unless end-p
                            (if (funcall predicate n)
                              (progn (setq target n) nil)
                              t))))
    target))

(defun js2-node-text (node)
  "Return the part of the buffer corresponding to NODE as a string."
  (let ((beg (js2-node-abs-pos node)))
    (buffer-substring-no-properties beg (+ beg (js2-node-len node)))))

(defun js2-test-indent (content keep-indent)
  (let ((s (replace-regexp-in-string "^ *|" "" content)))
    (with-temp-buffer
      (insert
       (if keep-indent
           s
         (replace-regexp-in-string "^ *" "" s)))
      (js2-jsx-mode)
      (js2-reparse) ; solely for js2-jsx-self-closing, for some reason
      (indent-region (point-min) (point-max))
      (should (string= s (buffer-substring-no-properties
                          (point-min) (point)))))))

(cl-defmacro js2-deftest-indent (name content &key bind keep-indent)
  `(ert-deftest ,(intern (format "js2-%s" name)) ()
     (let ,(append '(indent-tabs-mode
                     (js2-basic-offset 2)
                     (js2-pretty-multiline-declarations t)
                     (inhibit-point-motion-hooks t))
                   bind)
       (js2-test-indent ,content ,keep-indent))))

;;; Type annotations

;; Primitive type

(js2-deftest-parse flow-primitive-number-type
  "var a: number;")

(js2-deftest-parse flow-primitive-string-type
  "var a: string;")

(js2-deftest-parse flow-primitive-boolean-type
  "var a: boolean;")

(js2-deftest-parse flow-primitive-bool-type
  "var a: bool;")

(js2-deftest-parse flow-primitive-any-type
  "var a: any;")

(js2-deftest-parse flow-primitive-mixed-type
  "var a: mixed;")

(js2-deftest-parse flow-primitive-empty-type
  "var a: empty;")

;; Literal type

(js2-deftest-parse flow-literal-number-type
  "var a: 42;")

(js2-deftest-parse flow-literal-number-negative-type
  "var a: -42;")

(js2-deftest-parse flow-literal-number-dot-type
  "var a: .42;")

(js2-deftest-parse flow-literal-number-e-type
  "var a: 1e42;")

;; (js2-deftest-parse flow-literal-number-hex-type
;;   "var a: 0x42;")

;; (js2-deftest-parse flow-literal-string-type
;;   "var a: 'foo';")

;; (js2-deftest-parse flow-literal-string-double-quote-type
;;   "var a: \"foo\";")

(js2-deftest-parse flow-literal-true-type
  "var a: true;")

(js2-deftest-parse flow-literal-false-type
  "var a: false;")

(js2-deftest-parse flow-literal-null-type
  "var a: null;")

(js2-deftest-parse flow-literal-undefined-type
  "var a: undefined;")

(js2-deftest-parse flow-literal-this-type
  "var a: this;")

(js2-deftest-parse flow-literal-void-type
  "var a: void;")

(js2-deftest-parse flow-literal-star-type
  "var a: *;")

;; Generic Type

(js2-deftest-parse flow-generic-type
  "var a: a;")

(js2-deftest-parse flow-generic-member-type
  "var a: a.b;")

(js2-deftest-parse flow-generic-member-type-no-closed
  "var a: a.b.;"
  :syntax-error ".")

(js2-deftest-parse flow-generic-param-type
  "var a: a<b>;")

(js2-deftest-parse flow-generic-param-type-1
  "var a: a<>;")

(js2-deftest-parse flow-generic-params-type
  "var a: a<b, c>;")

(js2-deftest-parse flow-generic-params-type-1
  "var a: a<b, c,>;")

(js2-deftest-parse flow-generic-params-type-2
  "var a: a<;"
  :syntax-error "<"
  :errors-count 2)

(js2-deftest-parse flow-generic-params-type-3
  "var a: a<,;"
  :syntax-error ",")

(js2-deftest-parse flow-generic-params-type-4
  "var a: a<b;"
  :syntax-error "b")

(js2-deftest-parse flow-generic-params-type-5
  "var a: a<b,;"
  :syntax-error ","
  :errors-count 2)

(js2-deftest-parse flow-generic-params-type-6
  "var a: a<,>;"
  :syntax-error ","
  :errors-count 3)

(js2-deftest-parse flow-generic-params-nested-type
  "var a: a<b, c<d>>;")

(js2-deftest-parse flow-generic-params-nested-type-2
  "var a: a<b, c<d>, e>;")

(js2-deftest-parse flow-generic-params-nested-type-3
  "var a: a<b, c<d, e<f>>>;")

(js2-deftest-parse flow-generic-member-params-type
  "var a: a.b<c, d>;")

(js2-deftest-parse flow-generic-params-decl-type
  "var a: <b>(c) => d;")

(js2-deftest-parse flow-generic-params-decl-type-2
  "var a: <b: c>(d) => e;")

(js2-deftest-parse flow-generic-params-decl-type-3
  "var a: <b = 42>(c) => d;")

(js2-deftest-parse flow-generic-params-decl-type-4
  "var a: <b: c = 42>(d) => e;")

(js2-deftest-parse flow-generic-params-decl-type-5
  "var a: <b: c = d, e>(f) => g;")

(js2-deftest-parse flow-generic-params-decl-type-6
  "var a: <b: c = d, e: f>(g) => h;")

(js2-deftest-parse flow-generic-params-decl-type-7
  "var a: <b, c: d<e>>(f) => g;")

(js2-deftest-parse flow-generic-params-decl-type-8
  "var a: <b<c>>(d) => e;")

(js2-deftest-parse flow-generic-params-decl-type-no-closed
  "var a: <b(c) => d;"
  :syntax-error "b")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-2
  "var a: <,(b) => c;"
  :syntax-error ",")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-3
  "var a: <,b(c) => d;"
  :syntax-error ","
  :errors-count 4)

(js2-deftest-parse flow-generic-params-decl-type-no-closed-4
  "var a: <,>(b) => c;"
  :syntax-error ","
  :errors-count 6)

(js2-deftest-parse flow-generic-params-decl-type-no-closed-5
  "var a: <b:>(c) => d;"
  :syntax-error ":")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-6
  "var a: <b = >(c) => d;"
  :syntax-error "=")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-7
  "var a: <b: = >(c) => d;"
  :syntax-error ":"
  :errors-count 2)

(js2-deftest-parse flow-generic-params-decl-type-no-closed-8
  "var a: <b: c = >(d) => e;"
  :syntax-error "=")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-9
  "var a: <b: c = d<>(c) => d;"
  :syntax-error ">")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-10
  "var a: <b: c = d<e>(f) => g;"
  :syntax-error ">")

(js2-deftest-parse flow-generic-params-decl-type-no-closed-11
  "var a: <b: c = d<e(f) => g;"
  :syntax-error "e"
  :errors-count 2)

;; (js2-deftest-parse flow-generic-params-decl-type-not-decl
;;   "var a: <b: c = d<e: f>>(g) => h;")

;; Typeof type

(js2-deftest-parse flow-typeof-type
  "var a: typeof a;")

(js2-deftest-parse flow-typeof-type-nested
  "var a: typeof typeof a;")

(js2-deftest-parse flow-typeof-type-no-argument
  "var a: typeof;"
  :syntax-error "typeof")

(js2-deftest-parse flow-typeof-type-no-argument-2
  "var a: typeof typeof;"
  :syntax-error "typeof"
  :errors-count 1)

;; Maybe type

(js2-deftest-parse flow-maybe-type
  "var a: ?number;")

(js2-deftest-parse flow-maybe-maybe-type
  "var a: ??number;")

;; Array type

(js2-deftest-parse flow-array-type
  "var a: number[];")

(js2-deftest-parse flow-array-array-type
  "var a: number[][];")

(js2-deftest-parse flow-array-type-no-closed
  "var a: number[;"
  :syntax-error "[")

;; Tuple type

(js2-deftest-parse flow-tuple-type
  "var a: [a];")

(js2-deftest-parse flow-tuple-multi-type
  "var a: [a, b];")

(js2-deftest-parse flow-tuple-empty-type
  "var a: [];")

(js2-deftest-parse flow-tuple-array-type
  "var a: [][];")

(js2-deftest-parse flow-tuple-tuple-type
  "var a: [[]];")

(js2-deftest-parse flow-tuple-array-type
  "var a: [b[]];")

(js2-deftest-parse flow-tuple-type-no-closed
  "var a: [;"
  :syntax-error "["
  :errors-count 2)

(js2-deftest-parse flow-tuple-type-no-closed-2
  "var a: [b;"
  :syntax-error "b")

(js2-deftest-parse flow-tuple-type-no-closed-3
  "var a: [b,;"
  :syntax-error ","
  :errors-count 2)

(js2-deftest-parse flow-tuple-type-trailing-comma
  "var a: [,];"
  :syntax-error ","
  :errors-count 3)

(js2-deftest-parse flow-tuple-type-trailing-comma-2
  "var a: [b,];")

;; Function type

(js2-deftest-parse flow-function-type
  "var a: b => c;")

(js2-deftest-parse flow-function-type-maybe
  "var a: ?b => c;")

(js2-deftest-parse flow-function-type-chain
  "var a: b => c => d;")

(js2-deftest-parse flow-function-type-param
  "var a: (b) => c;")

(js2-deftest-parse flow-function-type-param-1
  "var a: (b);")

(js2-deftest-parse flow-function-type-param-tuple-type
  "var a: [] => c;")

(js2-deftest-parse flow-function-type-param-object-type
  "var a: {} => c;")

(js2-deftest-parse flow-function-type-param-function-type
  "var a: (b => c) => d;")

(js2-deftest-parse flow-function-type-param-no-closed
  "var a: (;"
  :syntax-error "("
  :errors-count 3)

(js2-deftest-parse flow-function-type-param-free
  "var a: b => c;")

(js2-deftest-parse flow-function-type-params
  "var a: (b, c) => d;")

(js2-deftest-parse flow-function-type-params-trailing-comma
  "var a: (b,) => c;")

(js2-deftest-parse flow-function-type-params-no-closed
  "var a: (b => c;"
  :syntax-error "c")

(js2-deftest-parse flow-function-type-params-no-closed-2
  "var a: (b, => c;"
  :syntax-error "=>"
  :errors-count 2)

(js2-deftest-parse flow-function-type-params-named
  "var a: (b: c) => d;")

(js2-deftest-parse flow-function-type-params-named-2
  "var a: (b: c, d) => e;")

(js2-deftest-parse flow-function-type-params-named-no-closed
  "var a: (b:) => d;"
  :syntax-error ":")

(js2-deftest-parse flow-function-type-params-named-no-closed-2
  "var a: (b:,) => d;"
  :syntax-error ":")

(js2-deftest-parse flow-function-type-params-optional
  "var a: (b?: c) => d;")

(js2-deftest-parse flow-function-type-params-optional-2
  "var a: (b?) => d;"
  :syntax-error "?")

(js2-deftest-parse flow-function-type-params-rest
  "var a: (...b) => c;")

(js2-deftest-parse flow-function-type-params-rest-2
  "var a: (...b: c) => d;")

(js2-deftest-parse flow-function-type-params-rest-3
  "var a: (b, ...c) => d;")

(js2-deftest-parse flow-function-type-params-rest-4
  "var a: (b?: c, ...d) => e;")

(js2-deftest-parse flow-function-type-params-rest-5
  "var a: (...b, c) => d;"
  :syntax-error "b"
  :errors-count 5)

(js2-deftest-parse flow-function-type-params-type-params
  "var a: <b>(c) => d;")

(js2-deftest-parse flow-function-type-params-type-params-2
  "var a: <b>c => d;"
  :syntax-error ">"
  :errors-count 2)

;; Object type

(js2-deftest-parse flow-object-type
  "var a: {};")

(js2-deftest-parse flow-object-type-prop
  "var a: {b: c};")

(js2-deftest-parse flow-object-type-prop-no-value
  "var a: { b };"
  :syntax-error "b")

(js2-deftest-parse flow-object-type-prop-optional
  "var a: {b?: c};")

(js2-deftest-parse flow-object-type-prop-readonly
  "var a: {+b: c};")

(js2-deftest-parse flow-object-type-prop-writeonly
  "var a: {-b: c};")

(js2-deftest-parse flow-object-type-prop-readonly-optional
  "var a: {+b?: c};")

(js2-deftest-parse flow-object-type-prop-method
  "var a: {b(): c};")

(js2-deftest-parse flow-object-type-prop-method-getset
  "var a: {get b(): c};")

(js2-deftest-parse flow-object-type-prop-method-getset-2
  "var a: {get set b(): c};"
  :syntax-error "set"
  :errors-count 4)

(js2-deftest-parse flow-object-type-prop-method-optional
  "var a: {b?(): c};")

(js2-deftest-parse flow-object-type-prop-indexed
  "var a: {[b]: c};")

(js2-deftest-parse flow-object-type-prop-indexed-named
  "var a: {[b: c]: d};")

(js2-deftest-parse flow-object-type-prop-call
  "var a: {(): b};")

(js2-deftest-parse flow-object-type-prop-call-2
  "var a: {<b>(): c};")

(js2-deftest-parse flow-object-type-prop-rest
  "var a: {...b};")

(js2-deftest-parse flow-object-type-prop-rest-2
  "var a: {...b: c};"
  :syntax-error "b"
  :errors-count 5)

(js2-deftest-parse flow-object-type-props
  "var a: {b: c, d: e};")

(js2-deftest-parse flow-object-type-props-2
  "var a: {b: c, ...d, +[e: f]: g, <h>(i: g, k, ...l): m, get n(o: p): q,};")

(js2-deftest-parse flow-object-no-closed
  "var a: {b:;"
  :syntax-error ":"
  :errors-count 2)

(js2-deftest-parse flow-object-no-closed-2
  "var a: { b: c;"
  :syntax-error "c")

(js2-deftest-parse flow-object-no-closed-3
  "var a: { b: c,;"
  :syntax-error ","
  :errors-count 3)

(js2-deftest-parse flow-object-type-props-trailing-commas
  "var a: {b: c,};")

(js2-deftest-parse flow-object-type-exact
  "var a: {|b: c|};")

(js2-deftest-parse flow-object-type-exact-no-closed
  "var a: {|b: c};"
  :syntax-error "c"
  :errors-count 2)

(js2-deftest-parse flow-object-type-exact-no-closed-2
  "var a: {|b: c;"
  :syntax-error "c")

(js2-deftest-parse flow-object-type-exact-with-union
  "var a: {|b: c | d|};")

(js2-deftest-parse flow-object-type-exact-with-union-2
  "var a: {|b: | c|};")

(js2-deftest-parse flow-object-type-exact-with-union-3
  "var a: {b: c|};"
  :syntax-error "c"
  :errors-count 3)

(js2-deftest-parse flow-object-type-exact-with-union-no-closed
  "var a: {|b: | c};"
  :syntax-error "c"
  :errors-count 2)

(js2-deftest-parse flow-object-type-exact-with-union-no-closed-2
  "var a: {|b: | c;"
  :syntax-error "c"
  :errors-count 1)

;; Intersection type

(js2-deftest-parse flow-intersection-type
  "var a: b & c;")

(js2-deftest-parse flow-intersection-type-2
  "var a: & b & c;")

(js2-deftest-parse flow-intersection-type-3
  "var a: b & c & d;")

(js2-deftest-parse flow-intersection-type-4
  "var a: &;"
  :syntax-error "&")

;; Union type

(js2-deftest-parse flow-union-type
  "var a: b | c;")

(js2-deftest-parse flow-union-type-2
  "var a: | b | c;")

(js2-deftest-parse flow-union-type-3
  "var a: b | c | d;")

(js2-deftest-parse flow-union-type-4
  "var a: |;"
  :syntax-error "|")

;;; Declatation, Statements, Expressions

;; Variables

(js2-deftest-parse variable-with-type
  "var a: a;")

(js2-deftest-parse variable-with-type-multi
  "var a: b, c: d;")

(js2-deftest-parse variable-with-type-init
  "var a: b = 42;")

(js2-deftest-parse variable-with-type-let
  "let a: b;")

(js2-deftest-parse variable-with-type-const
  "const a: b;")

;; Functions

(js2-deftest-parse function-stmt-basic
  "function a() {\n}")

(js2-deftest-parse function-expr-basic
  "var a = function() {};")

(js2-deftest-parse arrow-function-basic
  "var a = () => {};")

(js2-deftest-parse function-stmt-return-type
  "function a(): b {\n}")

(js2-deftest-parse function-stmt-return-type-2
  "function a(): b<c<d<e>>> {\n}")

(js2-deftest-parse function-expr-return-type
  "var a = function(): b {};")

(js2-deftest-parse arrow-function-return-type
  "var a = (): b => {};")

(js2-deftest-parse arrow-function-return-type-2
  "var a = (): b<c> => {};")

(js2-deftest-parse arrow-function-return-type-3
  "var a = (): b<c<d>> => {};")

(js2-deftest-parse function-expr-predicate-annotation
  "var a = function(): %checks {};")

(js2-deftest-parse function-expr-predicate-annotation
  "var a = function(): b %checks {};")

(js2-deftest-parse arrow-function-predicate-annotation
  "var a = (): %checks => {};")

(js2-deftest-parse arrow-function-predicate-annotation-2
  "var a = (): b %checks => {};")

(js2-deftest-parse function-stmt-type-params
  "function a<b>() {\n}")

(js2-deftest-parse function-expr-type-params
  "var a = function <b>() {};")

(js2-deftest-parse arrow-function-type-params
  "var a = <b>() => {};")

(js2-deftest-parse arrow-function-type-params-2
  "var a = <b<c>>() => {};")

(js2-deftest-parse async-function-stmt-type-params
  "async function a<b>() {\n}")

(js2-deftest-parse async-function-expr-type-params
  "var a = async function <b>() {};")

(js2-deftest-parse async-arrow-function-type-params
  "var a = <b> async () => {};")

(js2-deftest-parse function-stmt-param-type
  "function a(b: c) {\n}")

(js2-deftest-parse function-stmt-param-type-2
  "function a(b: c, []: d, {}: e, ...f: g) {\n}")

(js2-deftest-parse function-stmt-param-type-3
  "function a(b: c, d, e: f = 42, ...g) {\n}")

(js2-deftest-parse function-stmt-param-type-4
  "var a = (b: c) => {};")

(js2-deftest-parse function-stmt-param-type-5
  "var a = b: c => {};"
  :syntax-error ";"
  :errors-count 2)

(js2-deftest-parse function-stmt-param-type-optional
  "function a(b?: c) {\n}")

(js2-deftest-parse function-stmt-param-type-optional-2
  "function a([]?: c) {\n}")

(js2-deftest-parse function-stmt-param-type-optional-3
  "function a(...b?: c) {\n}")

(js2-deftest-parse function-type
  "var a = <a>(b: c, d): e => {};")

(js2-deftest-parse function-type-2
  "function a<b>(c: d): e {\n}")

;; Classes

(js2-deftest-parse class-stmt-basic
  "class a {\n}")

(js2-deftest-parse class-expr-basic
  "var a = class {\n};")

(js2-deftest-parse class-stmt-extends-super
  "class a extends b {\n}")

(js2-deftest-parse class-stmt-impl-interface
  "class a implements b {\n}")

(js2-deftest-parse class-stmt-impl-interface-multi
  "class a implements b, c {\n}")

(js2-deftest-parse class-stmt-extends-super-impl-interface-multi
  "class a extends b implements c, d {\n}")

(js2-deftest-parse class-stmt-bad-extends-with-impl-interface
  "class a extends implements b {\n}"
  :syntax-error "implements"
  :errors-count 3)

(js2-deftest-parse class-stmt-type-params
  "class a<b> {\n}")

(js2-deftest-parse class-stmt-type-params-and-super-type-params
  "class a<b> extends c<d> {\n}")

(js2-deftest-parse class-stmt-type-params-and-super-type-params-2
  "class a<b> extends (c)<d> {\n}")

(js2-deftest-parse class-stmt-type-params-and-super-type-params-3
  "class a<b> extends (c > 42)<d> {\n}")

(js2-deftest-parse class-stmt-type-params-and-super-type-params-4
  "class a<b> extends c.d<e> {\n}")

(js2-deftest-parse class-stmt-type-params-and-super-type-params-5
  "class a<b> extends [c]<d> {\n}")

;; (js2-deftest-parse class-stmt-type-params-and-super-type-params-6
;;   "class a<b> extends { c: d }<e> {\n}")

(js2-deftest-parse class-stmt-type-params-and-super-type-params-7
  "class a<b> extends c()<d> {\n}")

(js2-deftest-parse class-stmt-type-params-and-super-type-params-with-interfaces
  "class a<b> extends c<d> implements e, f {\n}")

(js2-deftest-parse class-expr-type-params
  "var a = class a<b> {\n};")

(js2-deftest-parse class-expr-type-params-no-name
  "var a = class <b> {\n};")

(js2-deftest-parse class-expr-type-params-no-name-with-extends-and-interfaces
  "var a = class <b> extends c<d> implements e, f {\n};")

(js2-deftest-parse class-prop-basic
  "class a {
  a = 42
}")

(js2-deftest-parse class-prop-type
  "class a {
  a: b
}")

(js2-deftest-parse class-prop-type-with-init
  "class a {
  a: b = 42
}")

(js2-deftest-parse class-prop-type-readonly
  "class a {
  +a: b
}")

(js2-deftest-parse class-prop-type-writeonly
  "class a {
  -a: b
}")

;; (js2-deftest-parse class-prop-type-mixed
;;   "class a {
;;   +a: b;
;;   -c;
;;   d
;; }")

(js2-deftest-parse class-prop-static-type-readonly
  "class a {
  static +a: b
}")

(js2-deftest-parse class-method-type
  "class a {
  a(b: c): d {}
}")

(js2-deftest-parse class-method-type-with-type-params
  "class a {
  b<c>(d: e): f {}
}")

(js2-deftest-parse class-method-type-with-named-type-params
  "class a {
  b<c: d>(e: f): g {}
}")

(js2-deftest-parse class-method-type-getset
  "class a {
  get a(b: c): d {}
}")

(js2-deftest-parse class-method-type-getset-readonly-static
  "class a {
  static +get a(b: c): d {}
}")

;; Modules

(js2-deftest-parse import-default
  "import type from 'm';")

(js2-deftest-parse import-type-default
  "import type type from 'm';")

(js2-deftest-parse import-typeof-default
  "import typeof typeof from 'm';")

(js2-deftest-parse import-default-as-local
  "import foo as type from 'm';")

(js2-deftest-parse import-type-default-as-local
  "import type as as type from 'm';")

(js2-deftest-parse import-default-as-local-type-as-name
  "import type as as from 'm';"
  :bind ((js2-ti-max-lookahead 3)))

(js2-deftest-parse import-type-default-as-local-as-as-name
  "import type as as as from 'm';"
  :bind ((js2-ti-max-lookahead 4)))

(js2-deftest-parse import-namespace
  "import * as as from 'm';")

(js2-deftest-parse import-type-namespace
  "import type * as as from 'm';")

(js2-deftest-parse import-named
  "import {a as b} from 'm';")

(js2-deftest-parse import-type-named
  "import type {a as b} from 'm';")

(js2-deftest-parse import-type-named-2
  "import type {a as b, c, d as e} from 'm';")

(js2-deftest-parse import-type-default-and-named
  "import type a, {b as c} from 'm';")

(js2-deftest-parse import-type-default-and-namespace
  "import type a, * as b from 'm';")

(js2-deftest-parse import-inline-type-named
  "import {type type} from 'm';")

(js2-deftest-parse import-inline-type-named-2
  "import {type as as} from 'm';"
  :bind ((js2-ti-max-lookahead 3)))

(js2-deftest-parse import-inline-type-named-3
  "import {type as as as} from 'm';"
  :bind ((js2-ti-max-lookahead 4)))

(js2-deftest-parse import-inline-type-named-4
  "import {type, type as, type as as1, type as as as2} from 'm';"
  :bind ((js2-ti-max-lookahead 4)))

(js2-deftest-parse import-inline-type-named-with-kind
  "import type {type type} from 'm';"
  :syntax-error "type")

(js2-deftest-parse export-all
  "export * from 'm';")

(js2-deftest-parse export-type-all
  "export type * from 'm';")

(js2-deftest-parse export-typeof-all
  "export typeof * from 'm';"
  :syntax-error "*"
  :errors-count 3)

(js2-deftest-parse export-named
  "export {type};")

(js2-deftest-parse export-type-named
  "export type {type} from 'm';")

(js2-deftest-parse export-type-named-2
  "export type { type type } from 'm';"
  :syntax-error "type"
  :errors-count 4)

;; Interface

(js2-deftest-parse interface
  "interface a {}")

(js2-deftest-parse interface-with-props
  "interface a {b: c; d: e}")

(js2-deftest-parse interface-with-type-params
  "interface a<b> {}")

(js2-deftest-parse interface-with-export
  "export interface a<b> {}")

;; Type aliases

(js2-deftest-parse type-alias
  "type a = b;")

(js2-deftest-parse type-alias-with-generic-type
  "type a = b<c<d<e>>>;")

(js2-deftest-parse type-alias-with-type-params
  "type a<b> = c;")

(js2-deftest-parse type-alias-with-export
  "export type a<b> = c;")

;; Opaque type aliases

(js2-deftest-parse opaque-type-alias
  "opaque type a = b;")

(js2-deftest-parse opaque-type-alias-with-type-params
  "opaque type a<b> = c;")

(js2-deftest-parse opaque-type-subtyping
  "opaque type a<b>: c = d;")

(js2-deftest-parse opaque-type-subtyping-no-binding
  "opaque type a: b;"
  :syntax-error ";"
  :errors-count 2)

(js2-deftest-parse opaque-type-alias-with-export
  "export opaque type a<b> = c;")

(js2-deftest-parse opaque-type-alias-subtype-with-export
  "export opaque type a<b>: c = d;")

;; Type casing expr

;; (js2-deftest-parse type-casing-expr-value
;;   "(a: b);")

;; (js2-deftest-parse type-casing-expr-value-2
;;   "(1 + 1: a);")

;; (js2-deftest-parse type-casing-expr
;;   "let a = (b: c);")

;; (js2-deftest-parse type-casing-expr-2
;;   "let a = { b: (c: d) };")

;; (js2-deftest-parse type-casing-expr-3
;;   "let a = ([(b: c), (d: c)]: e<c>);")

;; Declares

;; (js2-deftest-parse declare-variable
;;   "declare var a = b;")

;; (js2-deftest-parse declare-type-alias
;;   "declare type a = b;")

;; (js2-deftest-parse declare-function
;;   "declare function a(): b;")

;; (js2-deftest-parse declare-class
;;   "declare class a {\n}")

;; (js2-deftest-parse declare-module
;;   "declare module a {\n}")

;; (js2-deftest-parse declare-module-string-name
;;   "declare module 'a' {}")

;; (js2-deftest-parse declare-export
;;   "declare export var a;")

;; (js2-deftest-parse declare-cmd-exports
;;   "declare module.exports: a;")

;; Utilities

;;; Conflicts

(js2-deftest-parse type-as-variable-name
  "const type = 42;")

(js2-deftest-parse opaque-as-variable-name
  "const opaque = 42;")

(js2-deftest-parse type-as-caller-argument
  "func(type, type.a);")

(js2-deftest-parse type-as-caller-argument-2
  "(type) => {  return type;\n};")

(js2-deftest-parse switch-case-colon
  "\
switch (a) {
case b:
  break;
}")

(js2-deftest-parse object-method-with-type-params
  "var a = {b<c>() {}};")

(js2-deftest-parse jsx
  "function foo() {\n  return (<b></b>);\n}")

(js2-deftest-parse jsx-2
  "() => {  <b></b>;\n};")

(js2-deftest-parse export-default-as-named
  "export {default as foo} from 'm';")

;;; Babylon plugins and Stage-0

(js2-deftest-parse dynamic-import
  "import('foo')")

(js2-deftest-parse dynamic-import-with-expr
  "import(foo + 2)")

(js2-deftest-parse dynamic-import-chain-then
  "import(foo + 2).then()")

;; (js2-deftest-parse dynamic-import-with-comments
;;   "import(/* comment */ foo + 2)")


;;; Throw expr

(js2-deftest-parse throw-expr-assign
  "var a = throw new Error();")

(js2-deftest-parse throw-expr-arrow-function
  "() => {  throw new Error();\n};")


;;; Decorators

(js2-deftest-parse class-stmt-decorators
  "@a class b {\n}")
