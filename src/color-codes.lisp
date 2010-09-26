;;; color-codes.lisp

(in-package :lispmud)

;; color codes were taken from in http://tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html

;; Black       0;30     Dark Gray     1;30
;; Blue        0;34     Light Blue    1;34
;; Green       0;32     Light Green   1;32
;; Cyan        0;36     Light Cyan    1;36
;; Red         0;31     Light Red     1;31
;; Purple      0;35     Light Purple  1;35
;; Brown       0;33     Yellow        1;33
;; Light Gray  0;37     White         1;37

(defvar *cc-reset*  (coerce #(#\Esc #\[ #\0 #\m) 'string))

(defvar *cc-black*  (coerce #(#\Esc #\[ #\0 #\3 #\0 #\m) 'string))
(defvar *cc-red*    (coerce #(#\Esc #\[ #\0 #\3 #\1 #\m) 'string))
(defvar *cc-green*  (coerce #(#\Esc #\[ #\0 #\3 #\2 #\m) 'string))
(defvar *cc-brown*  (coerce #(#\Esc #\[ #\0 #\3 #\3 #\m) 'string))
(defvar *cc-blue*   (coerce #(#\Esc #\[ #\0 #\3 #\4 #\m) 'string))
(defvar *cc-purple* (coerce #(#\Esc #\[ #\0 #\3 #\5 #\m) 'string))
(defvar *cc-cyan*   (coerce #(#\Esc #\[ #\0 #\3 #\6 #\m) 'string))
(defvar *cc-lgrey*  (coerce #(#\Esc #\[ #\0 #\3 #\7 #\m) 'string))

(defun color (color-code &optional (stream *standard-output*))
  (write-string color-code stream))
