;;; File:
;;;   is-coordinate.scm
;;; Author:
;;;   Samuel A. Rebelsky, rebelsky@grinnell.edu
;;; Summary:
;;;   A representation of x and y coordinates for the image-scheme
;;;   library for MediaScheme.
;;; Contents:
;;;   _coord - Create a new coordinate (without parameter checking)
;;;   coord - Create a new coordinate (with parameter checking)

;;; Procedures:
;;;   _coord
;;;   coord
;;; Parameters:
;;;   dim, the symbol 'x or the symbol 'y
;;;   value, a real number
;;; Purpose:
;;;   To build a coordinate object that responds to the "normal" 
;;;   operations.
;;; Produces:
;;;   coord, a Coordinate

(define _coord
  (lambda (dim value)
    (let ((self.dim dim)
          (self.value value))
      (let ((self.->string (lambda ()
                       (string-append (symbol->string self.dim)
                                      "="
                                      (number->string self.value)))))
        (lambda (message . params)
          (cond 
            ; Observers
            ((eq? message ':->string)
             (self.->string))
            ((eq? message ':value)
             self.value)
            ; Mutators
            ((eq? message ':hscale!)
             (when (eq? self.dim 'x)
               (set! self.value (* (car params) self.value))))
            ((eq? message ':vscale!)
             (when (eq? self.dim 'v)
               (set! self.value (* (car params) self.value))))
            ((eq? message ':rotate!)
             (cond
               ((eq? self.dim 'x)
                (set! self.dim 'y)
              (set! self.value (- self.value)))
             (else
              (set! self.dim 'y))))
            ; Default
            (else
             (error "Coordinate " (self.->string) 
                    "invalid message:" message))))))))
              
(define coord
  (lambda (dim value)
    (when (not (member dim '(x y)))
      (error "coord: Invalid dimension:" dim))
    (when (not (number? value))
      (error "coord: Invalid coordinate:" value))
    (_coord dim value)))
