;; prepare for potential multithreading
;; keep logic cleanly separate from i/o
;;(load "/home/oblivia/Projects/snek/mersenne.lisp")

(in-package :snek)

(defvar *stay*  '(0  0))
(defvar *north* '(0 -1))
(defvar *east*  '(1  0))
(defvar *south* '(0  1))
(defvar *west*  '(-1 0))
(defvar *directions* (list *stay* *north* *east* *south* *west*))

(defvar *init-length* 3)

(defstruct (field (:constructor make-field (radius seed)))
;;  (area (make-array (list radius radius)))
  (prng (mersenne:make-mt seed))
  (radius radius)
  (apple `(,(floor (/ radius 4)) ,(floor (/ radius 4))))
  (snake (let ((c 0)) 
	   (make-list *init-length* :initial-element (list c c))))
  (step 0)
  (facing *west*))

(defun deadp (field)
  (if (> (field-step field) *init-length*)
      (or
       ;; snake has collided with itself
       (< (length (remove-duplicates (field-snake field)))
	  (length (field-snake field)))
       ;; snake has hit edge of field
       (let ((lo (apply #'min (car (field-snake field))))
	     (hi (apply #'max (car (field-snake field)))))
	 (or (<= lo (- (field-radius field)))
	     (>= hi (field-radius field)))))
      nil))

(defun move (pos dir) 
  (mapcar #'+ pos dir))

(defun rnd-pos (field)
  (flet ((gen () (mod (mersenne:mt-gen (field-prng field)) (* 2 (field-radius field)))))
    (list (- (field-radius field) (gen))
	  (- (field-radius field) (gen)))))

(defun rnd-dir (field)
  (flet ((gen () (mod (mersenne:mt-gen (field-prng field)) 5)))
    (elt *directions* (gen))))

(defun tick (field)
  (incf (field-step field))
  ;; advance game state by one tick
  (let ((head (car (field-snake field))))
    (push (move head (field-facing field))
	  (field-snake field))
    (if (equal (car (field-snake field))
	       (field-apple field))
	(spawn-apple field)
	(setf (field-snake field)
	      (butlast (field-snake field))))))

(defun spawn-apple (field)
  (format t "** spawning apple! **~%")
  (setf (field-apple field) (rnd-pos field)))

;; mostly just for testing
(defun random-run (radius seed)
  (let ((field (make-field radius seed)))
    (loop until (deadp field)
       collect (progn
		 (setf (field-facing field) (rnd-dir field))
		 (tick field)
		 (print-field field)
		 (format t "~%")))))

(defun print-field (field)
  (let* ((radius (field-radius field))
	 (s (make-array (expt (* 2 radius) 2)
			:element-type 'character
			:adjustable t
			:fill-pointer 0)))
    (loop
       for y from (- (1- radius)) to (1- radius) do
	 (loop for x from (- (1- radius)) to (1- radius) do
	      (let ((ch (cond ((member `(,x ,y)
				       (field-snake field)
				       :test #'equal)
			       #\*)
			      ((equal `(,x ,y)
				      (field-apple field))
			       #\@)
			      (:otherwise #\.))))
		(vector-push ch s)
		(when (= x (1- radius))
		  (vector-push #\Newline s)))))
    (format t "~a" s)))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Network Interface                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; output is output from the controller
;; input is input to the controller
;; so output, here, is input for the game
;; and input, here, is output from the game
(defparameter *packet-types*
  '(hello input score output params))

(defun le-dword (vec offset)
  (let ((dword 0))
    (loop for i below 4 do
	 (incf dword
	       (ash (aref vec (+ i offset)) (* i 8))))
    dword))

(defun neg32p (n)
  (/= 0 (logand #x80000000 n)))

(defun which-facing (body)
  (flet ((dir (w)
	   (cond ((zerop w) 0)
		 ((neg32p w) -1)
		 (:otherwise 1))))
    (list (dir (car body)) (dir (cadr body)))))

(defun decode-packet (pkt)
  ;; pkt is a byte vector
  (let* ((hdr (aref pkt 0))
	 (typ (elt *packet-types* (ldb (byte 4 4) hdr)))
	 (len (ldb (byte 4 0) hdr))
	 (body (subseq pkt 1))
	 (vals (loop for i below len collect
		    (le-dword body (* i 4)))))
    (case typ
      (output (list typ (which-facing vals)))
      (params (list typ vals))
      (:otherwise (list typ vals)))))
    
