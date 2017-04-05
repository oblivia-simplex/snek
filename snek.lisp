;; prepare for potential multithreading
;; keep logic cleanly separate from i/o
(load "mersenne.lisp")

(defvar *north* '(0 -1))
(defvar *east*  '(1  0))
(defvar *south* '(0  1))
(defvar *west*  '(-1 0))
(defvar *directions* (list *north* *east* *south* *west*))

(defvar *init-length* 3)

(defstruct (field (:constructor make-field (size seed)))
;;  (area (make-array (list size size)))
  (prng (make-mt seed))
  (size size)
  (apple `(,(floor (/ size 4)) ,(floor (/ size 4))))
  (snake (let ((c (floor (/ size 2))))
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
	 (or (< lo 0)
	     (>= hi (field-size field)))))
      nil))

(defun move (pos dir) 
  (mapcar #'+ pos dir))

(defun rnd-pos (field)
  (flet ((gen () (mod (mt-gen (field-prng field)) (field-size field))))
    (list (gen) (gen))))

(defun rnd-dir (field)
  (flet ((gen () (mod (mt-gen (field-prng field)) 4)))
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

(defun random-run (size seed)
  (let ((field (make-field size seed)))
    (loop until (deadp field)
       collect (progn
		 (setf (field-facing field) (rnd-dir field))
		 (tick field)))))
