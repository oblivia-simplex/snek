;; prepare for potential multithreading
;; keep logic cleanly separate from i/o


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Mersenne Twister                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (mt (:constructor make-mt (seed)))
  (index 624)
  (ar (let ((a (make-array '(624))))
	(setf (aref a 0) seed)
	(loop for i from 1 to 623 do
	     (setf (aref a i)
		   (%mt-next (aref a (1- i)) i)))
	a)))

(defun %mt-next (prev i)
  (logand
   #xFFFFFFFF
   (+ (* #x6C078965 (logxor prev (ash prev -30))) i)))

(defun %mt-twist (mt)
  (loop for i from 0 to 623 do
     ;; get the msb and add it to lsb of next
       (let ((y (logand
		 #xFFFFFFFF
		 (logand
		  #x80000000
		  (+ (aref (mt-ar mt) i)
		     (logand
		      #x7FFFFFFF
		      (aref (mt-ar mt) (mod (1+ i) 624))))))))
	 (setf (aref (mt-ar mt) i)
	       (ash
		(logxor
		 (aref (mt-ar mt) (mod (+ i 397) 624))
		 y)
		-1))
	 (if (oddp y)
	     (setf (aref (mt-ar mt) i)
		   (logxor
		    (aref (mt-ar mt) i)
		    #x9908B0DF))
	     nil)
	 (setf (mt-index mt) 0)))
  mt)

(defun mt-gen (mt)
  (let ((mt (if (>= (mt-index mt) 624)
		(%mt-twist mt)
		mt)))
    (let ((y (aref (mt-ar mt) (mt-index mt))))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (logand (ash y 7)  #x9D2C5680)))
      (setf y (logxor y (logand (ash y 15) #xEFC60000)))
      (setf y (logxor y (ash y -18)))
      (incf (mt-index mt))
      (values (logxor y #xFFFFFFFF)
	      mt))))
  
  
