;; prepare for potential multithreading
;; keep logic cleanly separate from i/o
;;(load "/home/oblivia/Projects/snek/mersenne.lisp")

(in-package :snek)

(defparameter *debug* t)

(defvar *north* '(0 -1))
(defvar *east*  '(1  0))
(defvar *south* '(0  1))
(defvar *west*  '(-1 0))
(defvar *directions* (list *north* *east* *south* *west*))
(defvar *init-length* 3)

(defun cls()
  (format t "~A[H~@*~A[J" #\escape))

(defstruct (field (:constructor make-field (radius seed limit)))
;;  (area (make-array (list radius radius)))
  (prng (mersenne:make-mt seed))
  (limit limit)
  (radius radius)
  (apple `(,(floor (/ radius 4)) ,(floor (/ radius 4))))
  (snake (let ((c 0)) 
	   (make-list *init-length* :initial-element (list c c))))
  (step 0)
  ;;(score 0)
  (path '())
  (facing *west*))

(defun new-field (radius seed limit)
  (let ((field (make-field radius seed limit)))
    (setf (field-facing field) (rnd-dir field))
    (setf (field-apple field) (rnd-pos field))
    field))

(defun deadp (field)
  (if (> (field-step field) *init-length*)
      (or
       (and (= (field-step field) 32)
	    (< (length (remove-duplicates (field-snake field)
					  :test #'equal))
	       5))
       ;; time limit has been reached
       (>= (field-step field) (field-limit field))
       ;; snake has collided with itself
       (< (length (remove-duplicates (field-snake field)
				     :test #'equal))
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
  (flet ((gen () (mod (mersenne:mt-gen (field-prng field)) 4)))
    (elt *directions* (gen))))

(defun tick (turn field)
  (unless (deadp field)
    (incf (field-step field))
;;    (incf (field-score field)
;;	  (if (zerop turn) (* 2 (- (length (field-snake field))
;;				   (1- *init-length*)))
;;	      (- (length (field-snake field))
;;		 (1- *init-length*))))
		 
    (turn-facing turn field)
    ;; advance game state by one tick
    (let ((head (car (field-snake field))))
      (push head (field-path field))
      (push (move head (field-facing field))
	    (field-snake field))
      (if (equal (car (field-snake field))
		 (field-apple field))
	  (spawn-apple field)
	  (setf (field-snake field)
		(butlast (field-snake field)))))))
  
(defun spawn-apple (field)
  (setf (field-apple field) (rnd-pos field)))

;; mostly just for testing
(defun random-run (radius seed)
  (let ((field (make-field radius seed)))
    (loop until (deadp field)
       collect (progn
		 (tick (1- (random 3)) field)
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
  '((okay    #x00)
    (input   #x10)
    (score   #x20)
    (output  #x30)
    (param   #x40)))

(defparameter *output-hdr* #x36)
(defparameter *input-hdr* #x16)

(defun bytes->dword (vec offset)
  (let ((dword 0))
    (loop for i below 4 do
	 (incf dword
	       (ash (aref vec (+ i offset)) (* i 8))))
    dword))

(defun dword->bytes (dword)
  (loop for i below 4 collect
       (ldb (byte 8 (* i 8)) dword)))

(defun neg32p (n)
  (/= 0 (logand #x80000000 n)))

(defun turn-snake (field vals)
  (setf (field-facing field)
	(turn (field-facing field) (which-turn vals))))

(defun turn (facing turn)
  (let* ((nd (length *directions*))
	 (idx (position facing *directions* :test #'equal)))
    (elt *directions* (mod (+ idx turn) nd))))


(defun which-turn (vals)
  (if (= (length (remove-duplicates vals)) 1) 0
      (let* ((idxs '(0 1 2))
	     (i (reduce (lambda (x y)
			  (if (> (elt vals x) (elt vals y)) x y))
			idxs)))
	(elt '(-1 0 1) i))))

(defun turn-facing (turn field)
  (setf (field-facing field)
	(elt *directions* (mod
			   (+ turn
			      (position (field-facing field)
					*directions*
					:test #'equal))
			   (length *directions*)))))

(defun get-score (field)
  (list
   (min #xFFFFFFFF (* (length (remove-duplicates (field-path field)
						 :test #'equal))
		      (length (field-snake field))))))
       ;(* (field-step field) (length (field-snake field))))))


(defun encode-packet (field)
  (let ((vals (cond
		((deadp field)
		 (concatenate 'list
			      (cdr (assoc 'score *packet-types*))
			      (get-score field)))
		(:otherwise
		 (concatenate 'list
			      `(,*input-hdr*)
			      (car (field-snake field))
			      (field-facing field)
			      (field-apple field))))))
    (coerce (cons (car vals)
		  (apply #'append (mapcar #'dword->bytes (cdr vals))))
	    'vector)))
       
;;;; rough sockety stuff
(defun okay-packet (w)
  (cdr (assoc 'okay *packet-types*)))
	  
(defun create-server (port)
  (let ((field nil)
	(connection nil)
	(socket (usocket:socket-listen "127.0.0.1" port
				       :reuse-address t
				       :element-type
				       '(unsigned-byte 8))))
    (loop
       while t
       with stream
       with buffer
       with words
       with reply
       with dbg
       do
	 (setq connection (usocket:socket-accept socket))
	 (setq stream (usocket:socket-stream connection))
	 (setq buffer (make-array 260))
	 (unwind-protect
	      (let* ((hdr (read-byte stream))
		     (typ (car (elt *packet-types*
				    (ldb (byte 4 4) hdr))))
		     (len (* 4 (ldb (byte 4 0) hdr))))
		(when dbg
		  (cls)
		  (format t "hdr: ~x, typ: ~s, len: ~d~%" hdr typ len))
		(read-sequence buffer stream :end len)
		(setf words (loop for i below (/ len 4) collect
				 (bytes->dword buffer (* i 4))))
		(when dbg
		  (format t "WORDS: ~S~%" words))
		;; branch according to typ
		;; either set up game, or send data to existing game
		(setf reply
		      (cond
			((eq typ 'param)
			 (setf field (apply #'new-field (cdr words)))
			 (if (zerop (car words))
			     (setf dbg nil)
			     (progn
			       (setf dbg (car words))
			       (print-field field)
			       (sleep (/ 1 dbg))))
			 (encode-packet field))
			((eq typ 'output)
			 (tick (which-turn words) field)
			 (when dbg
			   (print-field field)
			   (format t "SCORE: ~D~%" (get-score field))
			   (sleep (/ 1 dbg))
			   )
			 (encode-packet field))
			(t (format t "ERROR: Unrecognized packet HDR: ~X~%" hdr))))
		;; send back results as input pkt
		(write-sequence reply stream)
		(force-output stream)
		(usocket:socket-close connection))))
    
    
    (progn
      (format t "Closing socket~%")
      (usocket:socket-close socket))))
