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

(defstruct (field (:constructor make-field (seed
                                            radius
                                            limit
                                            tor
                                            app)))
;;  (area (make-array (list radius radius)))
  (prng (mersenne:make-mt seed))
  (limit limit)
  (torus (equal tor 1))
  (radius radius)
  (apples ())
  (snake (let ((c 0))
	   (make-list *init-length* :initial-element (list c c))))
  (step 0)
  ;;(score 0)
  (path '())
  (facing *west*))

(defun new-field (seed radius limit torus apple)
  (let ((field (make-field (1+ seed) radius limit torus apple)))
    (when (and (numberp apple) (> apple 0))
      (spawn-apples field apple))
    field))

(defun deadp (field)
  (when (> (field-step field) *init-length*)
    ;; every snake gets a grace period equal to its init-length
    (or
     (and (> (field-step field) (* 2 (field-radius field)))
          (= (car (get-score field)) 1))
     ;; time limit has been reached
     (>= (field-step field) (field-limit field))
     ;; snake has collided with itself
     (let ((collides (hit-obstacle-p (car (field-snake field)) field)))
       ;; hit-obstacle-p returns t if the head's hit the edge,
       ;; a list if the head's hit itself,
       ;; and nil if the head's ok.
       (and collides
            (or (null (field-torus field))
                (listp collides)))))))

(defun move (pos dir) 
  (mapcar #'+ pos dir))

(defun field-move (field)
  (push (move (car (field-snake field)) (field-facing field))
        (field-snake field)))

(defun field-torus-move (field)
  (field-move field)
  (let ((head-x (caar (field-snake field)))
        (head-y (cadar (field-snake field))))
    (when (>= (abs head-x) (field-radius field)
       (setf (caar (field-snake field)) (- head-x))))
    (when (>= (abs head-y) (field-radius field))
       (setf (cadar (field-snake field)) (- head-y)))))

(defun rnd-pos (field)
  (labels ((%gen () (mod (mersenne:mt-gen (field-prng field))
                              (field-radius field)))
           (gen ()
             (let ((g (- (%gen) (%gen))))
               (if (zerop g)
                   (gen)
                   g))))
    (list (gen) (gen))))

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
      (if (field-torus field)
          (field-torus-move field)
          (field-move field))
      (if (hit-apple-p head field)
          (progn
            (setf (field-step field) (1+ *init-length*))
            (delete head (field-apples field) :test #'equal)
            (push (rnd-pos field) (field-apples field)))
          (setf (field-snake field)
                (butlast (field-snake field)))))))

(defun remaining-steps (field)
  (- (field-limit field) (field-step field)))

(defun spawn-apples (field n)
  (setf (field-apples field)
        (loop repeat n collect (rnd-pos field))))

;; mostly just for testing
(defun random-run (seed radius)
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
			      ((member `(,x ,y)
                     (field-apples field)
                     :test #'equal)
			       #\@)
			      (:otherwise #\.))))
		(vector-push ch s)
		(when (= x (1- radius))
		  (vector-push #\Newline s)))))
    (format t "~a" s)))

(defun p-dist (p1 p2)
  (list (- (car p1) (car p2))
	(- (cadr p1) (cadr p2))))

(defun turn-snake (field vals)
  (setf (field-facing field)
        (turn (field-facing field) (which-turn vals))))

(defun turn (facing turn)
  (let* ((nd (length *directions*))
         (idx (position facing *directions* :test #'equal)))
    (elt *directions* (mod (+ idx turn) nd))))

(defun turn-facing (turn field)
  (setf (field-facing field)
        (elt *directions* (mod
                           (+ turn
                              (position (field-facing field)
                                        *directions*
                                        :test #'equal))
                           (length *directions*)))))

(defun hit-obstacle-p (head field)
  (or (<= (field-radius field) (apply #'max (mapcar #'abs head)))
      (member head (cdr (field-snake field)) :test #'equal)))

(defun hit-apple-p (head field)
  (member head (field-apples field) :test #'equal))

(defun distance-dir (head dir field &key (probe #'hit-obstacle-p))
  (labels ((counter (head field n)
             (cond ((funcall probe head field) n)
                   ((> n (field-radius field)) 0)
                   (t (counter (move head dir) field (1+ n))))))
    (counter head field 0)))

(defun probe-distance (field &key (probe #'hit-obstacle-p))
  (let* ((forward (field-facing field))
         (rightward (turn forward +1))
         (leftward (turn forward -1))
         (head (car (field-snake field))))
    (mapcar (lambda (x) (distance-dir head x field :probe probe))
            (list leftward forward rightward))))

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


(defun which-turn (vals)
  (if (= (length (remove-duplicates vals)) 1) 0
      (let* ((idxs '(0 1 2))
	     (i (reduce (lambda (x y)
			  (if (> (elt vals x) (elt vals y)) x y))
			idxs)))
	(elt '(-1 0 1) i))))


(defun get-score (field)
  (list
   (max 1
        (min #xFFFFFFFF
             (round (expt (max 1
                               (- (length (remove-duplicates (field-path field)
                                                             :test #'equal))
                                  (1- (field-radius field))))
                          (/ (length (field-snake field))
                             *init-length*)))))))

(defun encode-packet (field)
  (let ((vals (cond
		((deadp field)
		 (concatenate 'list
			      (cdr (assoc 'score *packet-types*))
			      (get-score field)))
		(:otherwise
		 (concatenate 'list
			      `(,*input-hdr*)
            (probe-distance field :probe #'hit-obstacle-p)
            (probe-distance field :probe #'hit-apple-p))))))
    (coerce (cons (car vals)
		  (apply #'append (mapcar #'dword->bytes (cdr vals))))
	    'vector)))
       
;;;; rough sockety stuff
(defun okay-packet (w)
  (cdr (assoc 'okay *packet-types*)))

(defun create-server (port)
  (let ((field nil)
        (connection nil)
        (params nil)
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
       (setf params (cdr words))
			 (setf field (apply #'new-field (cdr words)))
			 (if (zerop (car words))
			     (setf dbg nil)
			     (progn
			       (setf dbg (car words))
			       (print-field field)
             (format t "PARAMS: ~S~%" params)
			       (sleep (/ 1 dbg))))
			 (encode-packet field))
			((eq typ 'output)
       (let ((r nil))
         (tick (which-turn words) field)
         (setf r (encode-packet field))
         (when dbg
           (print-field field)
           (format t "SCORE: ~D~%STEPS LEFT: ~D~%"  (car (get-score field)) (remaining-steps field))
           (format t "REPLY: ~S~%" r)
           (sleep (/ 1 dbg)))
         r))
			(t (format t "ERROR: Unrecognized packet HDR: ~X~%" hdr))))
		;; send back results as input pkt
		(write-sequence reply stream)
		(force-output stream)
		(usocket:socket-close connection))))))

