;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Mersenne Twister                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :mersenne)

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
  
  
