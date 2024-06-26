;; lo scopo è fare tipo cal
;; ma in lisp
;; il primo gennaio 1970 è stato un giovedì

(defun divides-p (divisor divided)
  (declare (fixnum divisor divided))
  (= 0 (mod divided divisor)))

(defconstant seconds-in-minute 60)
(defconstant seconds-in-hour 3600)
(defconstant seconds-in-day 86400)

(defclass date ()
  ((year :initarg :year :accessor date-year :type fixnum)
   (month :initarg :year :accessor date-year :type fixnum)
   (day :initarg :year :accessor date-year :type fixnum)))

(defclass hour ()
  ((hour :initarg :hour :accessor date-year :type fixnum)
   (minute :initarg :minute :accessor date-year :type fixnum) 
   (second :initarg :second :accessor date-year :type fixnum)))

;; how do I update this shit? idfk
;; table taken from https://en.wikipedia.org/wiki/Leap_second
(defparameter *leap-second-month-list*
  '(
    (1 . 1972) (12 . 1972)
               (12 . 1973)
               (12 . 1974)
               (12 . 1975)
               (12 . 1976)
               (12 . 1977)
               (12 . 1978)
    (1 . 1981)
    (1 . 1982)
    (1 . 1983)
    (1 . 1985)
               (12 . 1987)
               (12 . 1989)
               (12 . 1990)
    (1 . 1992)
    (1 . 1993)
    (1 . 1994)
               (12 . 1995)
    (1 . 1997)
               (12 . 1998)
               (12 . 2005)
               (12 . 2008)
    (1 . 2012)
    (1 . 2015)
               (12 . 2016)
    )
  )

(defun month-has-leap-second (month year)
  "does the month `month' of year `year' have a leap second added at the end of it?"
  (position (cons month year) *leap-second-month-list* :test #'equal))

(defun leap-p (year)
  "https://en.wikipedia.org/wiki/Leap_year"
  (or (divides-p 400 year)
      (and (divides-p 4 year)
           (not (divides-p 100 year)))))

(defun seconds-in-year (year)
  (let ((leap-seconds
          (count-if
           (lambda (x) (= (cdr x) year))
           *leap-second-month-list*))
        (day-seconds
          (if (leap-p year)
              (* 366 seconds-in-day)
              (* 365 seconds-in-day))))
    (+ day-seconds leap-seconds)))

(defun days-in-month (month year)
  "30 giorni ha novembre, con april, giungo, e settembre, di 28 ce n'è solo uno, tutti gli altri ne han 31"
  (case month
    ((11 4 6 9) 30)
    ((2) (if (leap-p year) 29 28))
    (otherwise 31)))

(defun seconds-in-month (month year)
  (+ (* seconds-in-day (days-in-month month year))
     (if (month-has-leap-second month year) 1 0)))

(defun last-in-month-p (day month year)
  (= day (days-in-month month year)))

(defun day-has-leap-second-p (day month year)
  (and (last-in-month-p day month year)
       (month-has-leap-second month year)))

(defun seconds-in-day (day month year)
  (+ seconds-in-day
     (if (day-has-leap-second-p day month year) 1 0)))

(defun time-year (seconds)
  "https://exercism.org/tracks/common-lisp/concepts/date-time, time is expected to come from (get-universal-time)"
  (declare (fixnum seconds))
  (do* ((year 1900 (1+ year))
        (seconds-left seconds (- seconds-left (seconds-in-year year))))
    ((<= seconds-left (seconds-in-year (1+ year))) (values year seconds-left))))

(defun time-year-month (year seconds-left)
  (declare (fixnum year seconds-left))
  (do* ((month 1 (1+ month))
        (seconds-left seconds-left (- seconds-left (seconds-in-month month year))))
    ((<= seconds-left (seconds-in-month (1+ month) year)) (values month seconds-left))))

(defun time-year-month-day (year month seconds-left)
  (declare (fixnum year seconds-left))
  (do* ((day 1 (1+ day))
        (seconds-left seconds-left (- seconds-left (seconds-in-day day month year))))
    ((<= seconds-left (seconds-in-day (1+ day) month year)) (values day seconds-left))))

(defun /mod (a b) (values (floor (/ a b)) (mod a b)))

(defun time-seconds (seconds)
  (multiple-value-bind (hours left) (/mod seconds seconds-in-hour)
    (multiple-value-bind (minutes left) (/mod left seconds-in-minute)
      (values hours minutes left))))

(defun time-date (seconds)
  (declare (fixnum seconds))
  (multiple-value-bind (year left) (time-year seconds)
    (multiple-value-bind (month left) (time-year-month year left)
      (multiple-value-bind (day left) (time-year-month-day year month left)
        (multiple-value-bind (hours minutes seconds) (time-seconds left)
        (format nil "~A/~A/~A : ~A:~A - ~A"
                year month day
                hours minutes seconds))))))
    

