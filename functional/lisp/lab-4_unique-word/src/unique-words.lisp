;;; HELPERS

(defmacro list-diff (a b)
  `(set-difference ,a ,b :test #'equal))

(defmacro list-union (a b)
  `(union ,a ,b :test #'equal))

;;Delimiter characters
(defun word-delimiter-p (c) (position c " ,.;/[]()\"'"))
(defun sentence-delimiter-p (c) (position c ".?!~%"))

;;Splitter fuction, takes delimiter predicate and a string as the parameters
(defun split-by (str delimiterp)
  (loop for begin = (position-if-not delimiterp str)
    then (position-if-not delimiterp str :start (1+ end))
    for end = (and begin (position-if delimiterp str :start begin))
    when begin :collect (subseq str begin end)
    while end))

(defun split-sentences (str)
  (split-by str #'sentence-delimiter-p))

(defun split-words (str)
  (split-by str #'word-delimiter-p))

;;; MAIN

;; Main loop
(defun main (x)
  (let ((first-sentence-found nil)
        (uniq nil))
      (progn
        ; Processing line by line
        (loop for line = (read-line x nil)
        while line
        do (if first-sentence-found
               ; Just searching the unique words
               (setq uniq (list-diff uniq (split-words line)))
               ; Split by the sentence deliminer, if . found,
               ; and the sentence is not empty,
               ; start to search for unique words
               (let ((tmp (split-sentences line)))
                    ; Don't include empty first sentence, like "."
                    (if tmp
                      (if (and (= 1 (list-length tmp))
                          (not (position-if #'sentence-delimiter-p line)))
                      (setq uniq (list-union uniq (split-words (car tmp))))
                      (progn (setq first-sentence-found t) 
                        ; Before sentence deliminer
                        (setq uniq (list-union uniq (split-words (car tmp))))
                        ; After sentence deliminer
                        (setq uniq (list-diff uniq (mapcar #'split-words (butlast tmp))))))))))
        ; Result
        (format t "Unique words from the first sentence:~C~a~C" #\linefeed uniq #\linefeed))))

;; Start
; Getting the filename parameter
(format t "Please, enter the file name:~C" #\linefeed)
(defparameter *filename* (read-line))
; Opening the file 
(defparameter *file* (open *filename*))
; Calling the main loop
(main *file*)