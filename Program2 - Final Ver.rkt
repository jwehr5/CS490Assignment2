#lang racket
(require racket/hash)

;This function takes the source file, removes punctuation, and creates a hash of all the words and their associated count.
(define (profile-text file)

;Reads a file 
(define (file-contents)
    (port->string (open-input-file file) #:close? #t))

;Removes punctuation from the file and converts it to downcase
(define (remove-punctuation file)
 (string-downcase
  (string-replace
  (string-replace
   (string-replace
    (string-replace
     (string-replace
      (string-replace
       (string-replace
        (string-replace
         (string-replace
          (string-replace
           (string-replace
            (string-replace
             (string-replace
              (string-replace (string-replace file ")" " ") "(" " ") "\"" " ") "“" " ") "—" " ") ";" " ") ":" " ") "-" " ") "!" " ") "?" " ") "." " ") "," " ") "”" " ") "\r" " ") "\n" " ")))

; Create a hash table of every word along with its associated count.
(define (list-to-hash)

  (define (create-hash hash-table lst)
  (if (empty? lst)
      hash-table
      (cond
        [(hash-has-key? hash-table (first lst)) (create-hash (hash-update hash-table (first lst) add1) (rest lst))]
        [else (create-hash (hash-union hash-table (hash (first lst) 1)) (rest lst))])))

  ;When we call the create-hash function go ahead and create a hash and put the first word in it and pass that hash as a parameter.
  (create-hash (hash (first (string-split (remove-punctuation (file-contents)))) 1) (rest(string-split (remove-punctuation (file-contents))))))

;Return the hash
(list-to-hash)
  
 ); End of profile-text function




;This function takes the hash table and calculates the relative frequency for all the values. It returns the updated hash table.
(define (relative-freq-hash hash-table)

  (define (add value-list)
    (if (empty? value-list)
      0
      (+ (add (rest value-list)) (first value-list))))

  (define (relative-freq key value )
    (values key(* (log (/ value (add (hash-values hash-table)) ) 10) -1)))

  (hash-map/copy hash-table relative-freq))


  #|
  This function takes a hash of a mystery text and a hash of a known author text.
  If the parameter find-num-common-words is #t then the inner function, num-common-words will run which will find the number of common words.
  If the parameter find-num-common-words is #f then the innder function, compare will run which will calculate the absolute value of the differnce of the frequency scores for each common word.
  |#
 (define (compare-texts mystery-text-hash known-text-hash find-num-common-words?)

   (define (compare mystery-text-keys)
     (if (empty? mystery-text-keys)
         0
         (if (hash-has-key? known-text-hash (first mystery-text-keys))
             (+ (abs (- (hash-ref mystery-text-hash (first mystery-text-keys)) (hash-ref known-text-hash (first mystery-text-keys)))) (compare (rest mystery-text-keys)))
             (compare (rest mystery-text-keys)))))

   (define (num-common-words mystery-text-keys)
     (if (empty? mystery-text-keys)
      0
      (if (hash-has-key? known-text-hash (first mystery-text-keys))
          (+ 1 (num-common-words (rest mystery-text-keys)))
          (+ 0 (num-common-words (rest mystery-text-keys))))))

   (if (equal? find-num-common-words? #t)
       (num-common-words (hash-keys mystery-text-hash))
       (compare (hash-keys mystery-text-hash))))


;Main Program

;Get the frequency hash for all the texts.
(define mystery1-freq-hash (relative-freq-hash (profile-text "mystery1.txt")))
(define mystery2-freq-hash (relative-freq-hash(profile-text "mystery2.txt")))
(define doyle-freq-hash (relative-freq-hash (profile-text "Doyle.txt")))
(define lovecraft-freq-hash (relative-freq-hash (profile-text "Lovecraft.txt")))

;Find the average between each mystery text and known author text.
(define mystery1-doyle-avg (/ (compare-texts mystery1-freq-hash doyle-freq-hash #f) (compare-texts mystery1-freq-hash doyle-freq-hash #t)))
(define mystery1-lovecraft-avg (/ (compare-texts mystery1-freq-hash lovecraft-freq-hash #f) (compare-texts mystery1-freq-hash lovecraft-freq-hash #t)))
(define mystery2-doyle-avg (/ (compare-texts mystery2-freq-hash doyle-freq-hash #f) (compare-texts mystery2-freq-hash doyle-freq-hash #t)))
(define mystery2-lovecraft-avg (/ (compare-texts mystery2-freq-hash lovecraft-freq-hash #f) (compare-texts mystery2-freq-hash lovecraft-freq-hash #t)))

;Compare the averages.
(display "Analyzing mystery text 1... ")
(if (> mystery1-doyle-avg mystery1-lovecraft-avg)
    (display "Mystery text 1 is probably Lovecraft \n")
    (display "Mystery text 1 is probably Doyle \n"))


(display "Analyzing mystery text 2... ")
(if (> mystery2-doyle-avg mystery2-lovecraft-avg)
    (display "Mystery text 2 is probably Lovecraft \n")
    (display "Mystery text 2 is probably Doyle \n"))



  

        




