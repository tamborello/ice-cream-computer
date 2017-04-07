;;;; A brief software exploration inspired by a game of crazy or bluetooth
;;;;
;;;; (C) 2012-7 Frank Tamborello
;;;;
;;;; License granted according to Attribution-Noncommercial-Sharealike Creative Commons 
;;;;
;;;; Thanks to The Mysterious WMATA Blue Line Rider, wvxvw at lispforum.com, Adam Tornhill, Peter Seibel, and Paul Graham for inspiration & guidance.
;;;;
;;;; λ
;;;;
;;;; To Use:
;;;; 1. Execute all
;;;; 2. Call (icc:start-server) to start accepting HTTP requests
;;;; 3. Call (icc:stop-server) to stop accepting HTTP requests
;;;;
;;;; Revision 30
;;;;
;;;; Revision History
;;;;
;;;; 30 2017.04.05
;;;; 1. Mystery can be fun in an art project, but the Ice Cream Computer has also become for me a portfolio piece of data analytic software engineering. Therefore it's time to actually explain a few things to users:
;;;;  a. On the compute page to enter a space-delimited list of ingredients to estimate the deliciousness probability of a single ice cream composed of those ingredients.
;;;;  b. On the rankings page that the per-ingredient values are: number of times the ingredient's appeared in a delicious ice cream, number of times the ingredient's appeared in a disgusting ice cream, probability that any ice cream containing this ingredient will be delicious. Note that some ingredients will more strongly indicate delicious or disgusting classification than others. 
;;;; 2. Having "nil NIL NIL NIL" appear in the rankings is nonsensical, even by the standards of The Ice Cream Computer. Remove that.
;;;;
;;;; 29. 2016.06.26
;;;; 1. See if I can roll back to revision 27.
;;;; 
;;;; 28. 2016.06.26
;;;; 1. Rolled back to revision 5, the random text generator, after parenscript had apparently broken.
;;;; 2. TIL that I don't have to run ICC as root if I use port 8080 instead of port 80, and of course NAT can forward port 80 requests to port 8080.
;;;; 3. AJAXified the colophon, which looks way better.
;;;;
;;;;
;;;; 27.	2015.07.10
;;;; Compute-score was crashing whenever it got unknown terms. Squashed.
;;;;
;;;; 26.	2015.07.10
;;;; Updated the front-end portion to classify ice creams.
;;;;
;;;; 25.	2015.07.10
;;;; Added a few functions from Practical Common Lisp's spam filter chapter
;;;; to finally complete the naive Bayes ice cream classifier!
;;;;
;;;; 24.	2015.02.11
;;;; 1. train-ing now also calls update-score so that the ingredient's score will be updated
;;;; from the train page or from the rankings page
;;;;
;;;; 23.	2015.02.11
;;;; Added a use header to this file and commented out the call to start-server at
;;;; the end. This should make it easier to make big changes: simply copy the entire
;;;; ice-cream-computer.lisp file to the server, reload, and if need be call start-server
;;;; with 80. 
;;;;
;;;; 22.	2015.02.11
;;;; Rudimentary Leaderboard, "Rankings"
;;;;
;;;; 21.	2015.02.11
;;;; Save scores rather than recompute every time
;;;;  1. Add a score slot to the feature class and the flavor document
;;;;  2. Compute ice cream page calls method update-score
;;;;  3. Update-score computes and saves the score
;;;;
;;;; 20		2015.02.11
;;;; 1. Altered the colophon text to better market myself and to better credit 
;;;; Seibel and Tornhill.
;;;;
;;;; See previous revisions for prior revision history.
;;;;
;;;;
;;;; Bugs
;;;; None known
;;;;
;;;;
;;;; To Do
;;;;
;;;; N. Parsing
;;;;  1. Parse comma-separated lists into co-occurrence stimuli, parsing multi-word items like "motor oil" as one item each. Currently the ICC parses space-delimited lists, so it can't understand that "motor oil" is one ingredient and not two.
;;;;  2. Throwout non-content words like conjunctions.
;;;;
;;;; N. Single-Page Web App 
;;;; Rather than reloading the entire page or another page on submit, rework the AJAX  
;;;; to update a div, a la ice cream computer 1. 
;;;;
;;;; N. Leaderboard
;;;;  1. user-selectable ranking by available criteria
;;;; I think I'll want AJAX for that.
;;;; 
;;;;  2. thumbs-up/down from the rankings page
;;;; How could that work? A script at the top with variables for icecream and 
;;;; flavor, and buttons on each row that fill in the values of those variables
;;;; and calls the function, which posts the data?
;;;;
;;;;  3. Separate rankings for ice creams rather than only for ingredients
;;;;
;;;; N. Integrate some of the Ice Cream Computer 1's functionality:
;;;; Randomly generate an ice cream from ingredients in the database and score it.
;;;; Let users enter those into the rankings.
;;;;


(ql:quickload '(cl-mongo cl-who hunchentoot parenscript cl-ppcre))




(defpackage :icc
  (:use :common-lisp :hunchentoot :cl-mongo :cl-who :parenscript)
  (:export :start-server :stop-server))
(in-package :icc)

(require :misc-lib.lisp)

;;;; Storage ;;;;
;; Connect to the database, ic-store ("ice cream storage")
;; host changed when we moved, used to be 192.168.2.9
(db.use "ic-store" :mongo (mongo :host "loki"))

(defparameter *ice-cream-collection* "icecreams")

;; Avoid duplicate entries
(defun unique-index-on (field)
  (db.ensure-index *ice-cream-collection*
                   ($ field 1)
                   :unique t))

(unique-index-on "INGREDIENT")

(defclass totals ()
  ((deliciouses
    :accessor deliciouses
    :initarg :deliciouses
    :initform 0
    :documentation "The total number of delicious ingredients.")
   (disgustings
    :accessor disgustings
    :initarg :disgustings
    :initform 0
    :documentation "The total number of disgusting ingredients.")))

(defmethod print-object ((object totals) stream)
  (print-unreadable-object (object stream :type t)
;; Wouldn't it be handy to instead simply list all the slots of
;; object? How could I do that? Surely CL has some function or
;; macro to do that.
    (let (slt-val-lst)
      (dolist (itm '(deliciouses disgustings) (progn
                                                (terpri)
                                                (format t "~{~a: ~a~%~}" (nreverse slt-val-lst))))
        (push itm slt-val-lst)
        (push (slot-value object itm) slt-val-lst)))))

(defvar *totals* (make-instance 'totals))


(let ((totals-doc (make-document)))
  (add-element "NAME" "totals" totals-doc)
  (add-element "DELICIOUSES" 0 totals-doc)
  (add-element "DISGUSTINGS" 0 totals-doc)
  (db.insert *ice-cream-collection* totals-doc))

(defun clear-database ()
  (let (found-docs)
    (dolist (key '("NAME" "INGREDIENT") (db.delete *ice-cream-collection* found-docs))
                     (push (docs (db.find *ice-cream-collection* (kv key))) found-docs))
    (setf *totals* (make-instance 'totals))))
       




;; Class ingredient-feature
;; The integers in the cdrs of the assoc-lists delicious-list and disgusting-list indicate
;; frequency of appearance of the ingredient-feature in a delicious or disgusting
;; ice cream, and the car of the assoc-list item indicates with what other
;; ingredient feature this ingredient feature was paired to make that ice cream.
;; So we get not only an estimate of
;; what ingredients are good in ice cream by comparing that
;; ingredient's delicious and disgusting frequencies, but also estimates of
;; delicious and disgusting combinations by making that same comparison 
;; from the ingredient-feature to one of the other ingredient-features named
;; in one of its lists. Furthermore, estimates for unencountered combinations
;; can be guessed by back-chaining via the delicious & disgusting lists.
(defclass ingredient-feature ()
  ((ingredient 
    :initarg :ingredient 
    :accessor ingredient 
    :initform (error "Must supply :ingredient")
    :documentation "The ingredient this feature represents.")
   (delicious-freq
    :initarg :delicious-freq
    :accessor delicious-freq
    :initform 0
    :documentation "Frequency of delicious instances of this ingredient-feature.")
   (disgusting-freq
    :initarg :disgusting-freq
    :accessor disgusting-freq
    :initform 0
    :documentation "Frequency of disgusting instances of this ingredient-feature.")
   (score
    :initarg :score
    :accessor score
    :initform nil
    :documentation "The ingredient's flavor score.")))

(defvar *fields* (list 'ingredient 'delicious-freq 'disgusting-freq 'score))



(defmethod print-object ((object ingredient-feature) stream)
  (print-unreadable-object (object stream :type t)
;; Wouldn't it be handy to instead simply list all the slots of
;; object? How could I do that? Surely CL has some function or
;; macro to do that.
    (let (slt-val-lst)
      (dolist (itm *fields* (progn
                              (terpri)
                              (format t "~{~a: ~a~%~}" (nreverse slt-val-lst))))
        (push itm slt-val-lst)
        (push (slot-value object itm) slt-val-lst)))))

;; Where ing is the ingredient-feature to update,
;; feat is the new ingredient with which to update,
;; and the-lst is which flavor category list to update, delicious or digusting
;; if ing & feat are the same, then incf the ingredient-feature's self count,
;; else if this ingredient knows about the other ingredient then update that count,
;; else make a new count (starting with 1) in this ingredient-feature for the other one.
(defmethod increment-count ((ing ingredient-feature) class)
;; Update the frequencies
  (ecase class
    (delicious (incf (delicious-freq ing)))
    (disgusting (incf (disgusting-freq ing)))))

(defmethod increment-count :after ((ing ingredient-feature) class)
  (declare (ignore class))
  (let ((ing-kv-cont (ing->kv-cont ing)))
    (db.update *ice-cream-collection* ($ "INGREDIENT" (string-upcase (mkstr (ingredient ing)))) ing-kv-cont)))

(defmethod update-score ((ing ingredient-feature))
  (setf (score ing) (compute-score (list (ingredient ing)))))

(defmethod update-score :after ((ing ingredient-feature))
  (let ((ing-kv-cont (ing->kv-cont ing)))
    (db.update *ice-cream-collection* ($ "INGREDIENT" (string-upcase (mkstr (ingredient ing)))) ing-kv-cont)))

(defmethod increment-total-count ((tots totals) class)
  (ecase class
    (delicious (incf (deliciouses tots)))
    (disgusting (incf (disgustings tots)))))

(defmethod increment-total-count :after ((tots totals) class)
  (declare (ignore class))
  (let ((tots-kv-cont ($ ($ "NAME" "totals") 
                         ($ "DELICIOUSES" (deliciouses tots)) 
                         ($ "DISGUSTINGS" (disgustings tots)))))
    (db.update *ice-cream-collection* ($ "NAME" "totals") tots-kv-cont)))



(defun ing->kv-cont (ing-feat)
  ($ ($ "INGREDIENT" (ingredient ing-feat))
     ($ "DELICIOUS-FREQ" (delicious-freq ing-feat))
     ($ "DISGUSTING-FREQ" (disgusting-freq ing-feat))
     ($ "SCORE" (score ing-feat))))

;; This must be a functional version, rather than using cl-mongo's $ macro,
;; because I want to iteratively evalute a set of arguments when constructing each field.
;; This (& doc->ing for that matter) could be abstracted as a generic function taking an
;; object and a list of fields. Then it could be inherited by both ingredient-features & 
;; *totals*.
(defun ing->doc (ing-feat)
  (let ((doc (make-document)))
    (dolist (field *fields* doc)
      (add-element (mkstr field) (slot-value ing-feat field) doc))))



;; Retrieve a record, encapsulating it as an ingredient-feature object
;; This function positively weeps for a better abstraction, but at least this works.
(defun doc->ing (ing-doc)
  (make-instance 'ingredient-feature 
    :ingredient (get-element "INGREDIENT" ing-doc)
    :delicious-freq (get-element "DELICIOUS-FREQ" ing-doc)
    :disgusting-freq (get-element "DISGUSTING-FREQ" ing-doc)
    :score (get-element "SCORE" ing-doc)))


(defun add-ing (ing)
  "Add an ingredient-feature with the given symbol as the ingredient slot value.
   In this version we don't check for duplicates.
  Returns result of the mongo insert operation."
  (db.insert *ice-cream-collection* (ing->doc (make-instance 'ingredient-feature :ingredient ing))))


(defun sym->ing-feat (name)
  "Takes a symbol, returns an ingredient-feature object.
  Queries the database for an ingredient matching the given name.
   Note that db.find behaves like Mongo's findOne by default, so
   when we've found one we know there can be only one."
  (let ((found-ingredients (docs (db.find *ice-cream-collection* ($ "INGREDIENT" (string-upcase (mkstr name)))))))
    (when found-ingredients
      (doc->ing (first found-ingredients)))))

(defun ingredient-stored? (name)
  (sym->ing-feat name))


    
  




;; Train
;; Learn an ingredient instance, an accompanying flavor, & its flavor-category
(defun train-ing (ing class)
  (unless (sym->ing-feat ing) ; because it might find no such ingredient
     (add-ing ing))
  (increment-count (sym->ing-feat ing) class)
  (increment-total-count *totals* class)
  (update-score (sym->ing-feat ing)))





(defun delicious-probability (ingredient)
  (setf ingredient (sym->ing-feat ingredient))
  (with-slots (delicious-freq disgusting-freq) ingredient
    (let ((deliciouses (/ delicious-freq (max 1 (deliciouses *totals*))))
          (disgustings (/ disgusting-freq (max 1 (disgustings *totals*)))))
      (float (/ deliciouses (+ deliciouses disgustings))))))



(defun bayesian-delicious-probability (ing &optional
                                           (assumed-probability 1/2)
                                           (weight 1))
  (setf ing (sym->ing-feat ing))
  (let ((basic-probability (delicious-probability (ingredient ing)))
        (data-points (+ (delicious-freq ing) (disgusting-freq ing))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun untrained-p (feature)
  (let ((ing-feat (sym->ing-feat feature)))
    (with-slots (delicious-freq disgusting-freq) 
      ing-feat
      (and (zerop delicious-freq) (zerop disgusting-freq)))))

;; Version of Seibel's (2008, p. 301) using do* instead of loop
(defun inverse-chi-square (value df)
  (assert (evenp df))
  (min
   (do* ((m (/ value 2))
         (i 0 (incf i))
         (prob (exp (- m)) (* prob (/ m i)))
         (sum-probs 0))
       ((>= i (/ df 2)) sum-probs)
     (setf sum-probs (+ sum-probs prob)))
   1.0))

(defun fisher (probs n-probs)
  "The Fisher computation described by Robinson, in Seibel, 2008, p. 300"
  (inverse-chi-square
   (* -1 (reduce #'+ probs :key #'log))
   (* 2 n-probs)))

(defun compute-score (features)
  "Computes the probability that an ice cream with the ingredients is disgusting. Assigns score of .5 to individual features it has not trained on."
  (let ((del-probs ()) (dis-probs ()) (n-probs 0))
    (dolist (feature features)
      (if (not (sym->ing-feat feature))
          (progn
            (push .5 del-probs)
            (push .5 dis-probs)
            (incf n-probs))
        (let ((del-prob (float (bayesian-delicious-probability feature) 0.0d0)))
          (push del-prob del-probs)
          (push (- 1.0d0 del-prob) dis-probs)
          (incf n-probs))))
    (let ((del (- 1 (fisher dis-probs n-probs)))
          (dis (- 1 (fisher del-probs n-probs))))
       (/ (+ del (- 1 dis)) 2.0d0))))




(defun classify (ic)
  "Chains together feature extraction, score computation, and classification."
  (classification (compute-score (extract-features ic))))

(defun classification (score)
  "Takes a score and returns a classification atom."
  (cond
   ((>= score .6) 'delicious)
   ((<= score .4) 'disgusting)
   (t 'indeterminate)))

(defun extract-ings (text)
    "Takes a string, returns a list of unique words (strings) found in the string."
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))


(defun extract-features (text)
  "Takes a string. Calls extract-ings to parse into unique strings. Returns a list of those unique strings as symbols."
  (mapcar #'(lambda (ing)
              (intern (string-upcase ing))) 
          (extract-ings text)))

(defun train-ic (text type)
  "Takes a string and a classification for the entire string (either symbol 'delicious or 'disgusting).
Calls extract-features on the string and increments the appropriate count for each extracted
feature accordingly."
  (dolist (feature (extract-features text))
;; increment-count expects an ingredient-feature object and a classification symbol
    (train-ing feature type)))




;;; Web Server
(let ((the-server (make-instance 'easy-acceptor :port 8000 :document-root #P"/Lisp/ice-cream-computer/public-html/")))
  (defun start-server ()
    (start the-server))
  (defun stop-server ()
    (stop the-server)))



(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/icc-logo.png" "/Lisp/ice-cream-computer/static/icc-icon.png") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/icc.css" "/Lisp/ice-cream-computer/static/icc.css") *dispatch-table*))


;; DSL for our web pages
;; =====================

;; Here we grow a small domain-specific language for
;; creating dynamic web pages.

; Control the cl-who output format (default is XHTML, we 
; want HTML5):
(setf (html-mode) :html5)

(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro;
  less to type and a uniform look of the pages (defines the header
  and the stylesheet).
  The macro also accepts an optional script argument. When present, the
  script form is expected to expand into valid JavaScript."
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head 
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css" 
                    :rel "stylesheet"
                    :href "/icc.css")
             ,(when script
		    `(:script :type "text/javascript"
			      (str ,script))))
            (:body 
             (:div :id "header" ; Retro games header
                   (:a :href "/" (:img :src "/icc-logo.png" 
                                       :alt "Ice Cream" 
                                       :class "logo")))
             (:div :id "ad" :style "height:600px;width:160px;position:absolute;left:800px;top:25px;"
                   (:script :type "text/javascript" 
                            "google_ad_client = \"ca-pub-9395183324084917\";
                            /* Cogscent */
                            google_ad_slot = \"5805918080\";
                            google_ad_width = 160;
                            google_ad_height = 600;
                            ")
                   (:script :type "text/javascript" :src "http://pagead2.googlesyndication.com/pagead/show_ads.js"))
             ,@body))))

;; HTML
;; ====

;; The functions responsible for generating the actual pages of our app go here.
;; We use the Hunchentoot macro define-easy-handler to automatically
;; push our uri to the dispatch table of the server and associate the
;; request with a function that will handle it.
(define-easy-handler (icc :uri "/") ()
  (standard-page 
   (:title "Ice Cream Computer"
    :script (ps  ; client side validation
              (defvar computeform nil)
              (defun validateicecream (evt)
                "For a more robust event handling
                mechanism you may want to consider
                a library (e.g. jQuery) that encapsulates
                all browser-specific quirks."
                (when (= (@ computeform icecream value) "")
                  (chain evt (prevent-default))
                  (alert "Please enter an ingredient.")))
              (defun init ()
                (setf computeform (chain document
                                         (get-element-by-id "computeform")))
                (chain computeform
                       (add-event-listener "submit" validateicecream false)))
              (setf (chain window onload) init)))
    (:h1 "Compute some ice cream!")
    (:p "Enter a space-delimited list of ingredients to estimate the probability that a given ice cream composed of those ingredients will be delicious.")
    (:form :action "/compute-ice-cream" :method "post" :id "computeform"
           (:input :type "text" :name "icecream" :class "txt")
           (:input :type "submit" :value "Compute" :class "btn"))
    (:p (:a :href "rankings" "Rankings"))
    (:p (:a :href "colophon" "About") "&nbsp;ice cream computer.")))

;; How to get one or more ice creams so I can pass them to score?
;; Worry about that later, for now just do one at a time.
;; Although the Lispy way would be for define-easy-handler to be able to take more
;; than one argument when it's dispatched.
(define-easy-handler (compute-ice-cream :uri "/compute-ice-cream") (icecream)
  (let (msg)
    (progn
      (setf msg (format 
                     nil 
                     "~a ice cream is ~a." 
                     icecream 
                     (classify icecream))) ;(score (list icecream))))))
          (standard-page
              (:title "Ice Cream Classification"
                      :script (ps  ; client side validation
                              (defvar trainform nil)
                              (defun validateflavor (evt)
                                "For a more robust event handling
                                mechanism you may want to consider
                                a library (e.g. jQuery) that encapsulates
                                all browser-specific quirks."
                                (when (= (@ trainform flavor value) "...")
                                  (chain evt (prevent-default))
                                  (alert "Please select a flavor.")))
                              (defun init ()
                                (setf trainform (chain document
                                                       (get-element-by-id "trainform")))
                                (chain trainform
                                       (add-event-listener "submit" validateflavor false)))
                              (setf (chain window onload) init)))
            (:h1 "Ice Cream Classification")
            (:p (fmt msg))
            (:form :action "/train" :method "post" :id "trainform"
                 (:p "But what do you think?")
                 (:p
                  (:input :type "text" :name "icecream" :class "txt" :value icecream)
                  " ice cream is "
;; I could make this more efficient for the user by replacing these select and submit widgets with 
;; one link each for delicious or disgusting which also submits.
                  (:select :name "flavor"
                           (:option :value "..." "...")
                           (:option :value "delicious" "delicious")
                           (:option :value "disgusting" "disgusting"))
                  ".&nbsp;")
                 (:p
                  (:input :type "submit" :value "Learn" :class "btn")
                  "&nbsp;this flavor some more."))))))


(define-easy-handler (colophon :uri "/colophon") ()
  (standard-page 
   (:title "Colophon")
   (:h1 "Colophon")
   (:p "Silly it may be, but this site served as a useful exercise for me to learn how to
generate HTML and Javascript dynamically, implement a learning algorithm, and interface
with a database to achieve more and persistent storage than memory-alone would allow. This site runs with the cl-mongo, cl-who, hunchentoot, and parenscript packages for Common Lisp (Clozure Common Lisp) and Mongo DB, both on Raspian Linux, on two Raspberry Pi Model Bs. I used " (:a :href "http://leanpub.com/lispweb" "Lisp for the Web") " and " (:a :href "http://www.gigamonkeys.com/book/" "Practical Common Lisp") " as guides.")
   (:p "Ice Cream Computer's design theme was inspired by a phrase overheard on the
Washington, D.C. Metro's Blue Line. Thank you, mysterious Blue Line Rider!")))



(define-easy-handler (train :uri "/train") (icecream flavor)
;; train the classifier
  (train-ic icecream (intern (string-upcase flavor) :icc))
;; update scores
  (dolist (feat-sym (extract-features icecream))
    (update-score (sym->ing-feat feat-sym)))
;; display the page
  (let (msg0 msg1)
    (setf msg0 (format 
                nil 
                "Well in that case, I think ~a ice cream should have a ~,2F% chance of being delicious." 
                icecream 
                (* 100 (compute-score (extract-features icecream))))
          msg1 (format nil "Of course, this is only an estimate. Therefore, The Interest of Science mandates 
you provide me more ~a samples of this ~a ice cream to refine my estimate." flavor icecream))
    (standard-page
        (:title "Recently Trained Ice Cream Score")
      (:h1 "Recently Trained Ice Cream Score")
      (:p (fmt msg0))
      (:p (fmt msg1)))))







(define-easy-handler (rankings :uri "/rankings") ()
  (let* ((all-ing  (mapcar #'doc->ing 
                           (docs 
                            (iter
                             (db.sort *ice-cream-collection* :all :field "SCORE" :asc nil))))))
    
    (standard-page
        (:title "Ice Cream Flavor Rankings")
      (:h1 "Ingredient Rankings")
      (:p "The per-ingredient values are:")
      (:ul 
       (:li "Number of times the ingredient appeared in a delicious ice cream,")
       (:li "Number of times the ingredient appeared in a disgusting ice cream,")
       (:li "Probability that any ice cream containing this ingredient will be delicious."))
      (:p "Note that some ingredients will more strongly indicate delicious or disgusting classification than others.")
      (:p "&nbsp;")
      (:table
       (:tr (:td :width 250 (:b "Ingredient")) 
            (:td :width 60 (:b "Delicious Count"))
            (:td :width 60 (:b "Disgusting Count"))
            (:td :width 60 (:b "p(delicious)"))
#|
            (:td :width 25)
            (:td :width 25)
|#
	    )
       (dolist (ingrdnt all-ing)
         (htm (:tr 
               (:td (fmt "~a" (string-downcase (ingredient ingrdnt))))
               (:td (fmt "~a" (delicious-freq ingrdnt)))
               (:td (fmt "~a" (disgusting-freq ingrdnt)))
               (:td (fmt "~,3F" (score ingrdnt)))
;; 2017.04.06 What was I trying to do here, enable voting up or down of ingredients from the rankings page? That's not a bad idea. Too bad I apparently stopped midway implementation.
#|
	       (:td (ps 
		      (chain xmlhttp (open "post" "train"))
		      (chain xmlhttp (send "icecream=" (ingredient ingrdnt) "flavor=delicious"))))
	       (:td (fmt "&nbsp;"))
|#
	       )))))))


;; Alright, everything has been defined - launch Hunchentoot and have it
;; listen to incoming requests:
(publish-static-content)




;; Close the connection to Mongo when done
; (mongo-close "ic-store")



