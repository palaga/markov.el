(require 'cl-lib)
(require 'thingatpt)

(defvar markov--model (markov--new-model)
  "Variable containing the current markov model.")

;;; Customizations
(defcustom markov-model-type 'markov-tuple-type
  "Markov model implementation type."
  :group 'markov
  :type '(choice
          (const markov-word-type)
          (const markov-tuple-type)))

(cl-defstruct (markov-transitions
               (:constructor markov-transitions-create))
  "Datastructure containing a weighted state transition list and
a counter. The counter is the sum of all weights."
  (count 0)
  (transitions nil))

(defun markov--new-model ()
  "Create a new markov model."
  (make-hash-table :test 'equal))

(defun markov--reset-global-model ()
  "Reset the global markov model to clean state."
  (setq markov--model (markov--new-model)))

(defun markov--linear-weighted-choice (alist weight-sum)
  "Pick a random value from alist. The alist is expected to be a
list of (value . weight) pairs, where weight is an integer and
value any lisp form."
  (let* ((random-bound (1+ (random weight-sum))))
    (cl-loop for tuple in alist
             for sum = (cdr tuple) then (+ sum (cdr tuple))
             until (>= sum random-bound)
             finally return (car tuple))))

(defun markov-train-model ()
  "Train markov model on current buffer. Will reuse existing
state if any.

It will use markov-model-type to as its base implementation."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((final-state (get markov-model-type :markov-final-state))
          (initial-state (get markov-model-type :markov-initial-state))
          (read-state (get markov-model-type :markov-read-state))
          (move-to-next-state (get markov-model-type :markov-move-to-next-state))
          (equal-state (or (get markov-model-type :markov-equal-state) #'equal)))
      (cl-loop for previous-state = nil
               then current-state
               for current-state = (funcall read-state)
               then (progn (funcall move-to-next-state)
                           (funcall read-state))
               until (funcall equal-state current-state final-state)
               when previous-state do (markov--insert-transition markov--model
                                                                 previous-state
                                                                 current-state)))))

(defun markov-retrain-model ()
  "Reset model and train on current buffer."
  (interactive)
  (markov--reset-global-model)
  (markov-train-model))

(defun markov-generate ()
  "Generate output based in current markov model."
  (interactive)
  (let ((final-state (get markov-model-type :markov-final-state))
        (initial-state (get markov-model-type :markov-initial-state))
        (write-state (get markov-model-type :markov-write-state))
        (equal-state (get markov-model-type :markov-equal-state)))
    (cl-loop for current-state = initial-state
             then (markov--get-random-transition markov--model current-state)
             until (funcall equal-state current-state final-state)
             do (funcall write-state current-state))))

(defun markov--get-random-transition (model state)
  "Given a markov model and a state, pick a random transition and
return that state."
  (let ((transitions (gethash state model)))
    (unless transitions
      (error "No transitions for state '%s'." state))
    (markov--linear-weighted-choice
     (markov-transitions-transitions transitions)
     (markov-transitions-count transitions))))

(defun markov--insert-transition (model from-state to-state)
  "Insert a state transition into the markov model."
  (let* ((next-states (gethash from-state model
                               (markov-transitions-create)))
         (next-state-pos (seq-position (markov-transitions-transitions next-states)
                                       (list to-state)
                                       (lambda (a b)
                                         (equal (car a) (car b))))))
    ;; Increment total count
    (cl-incf (markov-transitions-count next-states))

    ;; Increment state specific counter
    (if next-state-pos
        (cl-incf (cdr (nth next-state-pos (markov-transitions-transitions next-states))))
      (push `(,to-state . 1) (markov-transitions-transitions next-states)))

    ;; If its the first transition, add it to the model
    (when (eq (markov-transitions-count next-states) 1)
      (puthash from-state
               next-states
               model))))

;;; markov model types
(cl-defmacro define-markov-model-type (name
                                       &key
                                       move-to-next-state equal-state
                                       read-state write-state
                                       initial-state final-state)
  `(setplist
    (quote ,(intern (concat "markov-" (symbol-name name) "-type")))
    (list
     :markov-move-to-next-state (lambda () ,move-to-next-state)
     :markov-equal-state ,(if equal-state
                              (list lambda () equal-state)
                            '(quote equal))
     :markov-read-state (lambda () ,read-state)
     :markov-write-state (lambda (state) ,write-state)
     :markov-initial-state ,initial-state
     :markov-final-state ,final-state)))

(define-markov-model-type word
  :move-to-next-state (if (looking-at (rx punct))
                          (forward-char)
                        (forward-word))
  :read-state (cond
               ((<= (point) (point-min)) 'start)
               ((>= (point) (point-max)) 'end)
               ((looking-back (rx punct) 1) (match-string-no-properties 0))
               (t (word-at-point 'no-properties)))
  :write-state (cond
                ((eq state 'start) nil)
                ((or (bobp)
                     (string-match-p (rx punct) state))
                 (insert (format "%s" state)))
                (t (insert (format " %s" state))))
  :initial-state 'start
  :final-state 'end)


(flet ((move-to-next-state
        () (if (looking-at (rx punct))
               (forward-char)
             (forward-word))))
  (define-markov-model-type tuple
    :move-to-next-state (move-to-next-state)
    :read-state (cond
                 ((<= (point) (point-min)) 'start)
                 ((>= (point) (point-max)) 'end)
                 (t (save-excursion
                      (cl-loop for i from 1 to 2
                               if (looking-back (rx punct) 1)
                               collect (match-string-no-properties 0)
                               else
                               collect (word-at-point 'no-properties)
                               end
                               do (move-to-next-state)))))
    :write-state (cond
                  ((eq state 'start) nil)
                  ((or (bobp)
                       (string-match-p (rx punct) (car state)))
                   (insert (format "%s" (car state))))
                  (t (insert (format " %s" (car state)))))
    :initial-state 'start
    :final-state 'end))

(provide 'markov)
