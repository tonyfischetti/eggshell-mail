
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                       ;;
;;   email-report.lisp                                   ;;
;;                                                       ;;
;;                Author: Tony Fischetti                 ;;
;;                        tony.fischetti@gmail.com       ;;
;;                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ql:quickload :chirp)
(ql:quickload :html-template)
(ql:quickload :cl-smtp)
(ql:quickload :cl+ssl)


(defpackage :email-report
  (:use :common-lisp
        :html-template)
  (:export :main))

(in-package :email-report)



(defparameter *eggshell-prefix* "~/eggshell/")
(defparameter *user-name* "tonyfischetti")
(defparameter *auth-file* (concatenate 'string *eggshell-prefix* 
                                       "auth-file.lisp"))
(defparameter *history-file* (concatenate 'string *eggshell-prefix*
                                          "tweet-history.lisp"))
(defvar *history* '())



; defines the tokens and secrets
; for authentication
(load *auth-file*)


(defun slurp (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


(defun load-history ()
  (with-open-file (in *history-file*)
    (with-standard-io-syntax
      (setf *history* (read in)))))


(defun save-history ()
  (with-open-file (out *history-file*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *history* out))))


(defun add-to-history (an-entry)
  (setf *history* (cons an-entry *history*)))


(defun make-entry (friends followers)
  (let* ((time-result (multiple-value-list (get-decoded-time)))
         (year        (nth 5 time-result))
         (month       (nth 4 time-result))
         (date        (nth 3 time-result)))
    `(:year ,year :month ,month :date ,date :friends ,friends :followers ,followers)))


(defun get-current-friend-list ()
  (chirp:friends/ids :screen-name *user-name*))


(defun get-current-followers-list ()
  (chirp:followers/ids :screen-name *user-name*))


(defun get-new-followers (prior current)
  (set-difference (getf current :followers) (getf prior :followers)
                  :test #'equal))


(defun get-unfollowers (prior current)
  (set-difference (getf prior :followers) (getf current :followers)
                  :test #'equal))


(defun get-users-info-hash (user-ids)
  (let* ((sids       (mapcar #'write-to-string user-ids))
         (results    (chirp:users/lookup :user-id sids))
         (hash       (make-hash-table :test #'equal)))
    (mapcar (lambda (f s) (setf (gethash f hash) s))
            user-ids results)
    hash))


(defun make-html (stream an-plist)
  (fill-and-print-template #p"./email-template.tmpl" an-plist :stream stream))


(defun make-user-info-plist (user-object)
  (let ((avatar (cdr (assoc :image-url (chirp:avatar user-object))))
        (name   (chirp:name user-object))
        (handle (chirp:screen-name user-object)))
    `(:user-name ,name
      :user-avi ,avatar
      :user-handle ,(format nil "@~A" handle)
      :user-url ,(format nil "http://twitter.com/~A" handle))))


(defun make-list-of-users (ids user-hash)
  (mapcar #'make-user-info-plist (mapcar (lambda (x) (gethash x user-hash)) ids)))


(defun make-whole-html (new-followers unfollowers user-hash)
  (let* ((new-f-plist (if new-followers (make-list-of-users new-followers user-hash) '()))
         (un-f-plist (if unfollowers (make-list-of-users unfollowers user-hash) '()))
         (whole-thing `(:followers ,new-f-plist :unfollowers ,un-f-plist)))
    (with-open-file (stream "./final-email.html" :direction :output :if-exists :supersede)
      (make-html stream whole-thing))))


; -------------------------------- ;


(load-history)

(defvar *prior-entry* (car *history*))

(defvar *current-entry* (make-entry (get-current-friend-list) (get-current-followers-list)))

(let* ((new-followers (get-new-followers *prior-entry* *current-entry*))
       (unfollowers (get-unfollowers *prior-entry* *current-entry*))
       (both (append new-followers unfollowers))
       (num-messages (length both))
       (users-hash (if both (get-users-info-hash both) (make-hash-table))))
  (progn
    (make-whole-html new-followers unfollowers users-hash)

    (cl-smtp:send-email
      "smtp.gmail.com" "eggshellmailer@gmail.com"
      "tony.fischetti@gmail.com"
      (format nil "Eggshell Mail Twitter Digest (~A new message~:P)" num-messages)
      (slurp "./final-email.html")
      :authentication `(,*email-username* ,*email-password*)
      :ssl t
      :extra-headers '(("Content-type" "text/html; charset=\"UTF-8\"")))))

(add-to-history *current-entry*)

(save-history)


