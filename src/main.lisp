;;;; main.lisp

(in-package :cl-user)


;;; Package

(defpackage :cloverlover
  (:use :cl)
  (:export :catch-up-on-all-messages :delete-messages :download-messages :login
           :register-new-device))

(in-package :cloverlover)


;;; Globals

(defparameter *verbose* t)

(defparameter *api-url* "https://api.pushover.net/1/")

(defparameter *app-name*    "Cloverlover")
(defparameter *app-version* "0.0.2")

(defparameter *user-agent* (concatenate 'string *app-name* "/" *app-version*
                                      " " (drakma::user-agent-string :drakma)))


;;; Common Functions

(defun errmsg (&rest args)
  (let ((*print-pretty* nil))
    (apply #'format (append (list *error-output*) args)))
  (force-output *error-output*))


(defun dbgmsg (&rest args)
  (when *verbose*
    (let ((*print-pretty* nil))
      (apply #'format (append (list *standard-output*) args)))
    (force-output *standard-output*)))


(defun make-keyword (string)
  (intern (string-upcase string) :keyword))


;; FIXME use this in all the right places
(defun json2plist (json)
  (loop for key in (jsown:keywords json)
        append (list (make-keyword key)
                     (jsown:val json key))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


;;; Pushover API Functions

(defun login (email password &key (parse-json t))
  (let* ((response (handler-case (drakma:http-request
                                  (mkstr *api-url* "users/login.json")
                                  :method :POST :user-agent *user-agent*
                                  :parameters `(("email"    . ,email)
                                                ("password" . ,password)))
                     (error (e) (mkstr e))))
         (response-string (handler-case (flexi-streams:octets-to-string
                                         response)
                            (error () (mkstr "{\"errors\": \"Error converting "
                                             "octets to string\", \"status\": "
                                             "0, \"cloverlover-error\": true, "
                                             "\"response\": \"" response
                                             "\"}")))))
    (if parse-json
        (json2plist (handler-case (jsown:parse response-string)
                      (error () `(:obj ("errors" . "Error parsing JSON")
                                     ("status" . 0)
                                     ("cloverlover-error" . t)
                                     ("response-string" . ,response-string)))))
        response-string)))


(defun register-new-device (secret name &optional (os "O"))
  (let* ((response (drakma:http-request (mkstr *api-url* "/devices.json")
                                        :method :POST :user-agent *user-agent*
                                        :parameters `(("secret" . ,secret)
                                                      ("name"   . ,name)
                                                      ("os"     . ,os))))
         (json (jsown:parse (flexi-streams:octets-to-string response)))
         (device-id (when (jsown:keyp json "id")
                      (jsown:val json "id")))
         (status (when (jsown:keyp json "status")
                   (jsown:val json "status")))
         (request (when (jsown:keyp json "request")
                    (jsown:val json "request"))))
    (if (and device-id status request)
        (list :device-id device-id :status status :request request)
        (progn (errmsg "~S" json)
               nil))))


(defun delete-messages (secret device-id highest-message-id)
  (let* ((response (drakma:http-request (mkstr *api-url* "/devices/" device-id
                                               "/update_highest_message.json")
                     :method :POST :user-agent *user-agent*
                     :parameters `(("secret"  . ,secret)
                                   ("message" . ,(mkstr highest-message-id)))))
         (json (jsown:parse (flexi-streams:octets-to-string response))))
    (if (= (jsown:val json "status") 1)
        t
        (progn (errmsg "~S" json)
               nil))))


(defun parse-messages (json)
  (when (jsown:keyp json "messages")
    (loop for msg in (jsown:val json "messages")
          for id       = (jsown:val msg "id")
          for title    = (if (jsown:keyp msg "title")
                             (jsown:val msg "title")
                             *app-name*)
          for message  = (jsown:val msg "message")
          for date     = (jsown:val msg "date")
          for priority = (jsown:val msg "priority")
          for acked    = (jsown:val msg "acked")
          for aid      = (jsown:val msg "aid")
          for app      = (jsown:val msg "app")
          for icon     = (jsown:val msg "icon")
          for umid     = (jsown:val msg "umid")
          collect (list :id id :title title :message message :date date
                        :priority priority :acked acked :aid aid :app app
                        :icon icon :umid umid))))


(defun download-messages (secret device-id)
  (let* ((response (drakma:http-request (mkstr *api-url* "/messages.json")
                                    :method :GET :user-agent *user-agent*
                                    :parameters `(("secret"    . ,secret)
                                                  ("device_id" . ,device-id))))
         (json (jsown:parse (flexi-streams:octets-to-string response))))
    (if (= (jsown:val json "status") 1)
        (values (parse-messages json)
                (append '(:obj) (cddr json)))
        (progn (errmsg "~S" json)
               nil))))


;;; Functions

(defun highest-message (messages)
  "Returns the highest message ID in MESSAGES.
  MESSAGES is assumed to be in the format as returned by DOWNLOAD-MESSAGES."
  (loop with highest-id = 0
        for msg in messages
        for id = (getf msg :id)
        for message = (getf msg :message)
        do (when (> id highest-id)
             (setf highest-id id))
        finally (return highest-id)))


(defun catch-up-on-all-messages (secret device-id)
  (loop for msgs = (download-messages secret device-id)
        while msgs
        for highest-id = (highest-message msgs)
        do (format t "Retrieved ~D messages.~%" (length msgs))
           (format t "  - highest id: ~D~%" highest-id)
           (format t "  - deleting messages...~%")
           (finish-output)
           (delete-messages secret device-id highest-id)
           (sleep 1)))
