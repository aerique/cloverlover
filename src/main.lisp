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


(defun json2plist (json)
  (loop for key in (jsown:keywords json)
        for val = (jsown:val json key)
        append (list (make-keyword key)
                     (cond ((and (consp val)
                                 (equal :obj (car val)))
                            (json2plist val))
                           ((listp val)
                            ;; XXX duplication of above sexp
                            (loop for item in val
                                  collect (if (and (consp item)
                                                   (equal :obj (car item)))
                                              (json2plist item)
                                              item)))
                           (t
                            (jsown:val json key))))))


(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))


;;; Pushover API Helper Functions

(defun api-call (resource &key (method :GET) (parameters nil))
  (let ((url (mkstr *api-url* resource)))
    (dbgmsg "[api-call] ~S request to ~S ~S~%" method url parameters)
    (handler-case (drakma:http-request url :method method
                                           :user-agent *user-agent*
                                           :parameters parameters)
      (error (e) (errmsg "[api-call] Error doing ~S request to ~S (~S): ~S~%"
                         method url parameters e)
                 ;; So this looks retarded, and maybe it is, but all other
                 ;; errors from either Pushover or Cloverlover are in JSON so
                 ;; this one might be as well.  (Especially since the main
                 ;; initial use of this lib is as a backend to a QT frontend,
                 ;; so we want to handle and show the errors in the frontend.)
                 (flexi-streams:string-to-octets
                   (let ((*print-pretty* nil))
                     (mkstr "{\"errors\": \"" e "\", \"status\": 0, "
                            "\"cloverlover-error\": true}")))))))


(defun json-response (drakma-response)
  "This function expects to be fed the return value of DRAKMA:HTTP-REQUEST."
  (handler-case (flexi-streams:octets-to-string drakma-response)
    (error (e) (errmsg (mkstr "[json-response] Error converting octets to "
                              "string: ~S (DRAKMA-RESPONSE: ~S)~%")
                       e drakma-response)
               (let ((*print-pretty* nil))
                 (mkstr "{\"errors\": \"Error converting octets to string: " e
                        "\", \"status\": 0, \"cloverlover-error\": true, "
                        "\"drakma-response\": \"" drakma-response "\"}")))))


(defun parsed-response (drakma-response)
  "This function expects to be fed the return value of DRAKMA:HTTP-REQUEST."
  (handler-case (json2plist (jsown:parse (json-response drakma-response)))
    (error (e) (errmsg "[parsed-response] Error parsing JSON: ~S~%" e)
               (let ((*print-pretty* nil))
                 `(:errors ,(mkstr "Error parsing JSON: " e) :status 0
                   :cloverlover-error t :drakma-response ,drakma-response)))))


;;; Pushover API Functions

(defun login (email password &key (parse-json t))
  (let ((response (api-call "users/login.json" :method :POST
                            :parameters `(("email"    . ,email)
                                          ("password" . ,password)))))
    (if parse-json
        (parsed-response response)
        (json-response response))))


(defun register-new-device (secret name &key (os "O") (parse-json t))
  (let ((response (api-call "devices.json" :method :POST
                            :parameters `(("secret" . ,secret)
                                          ("name"   . ,name)
                                          ("os"     . ,os)))))
    (if parse-json
        (parsed-response response)
        (json-response response))))


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


(defun download-messages (secret device-id &key (parse-json t))
  (let ((response (api-call "messages.json" :method :GET
                            :parameters `(("secret"    . ,secret)
                                          ("device_id" . ,device-id)))))
    (if parse-json
        (parsed-response response)
        (json-response response))))


(defun delete-messages (secret device-id highest-message-id
                        &key (parse-json t))
  (let ((response (api-call (mkstr "devices/" device-id
                                   "/update_highest_message.json")
                    :method :POST
                    :parameters `(("secret"  . ,secret)
                                  ("message" . ,(mkstr highest-message-id))))))
    (if parse-json
        (parsed-response response)
        (json-response response))))


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
