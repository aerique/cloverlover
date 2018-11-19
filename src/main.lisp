;;;; main.lisp

;;; Packages

(ql:quickload :drakma)
(ql:quickload :jsown)


;;; Globals

(defparameter *api-url* "https://api.pushover.net/1/")

(defparameter *app-name*    "Cloverlover")
(defparameter *app-version* "0.0.1")

(defparameter *user-agent* (concatenate 'string *app-name* "/" *app-version*
                                      " " (drakma::user-agent-string :drakma)))


;;; Common Functions

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


;;; Functions

(defun login (email password)
  (let* ((response (drakma:http-request (mkstr *api-url* "users/login.json")
                                      :method :POST :user-agent *user-agent*
                                      :parameters `(("email"    . ,email)
                                                    ("password" . ,password))))
         (json (jsown:parse (octets-to-string response)))
         (id (when (jsown:keyp json "id")
               (jsown:val json "id")))
         (secret (when (jsown:keyp json "secret")
                   (jsown:val json "secret"))))
    (when (and id secret)
      (list :id id :secret secret))))


(defun register-new-device (secret name &optional (os "O"))
  (let* ((response (drakma:http-request (mkstr *api-url* "/devices.json")
                                        :method :POST :user-agent *user-agent*
                                        :parameters `(("secret" . ,secret)
                                                      ("name"   . ,name)
                                                      ("os"     . ,os))))
         (json (jsown:parse (octets-to-string response)))
         (device-id (when (jsown:keyp json "id")
                      (jsown:val json "id")))
         (status (when (jsown:keyp json "status")
                   (jsown:val json "status")))
         (request (when (jsown:keyp json "request")
                    (jsown:val json "request"))))
    (when (and device-id status request)
      (list :device-id device-id :status status :request request))))


(defun delete-messages (secret device-id highest-message-id)
  (let* ((response (drakma:http-request (mkstr *api-url* "/devices/" device-id
                                               "/update_highest_message.json")
                     :method :POST :user-agent *user-agent*
                     :parameters `(("secret"  . ,secret)
                                   ("message" . ,(mkstr highest-message-id)))))
         (json (jsown:parse (octets-to-string response))))
    json))


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
         (json (jsown:parse (octets-to-string response))))
    (values (parse-messages json)
            ;; FIXME return the non-messages objects here
            'return-other-json-objects-here)))


;;; Main Program

(defun main ()
  )
