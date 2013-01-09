
;;; -*- Mode: LISP; Syntax: common-lisp; Package: cl-pcfinancial; Base: 10 -*-
  
;;; BEGIN_LICENSE:MIT
;; Copyright (c) 2031 Warren Wilkinson <warrenwilkinson@gmail.com>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; END_LICENSE 

(defpackage :cl-pcfinancial
  (:nicknames :pcf)
  (:use :common-lisp :drakma)
  (:export login logout download-transactions))

(in-package :cl-pcfinancial)

(defvar *cookie-jar* nil)
(defvar *force-ssh* t)

(defun open-url (url)
  (drakma:http-request url :force-ssl *force-ssh* :cookie-jar *cookie-jar*))

(defun submit-to (url &rest parameters)
  (format t "~%PARAMS: ~s" parameters)
  (drakma:http-request url :method :post :force-ssl *force-ssh* :cookie-jar *cookie-jar*
                       :parameters parameters))

(defun login (password cardNumber)
  (if *cookie-jar*
      (error "Already logged in...")
      (setf *cookie-jar* 
            (make-instance 'drakma:cookie-jar
                           :cookies (list
                                     (make-instance 'drakma:cookie 
                                                    :name "cardNumber"
                                                    :value cardNumber 
                                                    :domain "www.txn.banking.pcfinancial.ca")))))
  (submit-to "https://www.txn.banking.pcfinancial.ca/a/authentication/signOn.ams"
             (cons "cardNumberSaved" "0")
             (cons "password" password)))

(defun logout ()
  (multiple-value-prog1 (open-url "https://www.txn.banking.pcfinancial.ca/a/authentication/signOff.ams")
    (setf *cookie-jar* nil)))

(defun download-transactions (account &optional since)
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (or since (get-universal-time)))
    (declare (ignore sec min hour))
    (setf month (princ-to-string (decf month))
          year (princ-to-string year)
          day (princ-to-string day))
    (submit-to "https://www.txn.banking.pcfinancial.ca/a/banking/accounts/downloadTransactions2.ams"
               (cons "fromAccount" account)
               (cons "sinceLastDownload" (if since "false" "true"))
               (cons "previewDownload" "false")
               (cons "pfmSoftware" "other")
               (cons "fromDate__YEAR" year)
               (cons "fromDate__MONTH" month)
               (cons "fromDate__DAY" day))))
