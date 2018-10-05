(in-package :Tootsville)

;;; Normalize-URL

(defun normalize-url (url)
  "Normalize URL into a canonical form, using some typical UNIX pathname rules."
  (check-type url www-uri)
  ;; TODO s(/foo/../) (/)
  ;; TODO s( http://foo:80/ ) (http://foo/ )
  ;; TODO s( http://foo:443/ ) (http://foo/ )
  ;; TODO s ( %xx ) (byte xx ) and back again
  (error 'unimplemented))

(assert (equal (normalize-url "HTTPS://example.com/foo") "https://example.com/foo")
        () "Protocol should be downcased")
(assert (equal (normalize-url "https://EXAMPLE.COM/foo") "https://example.com/foo")
        () "Hostname should be downcased")
(assert (equal (normalize-url "https://example.com:443/foo") "https://example.com/foo")
        () "Default TLS port number should be omitted")
(assert (equal (normalize-url "https://example.com:4343/foo") "https://example.com:4343/foo")
        () "Non-default TLS port number should be kept")
(assert (equal (normalize-url "http://example.com:80/foo") "http://example.com/foo")
        () "Default HTTP port number should be omitted")
(assert (equal (normalize-url "http://example.com:8080/foo") "http://example.com:8080/foo")
        () "Default HTTP port number should be kept")
(assert (equal (normalize-url "http://example.com/foo/../bar") "http://example.com/bar")
        () "../ path elements should be treated as (:UP)")
(assert (equal (normalize-url "http://example.com/foo//bar") "http://example.com/foo/bar")
        () "// path elements should collapse to /")
(assert (equal (normalize-url "http://example.com/foo/./bar") "http://example.com/foo/bar")
        () "/./ path elements should collapse to /")
(assert (equal (normalize-url "http://example.com/foo/.") "http://example.com/foo/")
        () "trailing /. path elements should collapse to /")
(assert (equal (normalize-url "http://example.com/foo/bar/../../baz") "http://example.com/baz")
        () "../../ chained path elements should each be processed")
(assert (equal (normalize-url "http://example.com/foo/%47") "http://example.com/foo/%47")
        () "reserved characters should remain %-encoded")
(assert (equal (normalize-url "http://example.com/foo/+x") "http://example.com/foo/%20x")
        () "spaces should be converted from + to %20")
(assert (equal (normalize-url "http://example.com/foo/%97") "http://example.com/foo/a")
        () "non-reserved ASCII characters should not be %-encoded")



;;; Amazon Alexa functions. Mandated by Amazon.

(define-constant +amazon-cert-chain-url-matching+
    '(("https" . #'string-equal)
      ("" . #'string=)
      ("s3.amazonaws.com" . #'string-equal)
      ("echo.api" . #'string=))
  :test #'equalp
  :documentation  "list of  pairs  of strings  and comparison  functions
  which must be met for the URL of an Alexa certificate chain. See
 `CHECK-ALEXA-SIGNATURE-CERT-CHAIN-URL'")

(defun check-alexa-signature-cert-chain-url (url)
  "Perform the mandatory checks on an Alexa request's certificate chain URL.

Excerpt from Amazon requirements at @url{https://developer.amazon.com/docs/custom-skills/host-a-custom-skill-as-a-web-service.html}:

Verifying the Signature Certificate URL

Before downloading the certificate from the URL specified in the SignatureCertChainUrl header, you should ensure that the URL represents a URL Amazon would use for the certificate. This protects against requests that attempt to make your web service download malicious files and similar attacks.

First, normalize the URL so that you can validate against a correctly formatted URL. For example, normalize

https://s3.amazonaws.com/echo.api/../echo.api/echo-api-cert.pem

to:

https://s3.amazonaws.com/echo.api/echo-api-cert.pem

Next, determine whether the URL meets each of the following criteria:

@enumerate
@item
    The protocol is equal to https (case insensitive).
@item
    The hostname is equal to s3.amazonaws.com (case insensitive).
@item
    The path starts with /echo.api/ (case sensitive).
@item
    If a port is defined in the URL, the port is equal to 443.
@end enumerate

Examples of correctly formatted URLs:

@itemize
@item
    https://s3.amazonaws.com/echo.api/echo-api-cert.pem
@item
    https://s3.amazonaws.com:443/echo.api/echo-api-cert.pem
@item
    https://s3.amazonaws.com/echo.api/../echo.api/echo-api-cert.pem
@end itemize

Examples of invalid URLs:

@itemize
@item
    http://s3.amazonaws.com/echo.api/echo-api-cert.pem (invalid protocol)
@item
    https://notamazon.com/echo.api/echo-api-cert.pem (invalid hostname)
@item
    https://s3.amazonaws.com/EcHo.aPi/echo-api-cert.pem (invalid path)
@item
    https://s3.amazonaws.com/invalid.path/echo-api-cert.pem (invalid path)
@item
    https://s3.amazonaws.com:563/echo.api/echo-api-cert.pem (invalid port)
@end itemize

If the URL does not pass these tests, reject the request and do not proceed with verifying the signature.

"
  (check-type url www-uri)
  (let* ((normalized (normalize-url url))
         (parts (split-sequence #\/ normalized)))
    (assert (> (length parts) (length +amazon-cert-chain-url-matching+)) ()
            "The URL ~a does not have enough path components to be a valid Alexa certificate chain URL"
            url)
    (loop for (string . fun) in +amazon-cert-chain-url-matching+
       for part in parts
       unless (funcall fun string part)
       do (error "The URL ~a is not a valid Alexa certificate chain URL: ~a is not ~a ~a"
                 url part (ecase fun (#'string-equal "≈") (#'string= "=")) string)))
  t)

(assert (ignore-errors (check-alexa-signature-cert-chain-url 
                        "https://s3.amazonaws.com/echo.api/echo-api-cert.pem"))
        () "Unit test from Amazon requirements")
(assert (ignore-errors (check-alexa-signature-cert-chain-url 
                        "https://s3.amazonaws.com:443/echo.api/echo-api-cert.pem"))
        () "Unit test from Amazon requirements")
(assert (ignore-errors (check-alexa-signature-cert-chain-url 
                        "https://s3.amazonaws.com/echo.api/../echo.api/echo-api-cert.pem"))
        () "Unit test from Amazon requirements")

(assert (null (ignore-errors (check-alexa-signature-cert-chain-url
                              "http://s3.amazonaws.com/echo.api/echo-api-cert.pem")))
        () "Unit test from Amazon requirements")
(assert (null (ignore-errors (check-alexa-signature-cert-chain-url
                              "https://notamazon.com/echo.api/echo-api-cert.pem")))
        () "Unit test from Amazon requirements")
(assert (null (ignore-errors (check-alexa-signature-cert-chain-url
                              "https://s3.amazonaws.com/EcHo.aPi/echo-api-cert.pem")))
        () "Unit test from Amazon requirements")
(assert (null (ignore-errors (check-alexa-signature-cert-chain-url
                              "https://s3.amazonaws.com/invalid.path/echo-api-cert.pem")))
        () "Unit test from Amazon requirements")
(assert (null (ignore-errors (check-alexa-signature-cert-chain-url
                              "https://s3.amazonaws.com:563/echo.api/echo-api-cert.pem")))
        () "Unit test from Amazon requirements")

(defun check-alexa-signature ()
  "Check the signature of an Alexa request.

Excerpt from Amazon requirements at @url{https://developer.amazon.com/docs/custom-skills/host-a-custom-skill-as-a-web-service.html}:

Checking the Signature of the Request

Requests sent by Alexa provide the information you need to verify the signature in the HTTP headers:
@itemize
@item
    SignatureCertChainUrl
@item
    Signature
@end itemize

To validate the signature:

@enumerate
@item
    Verify the URL specified by the SignatureCertChainUrl header value on the request to ensure that it matches the format used by Amazon. See Verifying the Signature Certificate URL.
@item
    Download the PEM-encoded X.509 certificate chain that Alexa used to sign the message as specified by the SignatureCertChainUrl header value on the request.
@item
    This chain is provided at runtime so that the certificate may be updated periodically, so your web service should be resilient to different URLs with different content.
@item
    This certificate chain is composed of, in order, (1) the Amazon signing certificate and (2) one or more additional certificates that create a chain of trust to a root certificate authority (CA) certificate. To confirm the validity of the signing certificate, perform the following checks:
@itemize
@item
        The signing certificate has not expired (examine both the Not Before and Not After dates)
@item
        The domain echo-api.amazon.com is present in the Subject Alternative Names (SANs) section of the signing certificate
@item
        All certificates in the chain combine to create a chain of trust to a trusted root CA certificate
@end itemize
@item
    Once you have determined that the signing certificate is valid, extract the public key from it. 
@item
Base64-decode the Signature header value on the request to obtain the encrypted signature.
    @item
Use the public key extracted from the signing certificate to decrypt the encrypted signature to produce the asserted hash value.
    @item
Generate a SHA-1 hash value from the full HTTPS request body to produce the derived hash value
    @item
Compare the asserted hash value and derived hash values to ensure that they match.
@end enumerate
"
  (let ((cert-chain-url (hunchentoot:header-in* :Signature-Cert-Chain-Url))
        (signature (base64-decode (hunchentoot:header-in* :Signature))))
    (check-alexa-signature-cert-chain-url signature-cert-chain-url)
    (let ((x.509-cert (drakma:http-request signature-cert-chain-url :output :string)))
      (check-cert-dates-valid x.509-cert)
      (check-san x.509-cert "echo-api.amazon.com")
      (check-cert-chain-valid x.509-cert)
      (let ((public-key (extract-public-key-from-cert x.509-cert))
            (asserted-hash (decode-message signature public-key))
            (sha1-hash (sha1-hash (hunchentoot:raw-post-data :force-binary t))))
        (assert (= asserted-hash sha1-hash) ()
                "The signature must validate against this message")))))

(defconstant +alexa-timestamp-tolerance+ 150
  "Amazon requires we requect queries with  a timestamp more than ± this
  many seconds.")

(defun check-alexa-timestamp-tolerance (body-json)
  "Ensure that the timestamp of an Alexa-sent query is within the allowed tolerance.

Excerpt from Amazon requirements at @url{https://developer.amazon.com/docs/custom-skills/host-a-custom-skill-as-a-web-service.html}:

Checking the Timestamp of the Request

Every request  sent to your web  service by Alexa includes  a timestamp.
This information  is part of  the signed portion  of the request,  so it
cannot  be  changed without  also  invalidating  the request  signature.
Using  this timestamp  to verify  the  freshness of  the request  before
responding protects your service  from attackers attempting a \"replay\"
attack  in  which  they  acquire  a properly  signed  request  and  then
repeatedly resend it to disrupt your service.

Your service should  allow a tolerance of no more  than 150 seconds (two
and a  half minutes). This  means that  your service should  only accept
requests in  which the request  timestamp is  within 150 seconds  of the
current  time. Web  services that  allow  a longer  tolerance cannot  be
published to Amazon customers.

…

If you are not using the Java  library, you need to do this verification
yourself. The timestamp is provided as part of the request object in the
JSON body  of the  request … The  timestamp is provided  as an  ISO 8601
formatted string (for example, 2015-05-13T12:34:56Z). Your code needs to
parse the string into  a date object, then verify that  it is within the
tolerance  your  web   service  allows  (no  more   than  150  seconds).
Reject requests in which the  timestamp falls outside the tolerance with
an error code (such as 400 Bad Request).

"
  (assert (ignore-errors
            (zerop (search "application/json" (hunchentoot:header-in* :content-type)))) ()
            "The contet-type of an Alexa request must be application/json")
  (assert (<= (abs (timestamp-difference
                    (now) 
                    (parse-timestring (string-trim
                                       (extract body-json "request" "timestamp")
                                       +whitespace+))))
              +alexa-timestamp-tolerance+)
          ()
          "Messages from Alexa must have a timestamp within ± ~as (about ~a) of the current time."
          +alexa-timestamp-tolerance+))

(defun check-alexa (body-json)
  "Performs the mandatory checks for queries from Alexa.

Documented                 by                 Amazon                 at:
@url{https://developer.amazon.com/docs/custom-skills/host-a-custom-skill-as-a-web-service.html}

Service

To  handle requests  sent  by  Alexa, your  web  service  must meet  the
following requirements:

@enumerate

@item 
The service must be Internet-accessible.

@item 
The service must adhere to the Alexa Skills Kit interface.

@item
The   service   must   support   HTTP  over   SSL/TLS,   leveraging   an
Amazon-trusted certificate.

@itemize
@item

For   testing,   Amazon   accepts  different   methods   for   providing
a certificate. For details, see About the SSL Options.

@item

For publishing to  end users, Amazon only trusts  certificates that have
been signed by an Amazon-approved certificate authority.

@end itemize

@item
The service must accept requests on port 443.

@item
The service  must present  a certificate with  a subject  alternate name
that matches the domain name of the endpoint.

@item 
The service must validate that incoming requests are coming from Alexa.
@end itemize

Note: if you are using Apache HTTP  Server to host your web service, use
version 2.4.10 or later. Earlier versions  of Apache HTTP Server send an
\"unrecognized  name\" warning  if  the server  is  not configured  with
a ServerName  or ServerAlias in  the configuration files.  This prevents
the Alexa  service from sending  the customer's request to  your server.
To address this, either upgrade to  2.4.10 or later, or add ServerName /
ServerAlias to your server's configuration file.
"
  (handler-case 
      (progn
        (check-alexa-signature)
        (check-alexa-timestamp-tolerance body-json))
    (error (c)
      (report-error c)
      (throw 'endpoint-over (list 400 () (stringify c))))))

(defmacro define-alexa-endpoint (name (arg) &body body)
  `(defendpoint (post ,(format nil "/gossip/alexa/~(~a~)/region=:region" name))
     (let ((,arg (st-json:read-json-from-string
                  (hunchentoot:raw-post-data :force-text t))))
       (check-alexa ,arg)
       (,@body))))
