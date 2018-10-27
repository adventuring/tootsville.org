(defpackage dreamhost
  (:use :cl :alexandria :drakma :puri)
  (:export
   #:*api-key*
   #:dns-add-record
   #:dns-list-records
   #:dns-remove-record
   ))

(in-package :dreamhost)

(defvar *api-key* (values)
  "The  Dreamhost  API  Key  to  be used.

Generate one in the Panel at
https://panel.dreamhost.com/?tree=home.api")

(define-condition dreamhost-api-error (error) ())
(define-condition dreamhost-api-warning (warning) ())

(define-condition no-record (dreamhost-api-error) ())
(define-condition no-type (dreamhost-api-error) ())
(define-condition no-value (dreamhost-api-error) ())
(define-condition no-such-zone (dreamhost-api-error) ())
(define-condition CNAME-must-be-only-record (dreamhost-api-error) ())
(define-condition CNAME-already-on-record (dreamhost-api-error) ())
(define-condition record-already-exists-not-editable (dreamhost-api-error) ())
(define-condition record-already-exists-remove-first (dreamhost-api-error) ())
(define-condition internal-error-updating-zone (dreamhost-api-error) ())
(define-condition internal-error-could-not-load-zone (dreamhost-api-error) ())
(define-condition internal-error-could-not-add-record (dreamhost-api-error) ())
(define-condition dreamhost-api-error-with-details (dreamhost-api-error)
  ((details :initarg :details :reader dreamhost-error-details)))
(define-condition invalid-record (dreamhost-api-error-with-details) ())
(define-condition invalid-type (dreamhost-api-error-with-details) ())
(define-condition invalid-value (dreamhost-api-error-with-details) ())



(defgeneric validate-dns-value (type value)
  (:documentation
   "Returns VALUE in string form valid for a DNS record of type TYPE")
  (:method ((type (eql :a)) (value string))
    value)
  (:method ((type (eql :cname)) (value string))
    value)
  (:method ((type (eql :ns)) (value string))
    value)
  (:method ((type (eql :ptr)) (value string))
    value)
  (:method ((type (eql :naptr)) (value string))
    value)
  (:method ((type (eql :srv)) (value string))
    value)
  (:method ((type (eql :txt)) (value string))
    value)
  (:method ((type (eql :aaaa)) (value string))
    value))

(defun dns-add-record (name type &optional value comment)
  "Add a DNS record for NAME of TYPE; VALUE is determined by TYPE.

Adds  a  new  DNS record  to  a  domain  you  already have  hosted  with
DreamHost.  However, you  cannot add  dreamhosters.com records.  Keep in
mind DNS changes  may take a while to  propagate.

\textbf{type}: A, CNAME, NS, PTR, NAPTR, SRV, TXT, or AAAA

\textbf{Result success:}
record_added

\textbf{Possible errors:}
no_record
no_type
no_value
invalid_record (may have specifics after a tab)
invalid_type (may have specifics after a tab)
invalid_value (may have specifics after a tab)
no_such_zone
CNAME_must_be_only_record
CNAME_already_on_record
record_already_exists_not_editable
record_already_exists_remove_first
internal_error_updating_zone
internal_error_could_not_load_zone
internal_error_could_not_add_record"
  (check-type type (member '(A CNAME NS PTR NAPTR SRV TXT AAAA)
                           :test #'string-equal))
  (check-type name string)
  (let ((v (validate-dns-value (intern (string-upcase type) :keyword) value)))
    )
  )

(defun dns-list-records ()
  "
dns-list_records

Dump a list of all DNS records for all domains (not including registrations)
on all accounts you have access to.  Please note that this skips the
dreamhosters.com, dreamhost.com, dreamhostps.com, and newdream.net zones.

Command 	dns-list_records
Values 	(none)
Result

success
account_id zone record type value comment editable
1 718apts.com 718apts.com A 1.2.3.4 0
1 718apts.com 718apts.com MX 0 mx1.balanced.dreamhost.com. 0
1 718apts.com 718apts.com MX 0 mx2.balanced.dreamhost.com. 0
1 718apts.com 718apts.com NS ns1.dreamhost.com. 0
1 718apts.com 718apts.com NS ns2.dreamhost.com. 0
1 718apts.com 718apts.com NS ns3.dreamhost.com. 0
1 718apts.com test.718apts.com CNAME ghs.google.com. A test I did. 1
Possible values 	type : A,MX,NS,CNAME,PTR,NAPTR,TXT,SRV,AAAA, or A6

editable : 0 or 1
Possible errors 	(none)
")



(defun dns-remove-record (name)
"Commands
dns-remove_record

Removes an existing editable DNS record you have with DreamHost.  However,
you cannot remove dreamhosters.com records.  Keep in mind DNS changes may
take a while to propagate. 

 Command dns-remove_record Values

record : The full name of the record you'd like to remove, e.g., testing.groo.com
type : The type (see dns-add_record) of the record you'd like to remove.
value : The value (see dns-add_record) of the record you'd like to remove.
Result

success
record_removed
Possible errors

no_record
no_type
no_value
no_such_record
no_such_type
no_such_value
not_editable
internal_error_could_not_destroy_record
internal_error_could_not_update_zone

")



(defun register-dns-name (name ipv4-address)
"- name: register DNS name

shell: 'curl -X GET \"https://api.dreamhost.com/?key={{ api_key
}}&type=A&unique_id={{ fqdn | to_uuid }}&cmd=dns-add_record&record={{ fqdn
}}&value={{ public_v4 }}\"'
")

"
https://help.dreamhost.com/hc/en-us/articles/217555707-DNS-API-commands



What values does DreamHost's API use?
Required

key
An API Key that you need to generate via the web panel.

In order for a sub-account to create an API Key, it must be granted 'Billing' Account Privileges.

Please note that if a sub-account creates an API Key, it will only be
visible to the sub-account on the API Key page in the panel.  The primary
account owner will not be able to view the sub-account's API Key in their
panel.

cmd The command you'd like to run.  When you create your key you pick what
command(s) it may access.  unique_id You may pass a new unique_id (max
length: 64 chars) when you submit a request, to make sure you don't
accidentally submit the same command twice.  Note that only successful
queries \"use up\" a unique_id.  DreamHost recommends using UUIDs for this
purpose if you'd like.  The unique_id you use only applies to your specific
API key, so you never need to worry about any other users already \"using\"
a specific unique_id.  Optional

format
The format you want to receive output in. Valid formats are:

tab (default)
xml
json
perl
php
yaml
html

account

The account number to perform operations under.  Defaults to your own
account, or the first account you have access to otherwise.


Rate limit

In order to ensure that DreamHost systems remains stable and responsive, the
rate at which certain API commands or groups of API commands can be run by
the same user has been limited.  These limits are relatively generous and
very few users ever run into them.  Limits are usually set per hour or per
day, although some commands may have limits in shorter periods of time such
as 10 minutes.  When API calls fail for any reason (for example if you tried
to create a user that already existed), it does not count against your
limits (note that this also means that failing because you exceeded the rate
limit also does not count).  When you do run into a limit, the error
returned is:

error
slow_down_bucko (detailed info about type of limit reached after a tab)

If you run into this error, you should consider ways in which you could
reduce the frequency that you are calling the API.  Most likely you would
only run into these limits if you have a script or automated program that
loops to repeatedly make API calls.  So you can simply slow down the rate at
which your script runs, or make sure that it keeps track of how many
commands it has issued in the last hour/day.

Test Account

If you wish to test the DreamHost API without having your own account, you
may use the following API Key:

6SHU5P2HLDAYECUM

This account only has access to \"list\" functions however (and only
user-list_users_no_pw, not user-list_users) , as well as
dreamhost_ps-set_size, dreamhost_ps-set_settings, and dreamhost_ps-reboot to
ps7093.

An example

https://api.dreamhost.com/?key=6SHU5P2HLDAYECUM&cmd=user-list_users_no_pw&unique_id=4082432&format=perl

If you are writing a simple bash script to interface with DreamHost's API,
this may help as a start:

#!/bin/sh
####
#### USE THIS SCRIPT AT YOUR OWN RISK.
####

PS=$1

KEY=YOUR-KEY-HERE
UUID=`uuidgen`
CMD=user-list_users_no_pw

if [ $# -lt 1 ]; then
echo usage: `basename $0` [hostname]
exit 1
fi

if [ $PS = "" ]; then
PS=`hostname | cut -f1 -d.`
fi

# ARGS='other_http_get_arguments_for_the_DreamHost_cmd_that_you_are_using=4&foo=123'
LINK=https://api.dreamhost.com/?key=$KEY&unique_id=$UUID&cmd=$CMD&ps=$PS&$ARGS

RESPONSE=`wget -O- -q $LINK`

echo $LINK
echo $RESPONSE
if ! (echo $RESPONSE | grep -q 'success'); then
exit 1
fi

"