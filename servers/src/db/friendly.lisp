(in-package :Tootsville)

(defrecord db.person (:friendly "people")
  (uuid uuid)
  (display-name string) 
  (given-name string)
  (surname string)
  (date-of-birth timestamp)
  (age number)
  (sensitivep yornp)
  (child-code string)
  (gender keyword)
  (lang keyword))

(defgeneric ensure-person (person-or-uuid-or-uid)
  (:method ((person db.person))
    person)
  (:method ((person uuid:uuid))
    (find-record 'db.person :uuid person))
  (:method ((person real))
    (check-type person (real (0) (#.(expt 2 128)))) 
    (find-record 'db.person :uuid person))
  (:method ((person cons))
    (assert (= 2 (length person)))
    (check-type (car person) string)
    (check-type (cdr person) string)
    (db-select-single-column "credentials" "person"
                             "uid" (car person)
                             "provider" (cdr person))))

(defrecord db.parent-child (:friendly "parent_child")
  (parent uuid ref db.person)
  (child uuid ref db.person))

(defrecord db.credential (:friendly "credentials")
  (uuid uuid)
  (person uuid ref db.person)
  (uid string)
  (provider string)
  (id-token string)
  (auth-token string)
  (refresh-token string)
  (json-info json))

(defrecord db.person-link (:friendly "person_links")
  (uuid uuid)
  (person uuid ref db.person)
  (rel keyword)
  (url uri)
  (label string)
  (provenance string))

(defrecord db.login (:friendly "logins")
  (uuid uuid)
  (person uuid ref db.person)
  (credential uuid ref db.credential)
  (start timestamp)
  (renewed timestamp)
  (last-seen timestamp)
  (origin string))

(defrecord db.avatar (:friendly "avatars" :pull t)
  (id number)
  (moniker string))

(defrecord db.pattern (:friendly "patterns" :pull t)
  (id number)
  (name string))

(defrecord db.toot (:friendly "toots")
  (uuid uuid)
  (name string) 
  (pattern number ref db.pattern)
  (base-color color24)
  (pattern-color color24)
  (pad-color color24)
  (avatar number ref db.avatar)
  (player uuid ref db.person)
  (last-active timestamp)
  (note string))

(defrecord db.wear-slot (:friendly "wear-slots")
  (id number)
  (name string)
  (alternate number)
  (avatar-point keyword)
  (valence number)
  (obstruct-point keyword)
  (obstruct-min number)
  (obstruct-max number))

(defrecord db.avatar-slot (:friendly "avatar_slots")
  (id number)
  (avatar number ref db.avatar)
  (slot keyword)
  (valence number))

(defrecord db.item-template (:friendly "item_templates")
  (id number)
  (name string)
  (default-base-color color24)
  (avatar number ref db.avatar)
  (energy-kind keyword)
  (energy-max number)
  (on-zero keyword)
  (wear-slot number ref db.wear-slot) 
  (weight number))

(defrecord db.item (:friendly "items")
  (uuid uuid)
  (base-color color24)
  (alt-color color24)
  (template number ref db.item-template)
  (energy number))

(defrecord db.inventory-item (:friendly "inventory")
  (person uuid ref db.person)
  (toot uuid ref db.toot)
  (item uuid ref db.item)
  (equipped keyword)) 
