(in-package :Tootsville)

(defendpoint (:get "/world" "application/json")
  (error 'unimplemented))

(defendpoint (get "/world/tootanga/:x-coord/:y-coord/:z-coord" "application/json")
  "Get the information about the area near (X-COORD,Y-COORD,Z-COORD)

The terrain and objects in that area, characters, &c. will be returned.

Your character must be able to observe that general area. No peeking!
"
  (if-let (doc (clouchdb:get-document 
                (format nil "T:~36R,~36R,~36R" x-coord y-coord z-coord) ))
    doc
    (spawn-terrain :tootanga x-coord y-coord z-coord)))

