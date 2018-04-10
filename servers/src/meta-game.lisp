(in-package :tootsville)

(defun approximate-player-count () 1)
(defun server-full-player-count () 200)
(defun server-online-p () t)
(defun server-full-p ()
  (>= (approximate-player-count) (server-full-player-count)))

(defendpoint (get "/tootsville/game-server/")
  (render #p"game-server.html"
          `(:server-status
            ((:players-online .,(approximate-player-count))
             (:players-full-limit .,(server-full-player-count))
             (:online-status .,(if (server-online-p)
                                   (if (server-full-p)
                                       "OnlineFull"
                                       "Online")
                                   "OfflineTemporarily"))
             (:online-status-en .,(if (server-online-p)
                                      (if (server-full-p)
                                          "online, but full"
                                          "online and ready")
                                      "temporarily offline"))
             ))))
