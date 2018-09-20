(in-package :Tootsville)

(defendpoint (:get "/users/me" "application/json")
  (with-player ()
    (list 200 nil
          (list :hello "Hello, new user"
                :fake "This is a totes fake response"
                :toots "/users/me/toots.json"))))


(defendpoint (get "/users/me/toots" "application/json")
  (with-player ()
    (list 200 (list :last-modified (header-time (yesterday)))
          (list :toots (player-toots)))))
(defendpoint (put "/users/me/toots/:toot-name" "application/json")
  (with-player ()
    (assert-my-character toot-name)
    (list 200 nil (toot-info (find-toot-by-name toot-name)))))

(defendpoint (post "/users/me/toots" "application/json")
  (with-player ()
    (error 'unimplemented)))

(defendpoint (delete "/users/me/toots/:toot-name" "application/json")
  (with-player ()
    (assert-my-character toot-name)
    (error 'unimplemented)))

(defendpoint (get "/toots/:toot-name" "application/json")
  (check-arg-type toot-name toot-name)
  (with-player ()
    (list 200
          `(:last-modified ,(header-time))
          (if-let (toot (find-toot-by-name toot-name))
            (toot-info toot)
            `(:is-a "toot"
                    :name ,(string-capitalize toot-name)
                    :avatar "ultraToot"
                    :child-p nil
                    :sensitive-p t
                    :online-p t
                    :last-seen ,(local-time:format-timestring
                                 nil (local-time:now))
                    :exists-p "maybe?")))))
