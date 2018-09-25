(in-package :Tootsville)

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
