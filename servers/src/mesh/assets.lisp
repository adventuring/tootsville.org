(in-package :assets)

(defun fetch-bytes (asset-uri callback)
  "Fetches a  Javascript byte array  for the asset URI  indicated, using
 either the WebTorrent channels or a direct HTTP fetch.")
