;;; ../nixpkgs/program-cfg/doom/notes.el -*- lexical-binding: t; -*-

(defvar yf/lib/youtube-id-regex
  (rx-let
      ((id-char (not (in whitespace ?= ?& ?? ?/ ?#)))
       (video-id (group-n 1 (repeat 11 id-char))))
    (rx
     (or
      video-id
      (seq
       (? "http" (? ?s) "://")
       (or
        "youtu.be/"
        (seq "youtube.com/"
             (or
              (seq (+ (not "/")) "/" (+ anychar) "/")
              (seq (or "v" "e") (? "mbed") "/")
              (seq (* anychar) (or "?" "&") "v="))))
       video-id)))))

(defun yf/lib/notes-parse-youtube-id (candidate-str)

  ))

(defun yf/notes/create-youtube-note
    (interactive
     )
  )
