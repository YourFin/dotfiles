module `
(letrec ((parent (lambda (path) (substring (file-name-directory path) 0 -1)))
         (go (lambda (inpath)
               (let ((path (funcall parent inpath))
                     (fname (file-name-nondirectory path)))
                 (if (not (equal (capitalize fname) fname))
                     ""
                     (concat (go path) fname "."))))))
  (concat (funcall go buffer-file-name) (file-name-sans-extension buffer-file-name)))` where