(require 'cl-lib)

(cl-defmethod project-root ((project (head projectile)))
  (cdr project))

(cl-defmethod project-files ((project (head projectile)) &optional dirs)
  (let ((root (project-root project)))
    (mapcar (lambda (f)
              (concat root f))
            (projectile-project-files root))))

(defun project-projectile (dir)
  "Return projectile project of form ('projectile . ROOT-DIR) for DIR"
  (let ((root (projectile-project-root dir)))
    (when root
      (cons 'projectile root))))

(provide 'project-projectile)
