;;; init-fold.el --- Unified code folding -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; Customization
;;----------------------------------------------------------------------------

(defcustom fold-outline-indent-modes
  '(python-ts-mode yaml-ts-mode haskell-ts-mode scala-ts-mode)
  "Major modes that use outline-indent-minor-mode for folding.
Other prog modes use hs-minor-mode."
  :type '(repeat symbol)
  :group 'editing)

;;----------------------------------------------------------------------------
;; Variables
;;----------------------------------------------------------------------------

(defvar fold-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c-" 'fold-hide)
    (define-key map "\C-c=" 'fold-show)
    (define-key map "\C-c_" 'fold-hide-all)
    (define-key map "\C-c+" 'fold-show-all)
    map))

(defvar-local fold--hide-fn nil)
(defvar-local fold--show-fn nil)
(defvar-local fold--hide-all-fn nil)
(defvar-local fold--show-all-fn nil)

;;----------------------------------------------------------------------------
;; Commands
;;----------------------------------------------------------------------------

(defun fold-hide ()     (interactive) (funcall fold--hide-fn))
(defun fold-show ()     (interactive) (funcall fold--show-fn))
(defun fold-hide-all () (interactive) (funcall fold--hide-all-fn))
(defun fold-show-all () (interactive) (funcall fold--show-all-fn))

;;----------------------------------------------------------------------------
;; Backend setup
;;----------------------------------------------------------------------------

(defun fold-hs-set-up-overlay (ov)
  (overlay-put ov 'display
               (propertize "▼"
                           'mouse-face 'highlight
                           'help-echo "mouse-1: show hidden lines"
                           'keymap '(keymap (mouse-1 . hs-toggle-hiding)))))

;;----------------------------------------------------------------------------
;; Minor mode
;;----------------------------------------------------------------------------

(define-minor-mode fold-minor-mode
  "Unified code folding minor mode."
  :keymap fold-minor-mode-map)

(define-globalized-minor-mode fold-global-mode
  fold-minor-mode fold-enable)

(defun fold-enable ()
  (when (or (memq major-mode fold-outline-indent-modes)
            (derived-mode-p 'prog-mode))
    (let ((use-outline-indent (memq major-mode fold-outline-indent-modes)))
      (cond
       (use-outline-indent
        (outline-indent-minor-mode)
        (setq fold--hide-fn     #'outline-indent-close-fold)
        (setq fold--show-fn     #'outline-indent-open-fold)
        (setq fold--hide-all-fn #'outline-indent-close-folds)
        (setq fold--show-all-fn #'outline-indent-open-folds))
       ((derived-mode-p 'prog-mode)
        (hs-minor-mode)
        (setq fold--hide-fn     #'hs-hide-block)
        (setq fold--show-fn     #'hs-show-block)
        (setq fold--hide-all-fn #'hs-hide-all)
        (setq fold--show-all-fn #'hs-show-all)))
      (fold-minor-mode 1))))

;;----------------------------------------------------------------------------
;; Package config
;;----------------------------------------------------------------------------

(with-eval-after-load 'hideshow
  (setq hs-set-up-overlay #'fold-hs-set-up-overlay))

(use-package outline-indent
  :commands outline-indent-minor-mode
  :config
  (setq outline-indent-ellipsis "▼"))

(provide 'init-fold)
