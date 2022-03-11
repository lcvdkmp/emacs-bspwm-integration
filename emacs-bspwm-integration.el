;;; emacs-bspwm-integration/emacs-bspwm-integration.el -*- lexical-binding: t; -*-
;; Inspired by https://sqrtminusone.xyz/posts/2021-10-04-emacs-i3/

(defmacro bspc (&rest args)
     `(start-process "emacs-bspwm-integration" nil "/usr/bin/bspc" ,@args))

(defun emacs-bspwm-integration (command)
  (pcase command
    ((rx bos "node -f")
     (emacs-bspwm-goto-window
      (intern (elt (split-string command) 2))))
    ((rx bos "node -s")
     (emacs-bspwm-move-window
      (intern (elt (split-string command) 2))))
    ((rx bos "node -p")
     (emacs-bspwm-split-window
      (intern (elt (split-string command) 2))))
    ("node -c" (evil-quit))
    (- (bspc command))))

(defun emacs-bspwm-split-window (dir)
  (pcase dir
    ('west (evil-window-vsplit))
    ;; Also move east to emulate a split to the east (focus the new window)
    ('east (evil-window-vsplit)
           (emacs-bspwm-goto-window 'east))
    ('north (evil-window-split))
    ;; Also move south to emulate a split to the south (focus the new window)
    ('south (evil-window-split)
            (emacs-bspwm-goto-window 'south))))

(defun convert-bspwm-direction (dir)
  (pcase dir
    ('north 'up)
    ('south 'down)
    ('west 'left)
    ('east 'right)))

(defun emacs-bspwm-goto-window (dir)
  (let* ((tdir (convert-bspwm-direction dir))
         (target-window (windmove-find-other-window tdir)))
    (if (or (null target-window) (window-minibuffer-p target-window))
        (bspc "node" "-f" (symbol-name dir)))
      (windmove-do-window-select tdir)))

(defun emacs-bspwm-direction-exists-p (dir)
  (seq-some (lambda (dir)
              (let ((win (windmove-find-other-window dir)))
                (and win (not (window-minibuffer-p win)))))
            (pcase dir
              ('width '(left right))
              ('height '(up down)))))


(defun emacs-bspwm-move-window (dir)
  (let* ((tdir (convert-bspwm-direction dir))
         (target-window (windmove-find-other-window tdir))
         (other-direction (emacs-bspwm-direction-exists-p
                          (pcase tdir
                            ('up 'width)
                            ('down 'width)
                            ('left 'height)
                            ('right 'height)))))
    (cond
     ((and target-window (not (window-minibuffer-p target-window)))
      (window-swap-states (selected-window) target-window))
     (other-direction (evil-move-window tdir))
     (t (bspc "node" "-s" (symbol-name dir))))))

(provide 'emacs-bspwm-integration)
