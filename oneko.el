;;; oneko.el --- Oneko -*- lexical-binding: t; -*-

;; Package-Requires ((posframe "1.4.4"))

;;; Code:
(require 'posframe)
(require 'calc-misc)

(defvar oneko-image-dir
  (expand-file-name "img" (file-name-directory
			   (or load-file-name buffer-file-name)))
  "Directory containing the images for oneko.el.")

(defun oneko--image (name)
  "Get the image with NAME."
  (expand-file-name (format "%s.gif" name) oneko-image-dir))

(defvar oneko-position nil)
(defvar oneko-buffer nil)

(defun oneko--initialize ()
  "Initialize oneko for display."
  (setq oneko-position (cdr (mouse-pixel-position)))
  (unless oneko-buffer
    (setq oneko-buffer (generate-new-buffer "oneko")))

  (posframe-show oneko-buffer
		 :width 8
		 :height 4
		 :position oneko-position
		 :override-parameters '((alpha-background . 0)
					(alpha . 0)))
  (with-current-buffer oneko-buffer
    (erase-buffer)
    (insert-image (create-image (oneko--image "right_2") nil nil :scale 1.0))))

(defvar oneko-speed 10)
(defvar oneko-wait 0)
(defvar oneko-sleepy 0)

(defvar oneko-animation-frame 1)
(defun oneko--get-frame (name)
  (if (= oneko-animation-frame 1)
      (setq oneko-animation-frame 2)
    (setq oneko-animation-frame 1))
  (oneko--image (format "%s_%d" name oneko-animation-frame)))

(defun oneko--update-image (name &optional specific)
  (with-current-buffer oneko-buffer
    (erase-buffer)
    (let ((image (if specific
		     (oneko--image name)
		   (oneko--get-frame name))))
      (insert-image (create-image image nil nil
				  :mask 'heuristic)))))

(defun oneko--walk (dir)
  (let ((x (car dir))
	(y (cdr dir))
	(range 0.4))
    (cond
     ((and (> x range) (< (abs y) range))
      (oneko--update-image "right"))
     ((and (< x range) (< (abs y) range))
      (oneko--update-image "left"))
     ((and (< (abs x) range) (< y (- range)))
      (oneko--update-image "up"))
     ((and (< (abs x) range) (> y range))
      (oneko--update-image "down"))
     ((and (> x range) (< y (- range)))
      (oneko--update-image "topright"))
     ((and (< x range) (< y (- range)))
      (oneko--update-image "topleft"))
     ((and (> x range) (> y range))
      (oneko--update-image "bottomright"))
     ((and (< x range) (> y range))
      (oneko--update-image "bottomleft"))

     )
    ))

(defun oneko--update ()
  (let* ((mouse-pos (cdr (mouse-pixel-position)))
	 (vector (oneko--vector-from-to oneko-position mouse-pos)))
    (if (> (oneko--vector-length vector) 5.0) ;; should move
	(if (> oneko-wait 0) ;; should wait
	    (progn
	      (setq oneko-wait (- oneko-wait 1))
	      (cond
	       ((< oneko-wait 6)
		(oneko--update-image "wakeup" t))
	       ((< oneko-wait 3)
		(oneko--update-image "still" t))))
	  
	  ;; have waited
	  (let* ((normalized (oneko--vector-normalize vector))
		 (multiplied (oneko--vector-multiply normalized oneko-speed))
		 (added (oneko--vector-add oneko-position multiplied))
		 (new-position (oneko--vector-round added)))
	    (setq oneko-sleepy 0)
	    (setq oneko-position new-position)
	    (oneko--walk normalized)
	    (posframe-show oneko-buffer
			   :position oneko-position
			   :override-parameters '((internal-border-width . 0)
						  (border-width . 0)
						  (child-frame-border-width . 0)
						  (no-accept-focus . t)
						  (alpha . 0))
			   )))
      ;; still
      (progn
	(cond
	 ((> oneko-sleepy 50)
	  (when (= (% oneko-sleepy 4) 0)
	    (oneko--update-image "sleep")))
	 ((and (> oneko-sleepy 30) (< oneko-sleepy 40))
	  (oneko--update-image "yawn" t))
	 (t
	  (oneko--update-image "still" t)))
	
	(setq oneko-wait 10)
	(setq oneko-sleepy (+ oneko-sleepy 1))))
    ))

(defvar oneko-timer nil)
(defvar oneko-interval 0.1)
(defun oneko-start ()
  "Start oneko."
  (interactive)
  (oneko--initialize)
  (setq oneko-timer
	(run-at-time 0 oneko-interval #'oneko--update)))

(defun oneko-stop ()
  "Stop oneko."
  (interactive)
  (cancel-timer oneko-timer)
  (posframe-hide oneko-buffer))

(defun oneko--vector-from-to (a b)
  (let ((x1 (car a))
	(y1 (cdr a))
	(x2 (car b))
	(y2 (cdr b)))
    (let ((diff-x (- x2 x1))
	  (diff-y (- y2 y1)))
      (cons diff-x diff-y))))

(defun oneko--vector-add (a b)
  (cons (+ (car a) (car b))
	(+ (cdr a) (cdr b))))

(defun oneko--vector-multiply (vec by)
  (cons (* (car vec) by)
	(* (cdr vec) by)))

(defun oneko--vector-round (vec)
  (cons (ceiling (car vec))
	(ceiling (cdr vec))))

(defun oneko--vector-length (vec)
  (let ((x (* (car vec) (car vec)))
	(y (* (cdr vec) (cdr vec))))
    (sqrt (+ x y))))

(defun oneko--vector-normalize (vec)
  (let ((length (oneko--vector-length vec)))
    (cons (/ (car vec) length) (/ (cdr vec) length))))

(provide 'oneko)
;;; oneko.el ends here
