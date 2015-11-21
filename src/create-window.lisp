;;;; 新建一个窗口
;;;; 在这里我只是创建了一个窗口，其他什么事情也没有做，在
;;;; 下一个教程中我将在窗口中画一个三角形

(defun render (window)
  (gl:clear :color-buffer)
  (gl:flush)
  (sdl2:gl-swap-window window))

(defun create-window (&key (width 320) (height 480))
  (sdl2:with-init (:everything)
    (multiple-value-bind (window renderer)
        (sdl2:create-window-and-renderer width height '(:shown :opengl))
      (sdl2:with-gl-context (gl window)
        (sdl2:gl-make-current window gl)
	;; 设置窗口的尺寸和投影矩阵（正交投影）
        (gl-ortho-setup :width width :height height)

	(gl:matrix-mode :modelview)
	(gl:load-identity)

	;; 设置背景的颜色为灰色
	(gl:clear-color 0.5 0.5 0.5 1.0)

	(sdl2:with-event-loop (:method :poll)
	  (:keyup () (sdl2:push-event :quit))
	  (:mousebuttondown () (sdl2:push-event :quit))
	  (:idle () (render window))
	  (:quit () t))
	
	(sdl2:destroy-renderer renderer)
	(sdl2:destroy-window window)))))
