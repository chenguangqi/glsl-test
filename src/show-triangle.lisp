;;;; 画一个三角形
;;;; 在这里我们将创建第一个图形，三角形

(in-package #:glsl-test)

(defparameter *triangle-positions*
  '(-1.0 -1.0 0.0
    1.0 -1.0 0.0
    0.0 1.0 0.0))

(defvar *vertex-buffer* nil)

(defvar *vao* 0)
(defvar *program* 0)

(defvar *vertex-shader*
  "
#version 330 core
layout(location = 0) in vec4 vPosition;
void main() {
gl_Position = vPosition;
}
")

(defvar *fragment-shader*
  "
#version 330 core
out vec4 fColor;

void main() {
fColor = vec4(0.0, 0.0, 1.0, 1.0);
}
")

(defun load-vertex-data (lst)
  (let ((a (gl:alloc-gl-array :float (length lst))))
    (dotimes (i (length lst))
      (setf (gl:glaref a i) (nth i lst)))
    (setf *vertex-buffer* a)
    a))

(defun render (window)
  (gl:clear :color-buffer)
  (gl:bind-vertex-array *vao*)
  (gl:use-program *program*)

  (gl:draw-arrays :triangles 0 3)

  (gl:bind-vertex-array 0)
  (gl:flush)
  (sdl2:gl-swap-window window))

(defun initialize-vao ()
  ;; 获取可以使用的VAO和VBO的ID
  (let ((vao (first (gl:gen-vertex-arrays 1)))
        (vbo (first (gl:gen-buffers 1))))
    ;; 绑定VAO并设置为当前顶点数组
    (gl:bind-vertex-array vao)
    ;; 绑定VBO
    (gl:bind-buffer :array-buffer vbo)
    ;; 缓存数据到VBO中
    (gl:buffer-data :array-buffer :static-draw (load-vertex-data *triangle-positions*))
    ;; 设置将数据传递给vertex shader中的0位置对应的变量的方式
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    ;; 解除VAO的绑定
    (gl:bind-vertex-array 0)
    (setf *vao* vao)))

(defun initialize-program ()
  (setf *program*
	(load-shader *vertex-shader* *fragment-shader*)))

(defun show-triangle (&key (width 320) (height 480))
  (sdl2:with-init (:everything)
    (multiple-value-bind (window renderer)
        (sdl2:create-window-and-renderer width height '(:shown :opengl))
      (sdl2:with-gl-context (gl window)
        (sdl2:gl-make-current window gl)

	(initialize-program)
	(initialize-vao)

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
	(progn
	  ;;(gl:delete-program *program*)
	  (gl:free-gl-array *vertex-buffer*))
	(sdl2:destroy-renderer renderer)
	(sdl2:destroy-window window)))))

