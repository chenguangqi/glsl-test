
(in-package #:glsl-test)

(defun gl-ortho-setup (&key (width 500) (height 500))
  "Set up 1:1 pixel ortho matrix"
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:ortho 0 width height 0 -1 1))

(defun compile-and-check-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (unless (gl:get-shader shader :compile-status)
    (gl:get-shader-info-log shader)))

(defun gl-init-shaders ()
  (let ((v-shader (gl:create-shader :vertex-shader))
        (f-shader (gl:create-shader :fragment-shader)))
    (compile-and-check-shader v-shader *vertex-shader*)
    (compile-and-check-shader f-shader *fragment-shader*)
    (let ((program (gl:create-program)))
      (if (= 0 program)
          (error "Error creating program")
          (progn
            (gl:attach-shader program v-shader)
            (gl:attach-shader program f-shader)
            (gl:link-program program)
            (format t "~A~%" (gl:get-program-info-log program))
            (gl:use-program program)))
      program)))

(defun file-to-string (filename)
  (with-output-to-string (s)
    (format s "~%~{~A~%~}" 
	    (with-open-file (stream filename)
	      (when stream
		(loop for line = (read-line stream nil nil)
		   while line collect line))))))

(defun load-shader (vertex-src fragment-src)
  "Create shader programe and return id of the program."
  (let ((v-shader (gl:create-shader :vertex-shader))
	(f-shader (gl:create-shader :fragment-shader)))
    (compile-and-check-shader v-shader vertex-src)
    (compile-and-check-shader f-shader fragment-src)
    (let ((program (gl:create-program)))
      (if (= 0 program)
	  (error "Error create program")
	  (progn
	    (gl:attach-shader program v-shader)
	    (gl:attach-shader program f-shader)
	    (gl:link-program program)
	    (format t "~A~%" (gl:get-program-info-log program))
	    (gl:delete-shader v-shader)
	    (gl:delete-shader f-shader)
	    program)))))
     
