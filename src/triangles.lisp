(in-package :glsl-test)

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

(defun get-shader-source (file)
  (with-output-to-string (s)
    (format s "~%~{~A~%~}" 
	    (with-open-file (stream file)
	      (when stream
		(loop for line = (read-line stream nil nil)
		   while line collect line))))))
  
(defun load-shader ()
  ;; A program object is a collection of shader objects to be used
  ;; together in a single pipeline for rendering objects. To create a
  ;; program, you first create the individual shaders. Then you attach
  ;; the shaders to the program and link the program together.
  (let ((vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader)))
    (gl:shader-source vs (get-shader-source #P"shaders/triangles.vert"))
    (gl:compile-shader vs)
    (gl:shader-source fs (get-shader-source #P"shaders/triangles.frag"))
    (gl:compile-shader fs)
    ;; If the shader doesn't compile, you can print errors with:
    ;(print (gl:get-shader-info-log vs))
    ;(print (gl:get-shader-info-log fs))
		
    (let ((program (gl:create-program)))
      ;; You can attach the same shader to multiple different programs.
      (gl:attach-shader program vs)
      (gl:attach-shader program fs)
      ;; Don't forget to link the program after attaching the
      ;; shaders. This step actually puts the attached shader together
      ;; to form the program.
      (gl:link-program program)
      ;; If we want to render using this program object, or add
      ;; uniforms, we need to use the program. This is similar to
      ;; binding a buffer.
      (gl:use-program program))))


(defun basic-test ()
  "一个测试GLSL编程的例子程序"
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (let ((controllers ())
              (haptic ()))
	  
          ;; basic window/gl setup
          (format t "Setting up window/gl.~%")
          (finish-output)
          (sdl2:gl-make-current win gl-context)
	  
	  
          ;(gl:viewport 0 0 800 600)
          ;(gl:matrix-mode :projection)
          ;(gl:ortho -2 2 -2 2 -2 2)
	  (gl-ortho-setup :width 800 :height 600)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0.0 0.0 0.0 1.0)
          (gl:clear :color-buffer)
	  
	  ;; 
	  (let ((buffers (gl:gen-buffers 1)))
	    (let ((vertex-buffer (elt buffers 0)))
	      (gl:bind-buffer :array-buffer vertex-buffer)
	      (let ((arr (gl:alloc-gl-array :float 12))
		    (verts #(-0.90 -0.90
			      0.85 -0.90 
			     -0.90  0.85 
			      0.90 -0.85
			      0.90  0.90
			     -0.85  0.90)))
		(dotimes (i (length verts))
		  (setf (gl:glaref arr i) (aref verts i)))
		(gl:buffer-data :array-buffer :static-draw arr)
		(gl:free-gl-array arr))
	      
	      ;; 0 is always reserved as an unbound object.
	      ;(gl:bind-buffer :array-buffer 0)
	      
	      ;; Vertex array objects manage which vertex attributes are
	      ;; associated with which data buffers. 
	      (let ((vertex-array (gl:gen-vertex-array)))
		(gl:bind-vertex-array vertex-array)

		;; To associate our VBO data with this VAO, we bind it, specify
		;; which vertex attribute we want to associate it with, and specify
		;; where the data comes from.
		(gl:bind-buffer :array-buffer vertex-buffer)
		;; In this program, we use attribute 0 for position. If you had
		;; per-vertex normals, you could use a different attribute for those
		;; as well.
		(gl:enable-vertex-attrib-array 0)
		;; Using a null pointer as the data source indicates that we want
		;; the vertex data to come from the currently bound array-buffer.
		(gl:vertex-attrib-pointer 0 2 :float nil 0 (cffi:null-pointer))
		
		;; Once we're done, we can unbind the VAO, and rebind it when we want to render it.
					;(gl:bind-vertex-array 0)
		)))

	  (load-shader)

	  (format t "Opening game controllers.~%")
          (finish-output)
          ;; open any game controllers
          (loop for i from 0 upto (- (sdl2:joystick-count) 1)
             do (when (sdl2:game-controller-p i)
                  (format t "Found gamecontroller: ~a~%"
                          (sdl2:game-controller-name-for-index i))
                  (let* ((gc (sdl2:game-controller-open i))
                         (joy (sdl2:game-controller-get-joystick gc)))
                    (setf controllers (acons i gc controllers))
                    (when (sdl2:joystick-is-haptic-p joy)
                      (let ((h (sdl2:haptic-open-from-joystick joy)))
                        (setf haptic (acons i h haptic))
                        (sdl2:rumble-init h))))))

          ;; main loop
          (format t "Beginning main loop.~%")
          (finish-output)
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (let ((scancode (sdl2:scancode-value keysym))
                   (sym (sdl2:sym-value keysym))
                   (mod-value (sdl2:mod-value keysym)))
               (cond
                 ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                 ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                 ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
               (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                       sym
                       scancode
                       mod-value)))

            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))

            (:mousemotion
             (:x x :y y :xrel xrel :yrel yrel :state state)
             (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                     x xrel y yrel state))

            (:controlleraxismotion
             (:which controller-id :axis axis-id :value value)
             (format t "Controller axis motion: Controller: ~a, Axis: ~a, Value: ~a~%"
                     controller-id axis-id value))

            (:controllerbuttondown
             (:which controller-id)
             (let ((h (cdr (assoc controller-id haptic))))
               (when h
                 (sdl2:rumble-play h 1.0 100))))

            (:idle
             ()
             (gl:clear :color-buffer)
	     (gl:draw-arrays :triangles 0 12)
             (gl:flush)
             (sdl2:gl-swap-window win))

            (:quit () t))

          (format t "Closing opened game controllers.~%")
          (finish-output)
          ;; close any game controllers that were opened
          ;; as well as any haptics
          (loop for (i . controller) in controllers
             do (progn
                  (sdl2:game-controller-close controller)
                  (sdl2:haptic-close (cdr (assoc i haptic))))))))))
