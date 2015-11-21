;;;; glsl-test.asd

(asdf:defsystem #:glsl-test
  :description "Describe glsl-test here"
  :author "Chen Guanqi <348249063@qq.com>"
  :license "BSD"
  :depends-on (#:cffi #:cl-opengl #:sdl2)
  :serial t
  :pathname "src"
  
  :components ((:file "package")
	       (:file "util")
	       (:file "create-window")
	       (:file "show-triangle")
	       (:module "shares"
			:components
			())
	       (:module "assets"
			:components
			())))
