(asdf:defsystem "cl-ca"
 :description "cellular automaton library"
 :version "0.0.1"
 :author "Lukas Epple <post@lukasepple.de>"
 :licence "Public Domain"
 :components ((:file "packages")
	 (:file "cl-ca" :depends-on ("packages"))))

