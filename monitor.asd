(asdf:defsystem #:monitor
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :serial T
  :version "0.0.0"
  :components ((:file "module")
               (:file "db")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :auth)
               (:interface :mail)
               (:interface :relational-database)
               :r-data-model
               :r-oauth
               :r-clip
               :i-json)
  :build-operation "deploy-op"
  :build-pathname
  #+linux "monitor.run"
  #+darwin "monitor.app"
  #+(or windows win32) "monitor"
  #-(or linux darwin win32 windows) "monitor.o"
  :entry-point "org.shirakumo.radiance.core::startup-binary")
