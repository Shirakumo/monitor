(asdf:defsystem #:monitor
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :serial T
  :version "1.0.0"
  :components ((:file "module")
               (:file "db")
               (:file "alert")
               (:file "task-runner")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :auth)
               (:interface :mail)
               (:interface :relational-database)
               :machine-measurements
               :precise-time
               :fuzzy-dates
               :parse-float
               :cl-ppcre
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
