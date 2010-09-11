(in-package :cl-user)

(defpackage :lispmud
  (:use :cl :alexandria :split-sequence :usocket :sb-ext ;:sb-bsd-sockets 
	:sb-gray :iterate :bt); :gtk :cl-cairo2 :cl-gtk2-cairo)
  (:shadowing-import-from :sb-ext :with-timeout :timeout :timer)
;  (:shadow :action :item)
  (:shadowing-import-from :usocket :socket)
;  (:shadowing-import-from :cl-cairo2 :scale :rotate)
  (:import-from #:sb-thread #:semaphore #:make-semaphore #:semaphore-count
		#:semaphore-name #:signal-semaphore #:wait-on-semaphore))
