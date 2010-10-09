(in-package :cl-user)

(defpackage :lispmud
  (:use :cl :alexandria :split-sequence :usocket :sb-ext ;:sb-bsd-sockets 
	:sb-gray :iterate :bt)
  (:shadowing-import-from :sb-ext :with-timeout :timeout :timer)
;  (:shadow :action :item)
  (:shadowing-import-from :usocket :socket)
;  (:shadowing-import-from :cl-cairo2 :scale :rotate)
  (:import-from #:sb-thread #:semaphore #:make-semaphore #:semaphore-count
		#:semaphore-name #:signal-semaphore #:wait-on-semaphore)
  (:export 
   ;; core-room.lisp
   *exits* reverse-direction direction-name myroom
   description place-type mobs players items-on-floor triggers
   west-exit east-exit south-exit north-exit
   exit dest-room can-pass passage door hotel portal
   dx-for-direction dy-for-direction
   room-about message-to-visitors
   add-trigger process-room-triggers
   ;; core-zone.lisp
   #:zone
   #:name #:id #:map-array #:entry-rooms #:mobs-spec #:mobs-counters
   #:mobs-max-numbers #:lock #:message-queue
   #:queuue-mesg #:get-mesg #:event-loop #:save-zone #:load-zone #:link-rooms
   #:unlink #:unlink-orphaned-rooms #:start-work #:end-work))
