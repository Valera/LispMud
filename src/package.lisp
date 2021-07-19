(in-package :cl-user)

(defpackage :lispmud
  (:nicknames :lm)
  (:use :cl)
  (:import-from :lispmud/core-room
                *exits* reverse-direction direction-name myroom
                short-description description place-type mobs players items-on-floor triggers
                west-exit east-exit south-exit north-exit editor-info
                #:exit dest-room can-pass passage door hotel portal
                dx-for-direction dy-for-direction
                room-about message-to-visitors
                add-trigger process-room-triggers
                myroom service-room bank-room hotel-room)
  (:shadow store-room guild-room )
  (:import-from :lispmud/shop #:shop-room)
  (:import-from :lispmud/core-zone
                #:zone
                #:name #:id #:map-array #:entry-rooms #:mobs-spec #:mobs-counters
                #:mobs-max-numbers #:lock #:message-queue
                #:save-zone #:load-zone #:link-rooms
                #:unlink #:unlink-orphaned-rooms #:start-work #:end-work)
  (:export
   ;; core-room.lisp
   *exits* reverse-direction direction-name myroom
   short-description description place-type mobs players items-on-floor triggers
   west-exit east-exit south-exit north-exit editor-info
   #:exit dest-room can-pass passage door hotel portal
   dx-for-direction dy-for-direction
   room-about message-to-visitors
   add-trigger process-room-triggers
   myroom service-room bank-room shop-room store-room guild-room hotel-room
   ;; core-zone.lisp
   #:zone
   #:name #:id #:map-array #:entry-rooms #:mobs-spec #:mobs-counters
   #:mobs-max-numbers #:lock #:message-queue
   #:queuue-mesg #:get-mesg #:event-loop #:save-zone #:load-zone #:link-rooms
   #:unlink #:unlink-orphaned-rooms #:start-work #:end-work))
