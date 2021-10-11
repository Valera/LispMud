;;; world.lisp

(in-package :cl-user)
(defpackage :lispmud/world
  (:use :cl)
  (:import-from :iter #:iter #:for #:collect)
  (:import-from :lispmud/core-utils #:pvalue)
  (:import-from :lispmud/core-zone #:load-zone))
(in-package :lispmud/world)

(defun load-world (world-filename)
  "Загружает описание мира из файла, чьё имя передано в аргументе.
  Читает список файлов из файла мира, загружает все зоны, соответствующие этим файлам.
  Производит соединение точек входа и выхода."
  (let* ((file-content (alexandria:read-file-into-string world-filename))
         (list-of-zone-files (jonathan:parse file-content))
         (zones (iter
                  (for zone-filename in list-of-zone-files)
                  (for merged-zone-filename = (uiop:merge-pathnames* zone-filename world-filename))
                  (pvalue merged-zone-filename) ; TODO: base zone-filename on direcotory of world-filename
                  (collect (load-zone merged-zone-filename)))))
    (link-zones zones)))

(defun link-zones (zone-list)
  "Создаёт ссыки между зонами. Например, по порталу
   из одной зоны можно попасть в другую."
  (pvalue zone-list)
  zone-list)
