;;; world.lisp

(in-package :lispmud)

(defun load-world (world-filename)
  "Загружает описание мира из файла, чьё имя передано в аргументе. 
  Читает список файлов из файла мира, загружает все зоны, соответствующие этим файлам. 
  Производит соединение точек входа и выхода."
  (with-open-file (stream world-filename)
    (let ((zones (iter
		   (for zone-filename in (read stream))
		   (pvalue zone-filename)
		   (collect (load-zone2 zone-filename)))))
      (link-zones zones))))

(defun link-zones (zone-list)
  "Создаёт ссыки между зонами. Например, по порталу
   из одной зоны можно попасть в другую."
  ())
  

;    (destructuring-bind (&