;; Лабораторна робота 4, Варіант 3
;; Студент: Булавчук Данило, КВ-23

(defun bubble-pass-recursive-hof (lst key test)
  "Виконує ОДИН прохід 'бульбашки' функціонально.
  Приймає :key та :test."
  (cond 
    ((null (cdr lst))
     (values lst nil))
    
    (t (multiple-value-bind (processed-tail swapped-in-tail-p)
         (bubble-pass-recursive-hof (cdr lst) key test)
       
       (let* ((current-head (car lst))
              (new-head-of-tail (car processed-tail))
              (key-current (funcall key current-head))
              (key-new-head (funcall key new-head-of-tail)))
         
         (if (funcall test key-current key-new-head)
             (values (cons new-head-of-tail (cons current-head (cdr processed-tail)))
                     t)

             (values (cons current-head processed-tail)
                     swapped-in-tail-p) 
             ))))))

(defun sort-functional-hof (lst &key (key #'identity) (test #'>))
  "Реалізує бульбашкове сортування (ЛР3, Варіант 3) з параметрами :key та :test."
  (if (or (null lst) (null (cdr lst)))
      lst
      (multiple-value-bind (new-list swapped-p)
          (bubble-pass-recursive-hof lst key test)
        
        (if swapped-p
            (sort-functional-hof new-list :key key :test test)
            new-list)
        )))

(defun check-sort-hof (name input key test expected)
  (format t "~:a: ~:[FAILED~;passed~]~%"
          name
          (equal (sort-functional-hof input :key key :test test) expected)))

(defun test-part-1 ()
  (check-sort-hof "Test 1 (Стандартне сортування)"
                  '(5 1 4 2 8)
                  #'identity
                  #'>
                  '(1 2 4 5 8))
                  
  (check-sort-hof "Test 2 (Сортування за спаданням)"
                  '(5 1 4 2 8)
                  #'identity
                  #'<
                  '(8 5 4 2 1))

  (check-sort-hof "Test 3 (Сортування списків за довжиною)"
                  '((1 2 3) (1) (1 2))
                  #'length
                  #'>
                  '((1) (1 2) (1 2 3)))
                  
  (check-sort-hof "Test 4 (Сортування за другим елементом підсписку)"
                  '((1 5) (2 1) (3 8))
                  #'cadr
                  #'>
                  '((2 1) (1 5) (3 8))))

(defun add-next-fn (&key (transform #'identity))
  "Повертає замикання, яке приймає підсписок (для maplist) 
  і створює точкову пару (поточний . наступний)."
  (lambda (sublist)
    (let ((current (funcall transform (car sublist)))
          (next (if (cdr sublist)
                    (funcall transform (cadr sublist))
                    nil)))
      (cons current next))))

(defun check-add-next (name transform-fn input expected)
  (format t "~:a: ~:[FAILED~;passed~]~%"
          name
          (equal (maplist (add-next-fn :transform transform-fn) input)
                 expected)))

(defun test-part-2 ()
  (check-add-next "Test 1 (Без transform)"
                  #'identity
                  '(1 2 3)
                  '((1 . 2) (2 . 3) (3 . NIL)))
  
  (check-add-next "Test 2 (З transform #'1+)"
                  #'1+
                  '(1 2 3)
                  '((2 . 3) (3 . 4) (4 . NIL)))
                  
  (check-add-next "Test 3 (Один елемент)"
                  #'identity
                  '(10)
                  '((10 . NIL)))

  (check-add-next "Test 4 (Порожній список)"
                  #'identity
                  '()
                  '()))

(format t "--- Тестування Частини 1 (Сортування) ---~%")
(test-part-1)
(format t "~%--- Тестування Частини 2 (Замикання) ---~%")
(test-part-2)