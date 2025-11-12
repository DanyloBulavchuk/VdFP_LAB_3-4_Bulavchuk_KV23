<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Булавчук Данило, КВ-23</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
1.  Модифікувати функціональну реалізацію алгоритму сортування з ЛР №3, додавши ключові параметри `:key` та `:test`.
2.  Реалізувати функцію, що створює замикання, згідно з варіантом.

---

## Варіант першої частини
Алгоритм сортування обміном №2 (із використанням прапорця), модифікований для підтримки `:key` та `:test`.

## Лістинг реалізації першої частини завдання
```lisp
(defun bubble-pass-recursive-hof (lst key test)
  "Виконує один прохід бульбашками функціонально та приймає key та test."
  (cond 
    ((null (cdr lst)) ; список з 1 елемента
     (values lst nil)) ; повертаємо список і (nil)
    
    (t ; Рекурсивний випадок
     (multiple-value-bind (processed-tail swapped-in-tail-p)
         (bubble-pass-recursive-hof (cdr lst) key test)
       
       (let* ((current-head (car lst))
              (new-head-of-tail (car processed-tail))
              (key-current (funcall key current-head))
              (key-new-head (funcall key new-head-of-tail)))
         
         (if (funcall test key-current key-new-head)
             ;; випадок обміну
             (values (cons new-head-of-tail (cons current-head (cdr processed-tail)))
                     t)
             
             ;; випадок без обміну
             (values (cons current-head processed-tail)
                     swapped-in-tail-p) 
             ))))))

(defun sort-functional-hof (lst &key (key #'identity) (test #'>))
  "Реалізує бульбашкове сортування (Варіант 3) з параметрами :key та :test."
  (if (or (null lst) (null (cdr lst)))
      lst
      (multiple-value-bind (new-list swapped-p)
          (bubble-pass-recursive-hof lst key test)
        
        (if swapped-p
            (sort-functional-hof new-list :key key :test test) ; якщо обміни були, запускаємо ще один прохід
            new-list) ; Якщо обмінів не було - список відсортовано
        )))
```

## Тестові набори та утиліти першої частини

```lisp

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
```

## Тестування першої частини

```lisp
* (test-part-1)
Test 1 (Стандартне сортування): passed
Test 2 (Сортування за спаданням): passed
Test 3 (Сортування списків за довжиною): passed
Test 4 (Сортування за другим елементом підсписку): passed
NIL
```

## Варіант другої частини
## Лістинг реалізації другої частини завдання

```lisp
(defun add-next-fn (&key (transform #'identity))
  "Повертає замикання, яке приймає підсписок (для maplist)"
  (lambda (sublist)
    (let ((current (funcall transform (car sublist)))
          (next (if (cdr sublist)
                    (funcall transform (cadr sublist))
                    nil)))
      (cons current next))))
```

## Тестові набори та утиліти другої частини

```lisp
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
```

## Тестування другої частини

```lisp
* (test-part-2)
Test 1 (Без transform): passed
Test 2 (З transform #'1+): passed
Test 3 (Один елемент): passed
Test 4 (Порожній список): passed
NIL
```