<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Булавчук Данило, КВ-23</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Реалізувати алгоритм сортування чисел у списку двома способами: функціонально (конструктивний підхід)
та імперативно (деструктивний підхід).

## Варіант 3
**Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням**.
(Бульбашкове сортування з оптимізацією).

---

## Лістинг функції з використанням конструктивного підходу
```lisp
(defun bubble-pass-recursive (lst)
  "Виконує ОДИН прохід 'бульбашки' функціонально (для ЛР3).
  Повертає два значення: (values новий-список прапорець-обміну)"
  (cond 
    ((null (cdr lst))
     (values lst nil))
    
    (t (multiple-value-bind (processed-tail swapped-in-tail-p)
         (bubble-pass-recursive (cdr lst))
       
       (let ((current-head (car lst))
             (new-head-of-tail (car processed-tail)))
         
         (if (> current-head new-head-of-tail)
             (values (cons new-head-of-tail (cons current-head (cdr processed-tail)))
                     t)

             (values (cons current-head processed-tail)
                     swapped-in-tail-p) 
             ))))))

(defun sort-functional (lst)
  "Реалізує бульбашкове сортування з прапорцем (для ЛР3)."
  (if (or (null lst) (null (cdr lst)))
      lst
      (multiple-value-bind (new-list swapped-p)
          (bubble-pass-recursive lst)
        
        (if swapped-p
            (sort-functional new-list)
            new-list)о
        )))
```

## Тестові набори та утиліти (конструктивний підхід)

```lisp
(defun check-sort-functional (name input expected)
  (format t "~:a: ~:[FAILED~;passed~]~%"
          name
          (equal (sort-functional input) expected)))

(defun test-functional-sort ()
  (check-sort-functional "Test 1 (Змішаний)"
                         '(5 1 4 2 8)
                         '(1 2 4 5 8))
  (check-sort-functional "Test 2 (Вже відсортований)"
                         '(1 2 3 4 5)
                         '(1 2 3 4 5))
  (check-sort-functional "Test 3 (Зворотний порядок)"
                         '(5 4 3 2 1)
                         '(1 2 3 4 5))
  (check-sort-functional "Test 4 (Порожній список)"
                         '()
                         '())
  (check-sort-functional "Test 5 (Один елемент)"
                         '(1)
                         '(1)))
```

## Тестування (конструктивний підхід)

```lisp
* (test-functional-sort)
Test 1 (Змішаний): passed
Test 2 (Вже відсортований): passed
Test 3 (Зворотний порядок): passed
Test 4 (Порожній список): passed
Test 5 (Один елемент): passed
NIL
```

## Лістинг функції з використанням деструктивного підходу

```lisp
(defun sort-imperative (lst)
  "Реалізує бульбашкове сортування з прапорцем (для ЛР3),
  використовуючи цикли та деструктивні операції НАД КОПІЄЮ списку."
  
  (let ((list-copy (copy-list lst)))
    
    (loop
      (let ((swapped-p nil))
        
        (do* ((current list-copy (cdr current)))
             ((null (cdr current)))
          
          (when (> (car current) (cadr current))
            (rotatef (car current) (cadr current))
            (setf swapped-p t)))
        
        (unless swapped-p
          (return list-copy))))))
```

## Тестові набори та утиліти (деструктивний підхід)

```lisp
(defun check-sort-imperative (name input expected)
  (let* ((original-input (copy-list input))
         (sorted-list (sort-imperative input)))
    
    (format t "~:a (результат): ~:[FAILED~;passed~]~%"
            name
            (equal sorted-list expected))
    
    (format t "~:a (збереження вхідних даних): ~:[FAILED (змінено!)~;passed~]~%"
            name
            (equal input original-input))))

(defun test-imperative-sort ()
  (check-sort-imperative "Test 1 (Змішаний)"
                         '(5 1 4 2 8)
                         '(1 2 4 5 8))
  (check-sort-imperative "Test 2 (Вже відсортований)"
                         '(1 2 3 4 5)
                         '(1 2 3 4 5))
  (check-sort-imperative "Test 3 (Зворотний порядок)"
                         '(5 4 3 2 1)
                         '(1 2 3 4 5))
  (check-sort-imperative "Test 4 (Порожній список)"
                         '()
                         '())
  (check-sort-imperative "Test 5 (Один елемент)"
                         '(1)
                         '(1)))
```

## Тестування (деструктивний підхід)

```lisp
* (test-imperative-sort)
Test 1 (Змішаний) (результат): passed
Test 1 (Змішаний) (збереження вхідних даних): passed
Test 2 (Вже відсортований) (результат): passed
Test 2 (Вже відсортований) (збереження вхідних даних): passed
Test 3 (Зворотний порядок) (результат): passed
Test 3 (Зворотний порядок) (збереження вхідних даних): passed
Test 4 (Порожній список) (результат): passed
Test 4 (Порожній список) (збереження вхідних даних): passed
Test 5 (Один елемент) (результат): passed
Test 5 (Один елемент) (збереження вхідних даних): passed
NIL