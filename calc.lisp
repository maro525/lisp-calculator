;;; special variable
(defvar *ch*)
(defvar *token*)
(defvar *value*)

(defun nextch ()
  (setq *ch* (read-char)))

(defun getch () *ch*)

;;; 数値用の文字か
(defun number-char-p (c)
  (or (digit-char-p c)
      (find c '(#\. #\+ #\- #\d #\D))))

;;; 数値を求める
(defun get-number ()
  (let (buff)
    (loop while (number-char-p (getch)) do (push (getch) buff) (nextch))
    (setq buff (concatenate 'string (reverse buff)))
    (multiple-value-bind
      (num len)
      (read-from-string buff)
      (if (and (numberp num) (= (length buff) len))
          num
        (error "invalid number ~a~%" buff)))))


;;; 空白文字の判定
(defun white-space-p (c)
  (or (char= c #\ )
      (not (graphic-char-p c))))

;;; tokenの切り分け
(defun get-token ()
  (loop while (white-space-p (getch)) do (nextch))
  (cond
    ((digit-char-p (getch))
     (setq *token* 'number
           *value* (get-number)))
    (t
      (case (getch)
        ((#\+)
         (setq *token* '+)
         (nextch))
        ((#\-)
         (setq *token* '-)
         (nextch))
        ((#\*)
         (setq *token* '*)
         (nextch))
        ((#\/)
         (setq *token* '/)
         (nextch))
        ((#\()
         (setq *token* 'lpar)
         (nextch))
        ((#\))
         (setq *token* 'rpar)
         (nextch))
        ((#\;)
         (setq *token* 'semic)
         (nextch))
        (t
          (setq *token* 'others))))))



;;; 構文解析
(declaim (ftype (function () t) expression))

;;; 因子
(defun factor ()
  (case *token*
    (lpar
      (get-token)
      (let ((val (expression)))
        (if (eq *token* 'rpar)
            (get-token)
          (error "')' expeted"))
        val))
    (number
      (prog1 *value* (get-token)))
    (t
      (error "unexpected token ~a" *token*))))

;;; 項
(defun term (&aux (val (factor)))
  (loop
    (case
      *token*
      (*
        (get-token)
        (setq val (* val (factor))))
      (/
        (get-token)
        (setq val (/ val (factor))))
      (t
        (return val)))))

;;; 式
(defun expression (&aux (val (term)))
  (loop
    (case
      *token*
      (+
        (get-token)
        (setq val (+ val (term))))
      (-
        (get-token)
        (setq val (- val (term))))
      (t
        (return val)))))

;;; propmpt output
(defun prompt ()
  (format t "Calc> ")
  (force-output))

;;; 入力された式を評価する
(defun toplevel ()
  (let ((val (expression)))
    (cond
      ((eq *token* 'semic)
       (format t "=> ~a~%" val)
       (prompt))
      (t
        (error "invalid token ~a" *token*)))))

;;; 入力をクリアする
(defun clear-input-data ()
  (loop while (not (eql *ch* #\Newline)) do (nextch))
  (prompt))

;;; 電卓の実行
(defun calc ()
  (prompt)
  (nextch)
  (loop
    (handler-case
         (progn (get-token) (toplevel))
      ((or simple-error arithmetic-error) (c)
         (format t "ERROR: ~a~%" c)
         (clear-input-data)))))


