(defun lt (lhs rhs) (prim-lt lhs rhs))
(defun gt (lhs rhs) (lt rhs lhs))

(defun append (head tail)
  (if head
    (cons (car head)
          (append (cdr head) tail))
    tail))

(defun last (lst)
  (if (cdr lst)
    (last (cdr lst))
    (car lst)))

(defmacro eval-list (lst)
  (if lst
    (list 'cons (car lst) (list 'eval-list (cdr lst)))))

(defmacro progn (lst)
  (list 'last (list 'eval-list lst)))

(defun caar (lst)
  (car (car lst)))

(defun cadr (lst)
  (car (cdr lst)))

(defun cddr (lst)
  (cdr (cdr lst)))

(defun cadar (lst)
  (car (cdr (car lst))))

(defun define-all (vars)
  (if vars
    (cons (list 'define
                (caar vars)
                (cadar vars))
          (define-all (cdr vars)))))

(defmacro let (vars exprs)
  (list 'progn
        (append (define-all vars)
                exprs)))


(defun order (a b)
  (if (lt a b)
    (cons a b)
    (cons b a)))

(defun min (lst)
  (if lst
    (if (cdr lst)
      (car (order (car lst) (min (cdr lst))))
      (car lst))))

(defun max (lst)
  (if lst
    (if (cdr lst)
      (cdr (order (car lst) (max (cdr lst))))
      (car lst))))

(defun minmax (lst)
  (if lst
    (if (cdr lst)
      (let ((c (order (car lst) (cadr lst)))
            (r (if (cddr lst) (minmax (cddr lst)) c)))
        ((cons (car (order (car c) (car r)))
               (cdr (order (cdr c) (cdr r))))))
      (cons (car lst) (car lst)))))

(defun kondo3 (lst) (min lst))
(defun kondo4 (lst) (max lst))
(defun kondo5 (lst) (minmax lst))


(define large-seq
  '(121 202 257 220 216 285  88 233 107 141
    102 244  13 115  99 116 112  53 198  52
    279  37 123 240   1  20 289  15   5  14
    259 137  60 213  77  17 218  56   0  49
    282 165 145 277 212 197 211  38 167 256
    227 132 177 189 251  16 207   9 284 247
    142 221 232  22  42   7  46  50 187 184
     45 275 104 203 264 138 206 205 172 126
    236  57 239 234  68  69 209 229 258  44
    196  10 100 287  19 139 226 290 230 254
    179  87 208 117 237 182  36 242 180 152
    120 162  27  98 193 130 113 181 295  25
     97  34 166  41 224 134 228 161  58 194
     70 143  91 155  11  23  66  78 133 281
    171 222 151 118 101 280 157 214 148 168
    140  18 191 204  55  39 149   4 225 156
    129  93 185 250 267  65 170  26 111  79
     81 105 119 273 175 268 199   8 146  83
     32 188  33 150 159 153   2 238  40 243
    219 246  86 274  54  51  21 106 249 266
    114  28 201 255 288 109 276 131 178 200
    186  63 124 262  75 128  71 272  35  82
     73 253 297  89 190 231 158 125 245 210
     74 269 195  48 235   6  67   3  64 293
    261 296 294  62 135 271 144 263 270 291
     61 283 241 248 286 252 122 260  31  94
     80  92  12 183 127 299  84  96 164  29
    169 223  85 103 298  72  24  59 215 176
    163 292 136 154  30  47  76  90  43 174
     95 108 173 110 278 192 217 265 160 147))

(defun times (seq n)
  (if (gt n 0)
    (append seq (times seq (+ n (negate 1))))))

(define huge-seq (times large-seq 3))

(println (list 'kondo3-empty (kondo3 ())))
(println (list 'kondo4-empty (kondo4 ())))
(println (list 'kondo5-empty (kondo5 ())))

(println (list 'kondo3-tiny (kondo3 '(3 5 2 1 4))))
(println (list 'kondo4-tiny (kondo4 '(3 5 2 1 4))))
(println (list 'kondo5-tiny (kondo5 '(3 5 2 1 4))))

(println (list 'kondo3-small (kondo3 '(3 5 2 1 4 6))))
(println (list 'kondo4-small (kondo4 '(3 5 2 1 4 6))))
(println (list 'kondo5-small (kondo5 '(3 5 2 1 4 6))))

(println (list 'kondo3-large (kondo3 large-seq)))
(println (list 'kondo4-large (kondo4 large-seq)))
(println (list 'kondo5-large (kondo5 large-seq)))

(println (list 'kondo3-huge (kondo3 huge-seq)))
(println (list 'kondo4-huge (kondo4 huge-seq)))
(println (list 'kondo5-huge (kondo5 huge-seq)))

(exit)
