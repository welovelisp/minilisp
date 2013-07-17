(define random-seq
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


(defun not (x) (if x () t))
(defun and (lhs rhs) (if lhs (if rhs t)))
(defun or (lhs rhs) (if lhs t (if rhs t)))

(defun lt (lhs rhs) (prim-lt lhs rhs))
(defun gt (lhs rhs) (lt rhs lhs))

(defun eq (lhs rhs) (and (not (lt lhs rhs)) (not (gt lhs rhs))))
(defun ne (lhs rhs) (not (eq lhs rhs)))

(defun le (lhs rhs) (or (lt lhs rhs) (eq lhs rhs)))
(defun ge (lhs rhs) (or (gt lhs rhs) (eq lhs rhs)))


(defun select (n lst comp)
  (if lst
    (if (comp (car lst) n)
      (cons (car lst)
            (select n (cdr lst) comp))
      (select n (cdr lst) comp))))

(defun append (head tail)
  (if head
    (cons (car head)
          (append (cdr head) tail))
    tail))

(defun append3 (head middle tail)
  (append (append head middle)
          tail))

(defun qsort (lst lessp)
  (if lst
    (append3
      (qsort (select (car lst) (cdr lst) lessp) lessp)
      (list (car lst))
      (qsort (select (car lst) (cdr lst)
                     (lambda (lhs rhs) (not (lessp lhs rhs))))
             lessp))))


(defun times (seq n)
  (if (not (eq n 0))
    (append seq (times seq (+ n (negate 1))))))

(println (qsort (times random-seq 3) lt))
(exit)
