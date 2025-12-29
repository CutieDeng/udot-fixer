#lang racket

(provide
 ;; 解析
 parse-udot
 ;; 编码
 encode-udot
 encode-to-word        ; 直接返回 .word 格式字符串
 ;; 单文件操作
 convert-file
 verify-file
 ;; 批量操作
 convert-directory
 verify-directory
 ;; 完整流程
 run-all
 ;; 辅助
 bytes->hex-string)

;; ============================================================
;; 配置
;; ============================================================

(define DEFAULT-TEST-DIR "../test")
(define DEFAULT-OUTPUT-DIR "../test-convert")
(define DEFAULT-BUILD-DIR "../build")

;; ============================================================
;; Module 1: Parser - 识别 UDOT 指令
;; ============================================================

(define (parse-udot-vector line)
  (define pattern
    #px"^(\\s*)[uU][dD][oO][tT]\\s+[vV](\\d+)\\.(2[sS]|4[sS])\\s*,\\s*[vV](\\d+)\\.(8[bB]|16[bB])\\s*,\\s*[vV](\\d+)\\.(8[bB]|16[bB])\\s*(;.*)?$")
  (define match (regexp-match pattern line))
  (if match
      (let* ([leading-space (list-ref match 1)]
             [rd (string->number (list-ref match 2))]
             [ta (string-downcase (list-ref match 3))]
             [rn (string->number (list-ref match 4))]
             [tb1 (string-downcase (list-ref match 5))]
             [rm (string->number (list-ref match 6))]
             [tb2 (string-downcase (list-ref match 7))]
             [q (if (string=? ta "2s") 0 1)])
        (if (and (string=? tb1 tb2)
                 (or (and (= q 0) (string=? tb1 "8b"))
                     (and (= q 1) (string=? tb1 "16b")))
                 (<= 0 rd 31) (<= 0 rn 31) (<= 0 rm 31))
            (list 'udot q rd rn rm leading-space)
            #f))
      #f))

(define (parse-udot-indexed line)
  (define pattern
    #px"^(\\s*)[uU][dD][oO][tT]\\s+[vV](\\d+)\\.(2[sS]|4[sS])\\s*,\\s*[vV](\\d+)\\.(8[bB]|16[bB])\\s*,\\s*[vV](\\d+)\\.4[bB]\\[(\\d+)\\]\\s*(;.*)?$")
  (define match (regexp-match pattern line))
  (if match
      (let* ([leading-space (list-ref match 1)]
             [rd (string->number (list-ref match 2))]
             [ta (string-downcase (list-ref match 3))]
             [rn (string->number (list-ref match 4))]
             [tb (string-downcase (list-ref match 5))]
             [rm (string->number (list-ref match 6))]
             [index (string->number (list-ref match 7))]
             [q (if (string=? ta "2s") 0 1)])
        (if (and (or (and (= q 0) (string=? tb "8b"))
                     (and (= q 1) (string=? tb "16b")))
                 (<= 0 rd 31) (<= 0 rn 31) (<= 0 rm 31)
                 (<= 0 index 3))
            (list 'udot-indexed q rd rn rm index leading-space)
            #f))
      #f))

;; 解析 UDOT 指令
;; 返回: (list type q rd rn rm [index] leading-space) 或 #f
(define (parse-udot line)
  (or (parse-udot-vector line)
      (parse-udot-indexed line)))

;; ============================================================
;; Module 2: Encoder - 编码
;; ============================================================

(define (encode-udot-vector q rd rn rm)
  (bitwise-ior
   (arithmetic-shift q 30)
   (arithmetic-shift #b101110 24)
   (arithmetic-shift #b10 22)
   (arithmetic-shift rm 16)
   (arithmetic-shift #b100101 10)
   (arithmetic-shift rn 5)
   rd))

(define (encode-udot-indexed q rd rn rm index)
  (let* ([L (bitwise-and index 1)]
         [H (arithmetic-shift (bitwise-and index 2) -1)]
         [M (arithmetic-shift (bitwise-and rm #b10000) -4)]
         [Rm-low (bitwise-and rm #b01111)])
    (bitwise-ior
     (arithmetic-shift q 30)
     (arithmetic-shift #b101111 24)
     (arithmetic-shift #b10 22)
     (arithmetic-shift L 21)
     (arithmetic-shift M 20)
     (arithmetic-shift Rm-low 16)
     (arithmetic-shift #b1110 12)
     (arithmetic-shift H 11)
     (arithmetic-shift rn 5)
     rd)))

(define (uint32->bytes value big-endian?)
  (if big-endian?
      (bytes (bitwise-and (arithmetic-shift value -24) #xFF)
             (bitwise-and (arithmetic-shift value -16) #xFF)
             (bitwise-and (arithmetic-shift value -8) #xFF)
             (bitwise-and value #xFF))
      (bytes (bitwise-and value #xFF)
             (bitwise-and (arithmetic-shift value -8) #xFF)
             (bitwise-and (arithmetic-shift value -16) #xFF)
             (bitwise-and (arithmetic-shift value -24) #xFF))))

(define (bytes->uint32-be bs)
  (bitwise-ior
   (arithmetic-shift (bytes-ref bs 3) 24)
   (arithmetic-shift (bytes-ref bs 2) 16)
   (arithmetic-shift (bytes-ref bs 1) 8)
   (bytes-ref bs 0)))

;; 编码指令为 bytes
(define (encode-udot parsed [big-endian? #f])
  (match parsed
    [(list 'udot q rd rn rm _)
     (uint32->bytes (encode-udot-vector q rd rn rm) big-endian?)]
    [(list 'udot-indexed q rd rn rm index _)
     (uint32->bytes (encode-udot-indexed q rd rn rm index) big-endian?)]
    [_ (error 'encode-udot "Invalid: ~a" parsed)]))

;; 编码为 .word 格式字符串
(define (encode-to-word instr-string)
  (define parsed (parse-udot instr-string))
  (if parsed
      (let* ([encoded (encode-udot parsed #f)]
             [be-value (bytes->uint32-be encoded)]
             [leading (last parsed)])
        (format "~a.word 0x~a  // ~a"
                leading
                (string-downcase (~r be-value #:base 16 #:min-width 8 #:pad-string "0"))
                (string-trim instr-string)))
      #f))

(define (bytes->hex-string bs)
  (string-append "0x"
                 (string-join
                  (for/list ([b (in-bytes bs)])
                    (~r b #:base 16 #:min-width 2 #:pad-string "0"))
                  "")))

;; ============================================================
;; Module 3: 单文件操作
;; ============================================================

;; 转换单个文件
;; 返回: 转换的指令数量
(define (convert-file input-path output-path)
  (define lines (file->lines input-path))
  (define count 0)
  (define output-lines
    (for/list ([line lines])
      (define parsed (parse-udot line))
      (if parsed
          (begin
            (set! count (add1 count))
            (let* ([encoded (encode-udot parsed #f)]
                   [be-value (bytes->uint32-be encoded)]
                   [leading (last parsed)])
              (format "~a.word 0x~a  // ~a"
                      leading
                      (string-downcase (~r be-value #:base 16 #:min-width 8 #:pad-string "0"))
                      (string-trim line))))
          line)))
  (make-parent-directory* output-path)
  (display-lines-to-file output-lines output-path #:exists 'replace)
  count)

;; 验证单个文件 (与 objdump 对比)
;; 返回: (list passed failed details)
(define (verify-file asm-path [build-dir DEFAULT-BUILD-DIR])
  (make-directory* build-dir)

  (define base-name (path->string (file-name-from-path (path-replace-extension asm-path ""))))
  (define obj-path (path->string (build-path build-dir (string-append base-name ".o"))))

  ;; 编译
  (define compile-ok?
    (system (format "clang -c ~a -o ~a 2>/dev/null" asm-path obj-path)))

  (unless compile-ok?
    (error 'verify-file "汇编失败: ~a" asm-path))

  ;; 获取 objdump 编码
  (define objdump-output
    (with-output-to-string
      (lambda () (system (format "objdump -d ~a" obj-path)))))

  (define actual-encodings
    (filter-map
     (lambda (line)
       (define m (regexp-match #px"^\\s*[0-9a-f]+:\\s+([0-9a-f]+)\\s+udot" line))
       (and m (string-upcase (cadr m))))
     (string-split objdump-output "\n")))

  ;; 计算我们的编码
  (define our-encodings
    (filter-map
     (lambda (line)
       (define parsed (parse-udot line))
       (and parsed
            (let* ([encoded (encode-udot parsed #f)]
                   [be-value (bytes->uint32-be encoded)])
              (cons (string-trim line) (format "~X" be-value)))))
     (file->lines asm-path)))

  ;; 对比
  (define results
    (for/list ([our our-encodings]
               [actual actual-encodings])
      (define line (car our))
      (define our-hex (cdr our))
      (define ok? (string-ci=? our-hex actual))
      (list ok? line our-hex actual)))

  (define passed (count (lambda (r) (first r)) results))
  (define failed (- (length results) passed))

  (list passed failed results))

;; ============================================================
;; Module 4: 批量操作
;; ============================================================

;; 转换目录下所有 .s 文件
(define (convert-directory [input-dir DEFAULT-TEST-DIR]
                           [output-dir DEFAULT-OUTPUT-DIR])
  (make-directory* output-dir)

  (define files (directory-list input-dir))
  (define asm-files (filter (lambda (f) (regexp-match? #rx"\\.s$" (path->string f))) files))

  (printf "~n=== 批量转换 ===~n")
  (printf "输入目录: ~a~n" input-dir)
  (printf "输出目录: ~a~n~n" output-dir)

  (define total-count 0)
  (define file-results '())

  (for ([f asm-files])
    (define input-path (build-path input-dir f))
    (define output-path (build-path output-dir f))
    (define count (convert-file (path->string input-path) (path->string output-path)))
    (set! total-count (+ total-count count))
    (set! file-results (cons (list f count) file-results))
    (printf "  ~a: ~a 条指令~n" f count))

  (printf "~n共转换 ~a 个文件, ~a 条指令~n" (length asm-files) total-count)
  (list (length asm-files) total-count (reverse file-results)))

;; 验证目录下所有 .s 文件
(define (verify-directory [input-dir DEFAULT-TEST-DIR]
                          [build-dir DEFAULT-BUILD-DIR])
  (printf "~n=== 批量验证 (objdump) ===~n")
  (printf "测试目录: ~a~n~n" input-dir)

  (define files (directory-list input-dir))
  (define asm-files (filter (lambda (f) (regexp-match? #rx"\\.s$" (path->string f))) files))

  (define total-passed 0)
  (define total-failed 0)

  (for ([f asm-files])
    (define path (path->string (build-path input-dir f)))
    (printf "~a:~n" f)
    (define result (verify-file path build-dir))
    (define passed (first result))
    (define failed (second result))
    (define details (third result))

    (for ([d details])
      (if (first d)
          (printf "  ✓ ~a~n" (second d))
          (printf "  ✗ ~a (我们: ~a, 实际: ~a)~n" (second d) (third d) (fourth d))))

    (set! total-passed (+ total-passed passed))
    (set! total-failed (+ total-failed failed)))

  (printf "~n=== 验证结果 ===~n")
  (printf "通过: ~a  失败: ~a~n" total-passed total-failed)

  (list total-passed total-failed))

;; ============================================================
;; Module 5: 完整流程
;; ============================================================

;; 执行完整流程: 转换 + 验证
(define (run-all [test-dir DEFAULT-TEST-DIR]
                 [output-dir DEFAULT-OUTPUT-DIR]
                 [build-dir DEFAULT-BUILD-DIR])
  (printf "========================================~n")
  (printf "  UDOT 指令转换器~n")
  (printf "========================================~n")

  ;; 1. 转换
  (define convert-result (convert-directory test-dir output-dir))

  ;; 2. 验证原始文件编码正确性
  (define verify-result (verify-directory test-dir build-dir))

  ;; 3. 验证转换后文件与原始文件二进制一致
  (printf "~n=== 二进制对比 ===~n")
  (make-directory* build-dir)

  (define files (directory-list test-dir))
  (define asm-files (filter (lambda (f) (regexp-match? #rx"\\.s$" (path->string f))) files))

  (define all-match? #t)
  (for ([f asm-files])
    (define orig-path (path->string (build-path test-dir f)))
    (define conv-path (path->string (build-path output-dir f)))
    (define base (path->string (path-replace-extension f "")))
    (define orig-obj (path->string (build-path build-dir (string-append base "_orig.o"))))
    (define conv-obj (path->string (build-path build-dir (string-append base "_conv.o"))))

    (system (format "clang -c ~a -o ~a 2>/dev/null" orig-path orig-obj))
    (system (format "clang -c ~a -o ~a 2>/dev/null" conv-path conv-obj))

    (define match?
      (equal? (file->bytes orig-obj) (file->bytes conv-obj)))

    (unless match? (set! all-match? #f))
    (printf "  ~a: ~a~n" f (if match? "✓ 一致" "✗ 不一致")))

  (printf "~n========================================~n")
  (printf "  总结: ~a~n"
          (if (and (= (second verify-result) 0) all-match?)
              "全部通过 ✓"
              "存在失败 ✗"))
  (printf "========================================~n")

  (list convert-result verify-result all-match?))

;; 直接执行
(run-all)
