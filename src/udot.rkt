#lang racket

(provide parse-udot
         encode-udot
         process-asm-file
         convert-asm-file
         verify-with-objdump
         bytes->hex-string)

;; ============================================================
;; Module 1: Parser - 识别 UDOT 指令
;; ============================================================

;; 解析 UDOT vector 变体: udot Vd.Ta, Vn.Tb, Vm.Tb
;; 返回 (list 'udot rd rn rm) 或 #f
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
        ;; 验证: tb1 和 tb2 应该匹配，且与 ta 一致
        (if (and (string=? tb1 tb2)
                 (or (and (= q 0) (string=? tb1 "8b"))
                     (and (= q 1) (string=? tb1 "16b")))
                 (<= 0 rd 31)
                 (<= 0 rn 31)
                 (<= 0 rm 31))
            (list 'udot q rd rn rm leading-space)
            #f))
      #f))

;; 解析 UDOT indexed 变体: udot Vd.Ta, Vn.Tb, Vm.4B[index]
;; 返回 (list 'udot-indexed rd rn rm index) 或 #f
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
                 (<= 0 rd 31)
                 (<= 0 rn 31)
                 (<= 0 rm 31)
                 (<= 0 index 3))
            (list 'udot-indexed q rd rn rm index leading-space)
            #f))
      #f))

;; 主解析函数
(define (parse-udot line)
  (or (parse-udot-vector line)
      (parse-udot-indexed line)))

;; ============================================================
;; Module 2: Encoder - 将指令编码为机器码
;; ============================================================

;; UDOT (vector) 编码:
;; 31 30 29-24  23-22 21 20-16 15-10  9-5  4-0
;; 0  Q  101110 size  0  Rm    100101 Rn   Rd
;; size = 10 for UDOT

;; UDOT (indexed) 编码:
;; 31 30 29-24  23-22 21-16     15-12 11 10 9-5  4-0
;; 0  Q  001111 size  L:M:Rm    1110  H  0  Rn   Rd
;; 其中 size=10, L:H 构成 index

(define (encode-udot-vector q rd rn rm)
  (bitwise-ior
   (arithmetic-shift q 30)             ; Q at bit 30
   (arithmetic-shift #b101110 24)      ; fixed bits 29-24
   (arithmetic-shift #b10 22)          ; size at bits 23-22
   (arithmetic-shift 0 21)             ; 0 at bit 21
   (arithmetic-shift rm 16)            ; Rm at bits 20-16
   (arithmetic-shift #b100101 10)      ; fixed bits 15-10
   (arithmetic-shift rn 5)             ; Rn at bits 9-5
   rd))                                ; Rd at bits 4-0

(define (encode-udot-indexed q rd rn rm index)
  ;; UDOT (by element) 编码:
  ;; 31 30 29-24  23-22 21 20   19-16   15-12 11 10 9-5  4-0
  ;; 0  Q  101111 size  L  M    Rm[3:0] 1110  H  0  Rn   Rd
  ;; size=10, index = L:H (L=index[0], H=index[1])
  ;; M = Rm[4], Rm[3:0] at bits 19-16
  (let* ([L (bitwise-and index 1)]
         [H (arithmetic-shift (bitwise-and index 2) -1)]
         [M (arithmetic-shift (bitwise-and rm #b10000) -4)]
         [Rm-low (bitwise-and rm #b01111)])
    (bitwise-ior
     (arithmetic-shift q 30)           ; Q at bit 30
     (arithmetic-shift #b101111 24)    ; fixed bits 29-24
     (arithmetic-shift #b10 22)        ; size at bits 23-22
     (arithmetic-shift L 21)           ; L at bit 21
     (arithmetic-shift M 20)           ; M (Rm[4]) at bit 20
     (arithmetic-shift Rm-low 16)      ; Rm[3:0] at bits 19-16
     (arithmetic-shift #b1110 12)      ; fixed bits 15-12
     (arithmetic-shift H 11)           ; H at bit 11
     (arithmetic-shift 0 10)           ; 0 at bit 10
     (arithmetic-shift rn 5)           ; Rn at bits 9-5
     rd)))                             ; Rd at bits 4-0

;; 将 32 位整数转换为字节序列
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

;; 主编码函数
;; 参数: parsed-instr - 解析后的指令
;;       big-endian? - #t 为大端序, #f 为小端序
;; 返回: bytes 对象
(define (encode-udot parsed-instr big-endian?)
  (match parsed-instr
    [(list 'udot q rd rn rm _)
     (uint32->bytes (encode-udot-vector q rd rn rm) big-endian?)]
    [(list 'udot-indexed q rd rn rm index _)
     (uint32->bytes (encode-udot-indexed q rd rn rm index) big-endian?)]
    [_ (error 'encode-udot "Invalid parsed instruction: ~a" parsed-instr)]))

;; 辅助函数：将 bytes 转换为十六进制字符串
(define (bytes->hex-string bs)
  (string-append "0x"
                 (string-join
                  (for/list ([b (in-bytes bs)])
                    (let ([s (number->string b 16)])
                      (if (< (string-length s) 2)
                          (string-append "0" s)
                          s)))
                  "")))

;; ============================================================
;; Module 3: Pipeline - 处理汇编文件
;; ============================================================

;; 格式化编码输出（使用 .word 伪指令）
;; 注意: .word 伪指令会将值以小端序写入，所以我们需要传入大端序值
(define (format-encoded-instruction encoded leading-space original-line)
  ;; encoded 是小端序 bytes, 需要转换为大端序整数给 .word
  (define be-value (bytes->uint32-be encoded))
  (string-append leading-space
                 ".word 0x"
                 (string-downcase (~r be-value #:base 16 #:min-width 8 #:pad-string "0"))
                 "  // "
                 (string-trim original-line)))

;; 处理单行
(define (process-line line big-endian?)
  (define parsed (parse-udot line))
  (if parsed
      (let* ([leading-space (last parsed)]
             [encoded (encode-udot parsed big-endian?)])
        (format-encoded-instruction encoded leading-space line))
      line))

;; 处理汇编文件
;; 参数: input-path  - 输入文件路径
;;       output-path - 输出文件路径
;;       big-endian? - #t 为大端序, #f 为小端序 (默认 #f, ARM 通常是小端序)
(define (process-asm-file input-path output-path [big-endian? #f])
  (define lines (file->lines input-path))
  (define processed-lines
    (for/list ([line (in-list lines)])
      (process-line line big-endian?)))
  (display-lines-to-file processed-lines output-path #:exists 'replace)
  (printf "Processed ~a lines, output written to ~a~n"
          (length lines) output-path))

;; ============================================================
;; Module 4: 文件转换封装函数
;; ============================================================

;; 简化的文件转换函数
;; 参数: input-path  - 输入汇编文件路径
;;       output-path - 输出文件路径 (可选，默认在原文件名后加 .converted)
;;       big-endian? - 是否使用大端序 (可选，默认 #f 小端序)
;; 返回: 转换的指令数量
(define (convert-asm-file input-path
                          [output-path #f]
                          [big-endian? #f])
  (define actual-output
    (or output-path
        (string-append (path->string (path-replace-extension input-path ""))
                       ".converted.s")))
  (define lines (file->lines input-path))
  (define converted-count 0)
  (define processed-lines
    (for/list ([line (in-list lines)])
      (define parsed (parse-udot line))
      (if parsed
          (begin
            (set! converted-count (add1 converted-count))
            (let* ([leading-space (last parsed)]
                   [encoded (encode-udot parsed big-endian?)])
              (format-encoded-instruction encoded leading-space line)))
          line)))
  (display-lines-to-file processed-lines actual-output #:exists 'replace)
  (printf "转换完成: ~a → ~a~n" input-path actual-output)
  (printf "共转换 ~a 条 UDOT 指令~n" converted-count)
  converted-count)

;; ============================================================
;; Module 5: objdump 验证
;; ============================================================

;; 从 objdump 输出解析编码
(define (parse-objdump-line line)
  ;; 格式: "       0: 2e8394a0     	udot.2s	v0, v5, v3"
  (define pattern #px"^\\s*[0-9a-f]+:\\s+([0-9a-f]+)\\s+udot")
  (define match (regexp-match pattern line))
  (if match
      (string-upcase (list-ref match 1))
      #f))

;; 使用 objdump 验证编码正确性
;; 参数: asm-path - 汇编文件路径
;;       build-dir - 编译输出目录 (可选，默认 "../build")
;; 返回: (list passed-count failed-count failures)
(define (verify-with-objdump asm-path [build-dir "../build"])
  (printf "~n=== 使用 objdump 验证编码正确性 ===~n~n")

  ;; 确保 build 目录存在
  (unless (directory-exists? build-dir)
    (make-directory build-dir))

  ;; 1. 用系统汇编器编译
  (define base-name (path->string (file-name-from-path (path-replace-extension asm-path ""))))
  (define obj-path (build-path build-dir (string-append base-name ".o")))
  (define compile-result
    (with-output-to-string
      (lambda ()
        (system (format "clang -c ~a -o ~a 2>&1" asm-path obj-path)))))

  (when (not (file-exists? obj-path))
    (error 'verify-with-objdump "汇编失败: ~a" compile-result))

  ;; 2. 用 objdump 获取实际编码
  (define objdump-output
    (with-output-to-string
      (lambda ()
        (system (format "objdump -d ~a" obj-path)))))

  (define objdump-lines (string-split objdump-output "\n"))
  (define actual-encodings
    (filter-map parse-objdump-line objdump-lines))

  ;; 3. 解析原始汇编文件，计算我们的编码
  (define asm-lines (file->lines asm-path))
  (define our-results
    (filter-map
     (lambda (line)
       (define parsed (parse-udot line))
       (if parsed
           (let* ([encoded (encode-udot parsed #f)]  ; 小端序
                  [be-value (bytes->uint32-be encoded)])
             (list line (format "~X" be-value) parsed))
           #f))
     asm-lines))

  ;; 4. 对比
  (define passed 0)
  (define failed 0)
  (define failures '())

  (for ([our our-results]
        [actual actual-encodings])
    (define line (first our))
    (define our-hex (second our))
    (define match? (string-ci=? our-hex actual))
    (if match?
        (begin
          (set! passed (add1 passed))
          (printf "✓ ~a~n  我们: 0x~a  实际: 0x~a~n"
                  (string-trim line) our-hex actual))
        (begin
          (set! failed (add1 failed))
          (set! failures (cons (list line our-hex actual) failures))
          (printf "✗ ~a~n  我们: 0x~a  实际: 0x~a~n"
                  (string-trim line) our-hex actual))))

  (printf "~n=== 验证结果 ===~n")
  (printf "通过: ~a  失败: ~a~n" passed failed)

  (list passed failed (reverse failures)))

;; 辅助：小端 bytes 转大端 uint32 用于显示
(define (bytes->uint32-be bs)
  (bitwise-ior
   (arithmetic-shift (bytes-ref bs 3) 24)
   (arithmetic-shift (bytes-ref bs 2) 16)
   (arithmetic-shift (bytes-ref bs 1) 8)
   (bytes-ref bs 0)))

;; ============================================================
;; 测试/演示
;; ============================================================

(module+ main
  (printf "=== UDOT Instruction Parser & Encoder ===~n~n")

  ;; 测试解析和编码
  (define test-cases
    '("udot v0.2s, v5.8b, v3.8b"
      "  udot v1.4s, v2.16b, v3.16b"
      "    UDOT V10.2S, V20.8B, V30.8B  ; comment"
      "udot v0.2s, v1.8b, v2.4b[0]"
      "udot v0.2s, v1.8b, v2.4b[1]"
      "udot v0.2s, v1.8b, v2.4b[2]"
      "udot v0.2s, v1.8b, v2.4b[3]"
      "mov x0, #1"
      "invalid instruction"))

  (for ([tc (in-list test-cases)])
    (define result (parse-udot tc))
    (printf "Input:  ~s~n" tc)
    (printf "Parsed: ~a~n" result)
    (when result
      (printf "Encoded (LE): ~a~n" (bytes->hex-string (encode-udot result #f)))
      (printf "Encoded (BE): ~a~n" (bytes->hex-string (encode-udot result #t))))
    (printf "~n"))

  ;; 使用 objdump 验证
  (define test-asm "../test/verify.s")
  (when (file-exists? test-asm)
    (verify-with-objdump test-asm)))
