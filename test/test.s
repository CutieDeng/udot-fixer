    .text
    .global main

main:
    mov x0, #0
    udot v0.2s, v5.8b, v3.8b
    udot v1.4s, v2.16b, v3.16b
        UDOT V10.2S, V20.8B, V30.8B  ; with comment
    add x1, x0, #1
    udot v0.2s, v1.8b, v2.4b[1]
    ret
