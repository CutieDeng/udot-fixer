    .text
    .global main

main:
    mov x0, #0
    .word 0x2e8394a0  // udot v0.2s, v5.8b, v3.8b
    .word 0x6e839441  // udot v1.4s, v2.16b, v3.16b
        .word 0x2e9e968a  // UDOT V10.2S, V20.8B, V30.8B  ; with comment
    add x1, x0, #1
    .word 0x2fa2e020  // udot v0.2s, v1.8b, v2.4b[1]
    ret
