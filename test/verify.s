    .text
    .global _main
    .align 4

_main:
    udot v0.2s, v5.8b, v3.8b
    udot v1.4s, v2.16b, v3.16b
    udot v10.2s, v20.8b, v30.8b
    udot v31.4s, v31.16b, v31.16b
    udot v0.2s, v1.8b, v2.4b[0]
    udot v0.2s, v1.8b, v2.4b[1]
    udot v0.2s, v1.8b, v2.4b[2]
    udot v0.2s, v1.8b, v2.4b[3]
    ret
