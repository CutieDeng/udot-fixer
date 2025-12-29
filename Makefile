.PHONY: test clean convert help

RACKET := racket
SRC := src/udot.rkt

help:
	@echo "Usage:"
	@echo "  make test     - 运行测试并验证"
	@echo "  make convert  - 转换 test/test.s"
	@echo "  make clean    - 清理编译产物"

test:
	cd src && $(RACKET) udot.rkt

convert:
	$(RACKET) -e '(require "$(SRC)") (convert-asm-file "test/test.s" "test/test.converted.s")'

clean:
	rm -f build/*.o test/*.converted.s
