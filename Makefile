.PHONY: all test clean help

RACKET := racket
SRC := src/udot.rkt

help:
	@echo "Usage:"
	@echo "  make        - 运行完整流程 (转换 + 验证)"
	@echo "  make test   - 同上"
	@echo "  make clean  - 清理编译产物"

all: test

test:
	cd src && $(RACKET) udot.rkt

clean:
	rm -rf build/*.o test-convert/
