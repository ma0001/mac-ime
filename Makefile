# Makefile
CC = clang
CFLAGS = -Wall -O2 -fPIC -fobjc-arc
LDFLAGS = -dynamiclib -framework Cocoa -framework Carbon

# Include path for emacs-module.h
CFLAGS += -I./src -I./src/emacs-27.1

SRC = src/mac_ime.m
OBJ = mac-ime-module.so

all: $(OBJ)

$(OBJ): $(SRC)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

clean:
	rm -f $(OBJ)

test: $(OBJ)
	@echo "Running Mock Tests..."
	emacs -Q -batch -L . -L test -l test/mac-ime-mock-test.el -f ert-run-tests-batch-and-exit
	@echo "Running Integration Tests..."
	emacs -Q -batch -L . -L test -l test/mac-ime-integration-test.el -f ert-run-tests-batch-and-exit

.PHONY: all clean test
