# Makefile
CC = clang
CFLAGS = -Wall -O2 -fPIC -fobjc-arc
LDFLAGS = -dynamiclib -framework Cocoa -framework Carbon

# Include path for emacs-module.h
CFLAGS += -I./src -I./src/emacs-27.1 -I./src/emacs-27.1

SRC = src/mac_ime.m
OBJ = mac-ime-module.so

all: $(OBJ)

$(OBJ): $(SRC)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

clean:
	rm -f $(OBJ)

test: $(OBJ)
	emacs -Q -batch -L . -l test/mac-ime-test.el -f ert-run-tests-batch-and-exit

.PHONY: all clean test
