# Makefile
CC = clang
CFLAGS = -Wall -O2 -fPIC -fobjc-arc
LDFLAGS = -dynamiclib -framework Cocoa

# Include path for emacs-module.h
CFLAGS += -I./src -I./src/emacs-27.1 -I./src/emacs-27.1

SRC = src/ime_hook.m
OBJ = ime-hook-module.so

all: $(OBJ)

$(OBJ): $(SRC)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

clean:
	rm -f $(OBJ)

.PHONY: all clean
