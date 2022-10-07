CC=gcc
CFLAGS=-Wall -g -fno-omit-frame-pointer -I. -fPIE

RUNTIME_OBJS=runtime/gc.o runtime/object.o runtime/rest.o runtime/apply.o \
	         runtime/gdb_exts.o runtime/ports.o runtime/srfi-170.o runtime/main.o
RUNTIME_TARGET=runtime/runtime.a

$(RUNTIME_TARGET): $(RUNTIME_OBJS)
	ar cr $@ $?

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $^ -o $@


.PHONY: clean check

clean:
	rm runtime/*.a runtime/*.o

check:
	test/test
