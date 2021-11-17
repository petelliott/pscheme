CC=gcc
CFLAGS=-Wall -g -fno-omit-frame-pointer -I.

RUNTIME_OBJS=runtime/gc.o runtime/object.o runtime/number.o runtime/list.o runtime/ports.o runtime/rest.o
RUNTIME_TARGET=runtime/runtime.a

$(RUNTIME_TARGET): $(RUNTIME_OBJS)
	ar cr $@ $?

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $^ -o $@


.PHONY: clean

clean:
	rm runtime/*.a runtime/*.o
