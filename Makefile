CC=gcc
CFLAGS=-Wall -g -fno-omit-frame-pointer -I.

RUNTIME_OBJS=runtime/runtime.o runtime/gc.o runtime/object.o
RUNTIME_TARGET=runtime/runtime.a

$(RUNTIME_TARGET): $(RUNTIME_OBJS)
	ar cr $@ $?

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@


%.elf: %.scm $(RUNTIME_TARGET)
	gosh -r7 compiler/main.scm < $< | $(CC) $(CFLAGS) -o $@ -xassembler - -xnone $(RUNTIME_TARGET)

.PHONY: clean

clean:
	rm runtime/*.a runtime/*.o
