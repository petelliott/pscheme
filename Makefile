CC=gcc
CFLAGS=-Wall -g -fno-omit-frame-pointer -I. -fPIE

RUNTIME_OBJS=runtime/gc.o runtime/object.o runtime/rest.o runtime/apply.o \
	         runtime/gdb_exts.o runtime/ports.o runtime/srfi-170.o runtime/main.o
RUNTIME_TARGET=runtime/runtime.a

pscheme1: $(RUNTIME_TARGET)
	./pscheme0.sh -pg ./scm/pscheme/compiler/main.scm -o pscheme1

$(RUNTIME_TARGET): $(RUNTIME_OBJS)
	ar cr $@ $?

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $^ -o $@


.PHONY: clean check pscheme1

clean:
	rm -rf runtime/*.a runtime/*.o
	find . -name '*.ll' | xargs rm -rf
	find . -name '*.o' | xargs rm -rf
	find . -name '*.pir' | xargs rm -rf
	rm -rf pscheme1

check:
	test/test
