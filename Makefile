CC=gcc
CFLAGS=-Wall -g -I. -fPIE -O2

RUNTIME_OBJS=runtime/gc.o runtime/object.o runtime/rest.o runtime/apply.o \
	         runtime/gdb_exts.o runtime/ports.o runtime/srfi-170.o \
	         runtime/main.o runtime/symbol_table.o runtime/rangetable.o
RUNTIME_TARGET=runtime/runtime.a

pscheme: $(RUNTIME_TARGET) pscheme1
	./pscheme1 -p --fresh ./scm/pscheme/compiler/main.scm -o pscheme

pscheme1: $(RUNTIME_TARGET)
	./pscheme0.sh -p --fresh ./scm/pscheme/compiler/main.scm -o pscheme1

dev: $(RUNTIME_TARGET)
	./pscheme -pg ./scm/pscheme/compiler/main.scm -o pscheme-dev

dev0: $(RUNTIME_TARGET)
	./pscheme0.sh -pg ./scm/pscheme/compiler/main.scm -o pscheme-dev

$(RUNTIME_TARGET): $(RUNTIME_OBJS)
	ar cr $@ $?

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

%.o: %.s
	$(CC) $(CFLAGS) -c $^ -o $@


.PHONY: clean check pscheme1 pscheme dev

clean:
	rm -rf runtime/*.a runtime/*.o
	find . -name '*.ll' | xargs rm -rf
	find . -name '*.o' | xargs rm -rf
	find . -name '*.pir' | xargs rm -rf
	rm -rf pscheme1

check:
	cmp pscheme1 pscheme
	test/test
