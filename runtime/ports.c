#include <stdio.h>

FILE* pscheme_default_input_port_file(void) {
    return stdin;
}

FILE* pscheme_default_output_port_file(void) {
    return stdout;
}

FILE* pscheme_default_error_port_file(void) {
    return stderr;
}
