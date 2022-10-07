#include "object.h"
#include "gc.h"
#include <string.h>

static pscheme_t command_line;

pscheme_t pscheme_get_command_line(void) {
    return command_line;
}

void pscheme_main(void);

int main(int argc, char** argv) {
    command_line = PSCM_NIL;

    for (int i = argc - 1; i >= 0; --i) {
        char *arg_copy = pscheme_allocate_block(strlen(argv[i]));
        strcpy(arg_copy, argv[i]);
        command_line = pscheme_cons(((pscheme_t)arg_copy) | PSCM_T_STRING, command_line);
    }

    pscheme_main();

    return 0;
}
