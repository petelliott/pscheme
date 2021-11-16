#ifndef PSCHEME_OBJECT_H
#define PSCHEME_OBJECT_H

#include <stdint.h>
#include <stdbool.h>

// there's room for 16 tags
enum pscheme_tags {
    PSCM_T_FIXNUM = 0,
    PSCM_T_CONS,
    PSCM_T_SINGLETON,
    PSCM_T_STRING,
    PSCM_T_CHAR
};

typedef uintptr_t pscheme_t;

static inline uintptr_t sra(uintptr_t x, uintptr_t y) {
    uintptr_t bottom = x >> y;
    uintptr_t top = -((x & (1lu << 63)) >> y);
    return top | bottom;
}


static inline void *ptr(pscheme_t obj) { return (void *)(obj & ~0xf); }
static inline uintptr_t unum(pscheme_t obj) { return obj >> 4; }
static inline intptr_t num(pscheme_t obj) { return (intptr_t)(sra(obj, 4)); }
static inline uintptr_t tag(pscheme_t obj) { return obj & 0xf; }

struct pscheme_cons_cell {
    pscheme_t car;
    pscheme_t cdr;
};

pscheme_t pscheme_cons(pscheme_t a, pscheme_t b);
pscheme_t pscheme_car(pscheme_t obj);
pscheme_t pscheme_cdr(pscheme_t obj);

enum pscheme_singletons {
    PSCM_S_NIL = 0,
    PSCM_S_F,
    PSCM_S_T,
    PSCM_S_EOF,
    PSCM_S_UNSPECIFIED,
    PSCM_S_UNBOUND
};

#define SINGLETON(n) ((pscheme_t) (n << 4) | PSCM_T_SINGLETON)

#define PSCM_NIL SINGLETON(PSCM_S_NIL)
#define PSCM_F SINGLETON(PSCM_S_F)
#define PSCM_T SINGLETON(PSCM_S_T)
#define PSCM_UNSPECIFIED SINGLETON(PSCM_S_UNSPECIFIED)
#define PSCM_UNBOUND SINGLETON(PSCM_S_UNBOUND)

static inline pscheme_t make_pscm_bool(bool b) {
    return (pscheme_t) (((PSCM_S_F << 4) << b) | PSCM_T_SINGLETON);
}

static inline pscheme_t make_pscm_fixnum(intptr_t n) {
    return n << 4 | PSCM_T_FIXNUM;
}

#endif
