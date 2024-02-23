#include <stdlib.h>
#include <libdwarf/libdwarf.h>

static Dwarf_Debug debug = 0;

static int init_dwarf(void) {
    static int saved_rc = -8;
    if (saved_rc != -8) return saved_rc;

    int rc = dwarf_init_path("/proc/self/exe", NULL, 0, DW_GROUPNUMBER_ANY, NULL, NULL, &debug, NULL);

    if (rc == DW_DLV_ERROR) abort();

    saved_rc = rc;
    return saved_rc;
}

void pscheme_backtrace(void) {
    if (init_dwarf() != DW_DLV_OK) return;

    Dwarf_Cie *cie_data = 0;
    Dwarf_Signed cie_count = 0;
    Dwarf_Fde *fde_data = 0;
    Dwarf_Signed fde_count = 0;
    int fres = dwarf_get_fde_list(debug, &cie_data, &cie_count, &fde_data, &fde_count, NULL);
    if (fres != DW_DLV_OK) return;



    dwarf_dealloc_fde_cie_list(dbg, cie_data, cie_count, fde_data,fde_count);
    return;
}

void pscheme_abort(void) {
    pscheme_backtrace();
    abort();
}
