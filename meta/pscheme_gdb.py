import gdb

PSCM_T_FIXNUM = 0
PSCM_T_CONS = 1
PSCM_T_SINGLETON = 2
PSCM_T_STRING = 3
PSCM_T_CHAR = 4
PSCM_T_CLOSURE = 5
PSCM_T_SYMBOL = 6
PSCM_T_FOREIGN = 7
PSCM_T_SLOTS = 8

PSCM_S_NIL = 0
PSCM_S_F = 1
PSCM_S_T = 2
PSCM_S_EOF = 3
PSCM_S_UNSPECIFIED = 4
PSCM_S_UNBOUND = 5

def tag(n):
    return n & 0xf

def unum(n):
    return n >> 4;

def num(n):
    u = unum(n);
    if u >> 59 == 1:
        return u - (1 << 60)
    else:
        return u

def ptr(n):
    return (n >> 4) << 4;

def list_write(n):
    car = gdb.Value(ptr(n)).cast(gdb.lookup_type("pscheme_t").pointer()).dereference()
    cdr = gdb.Value(ptr(n) + 8).cast(gdb.lookup_type("pscheme_t").pointer()).dereference()
    cdrn = int(cdr)
    if tag(cdrn) == PSCM_T_CONS:
        return f"{car.format_string()} {list_write(int(cdr))}"
    elif tag(cdrn) == PSCM_T_SINGLETON and unum(cdrn) == PSCM_S_NIL:
        return f"{car.format_string()}"
    else:
        return f"{car.format_string()} . {cdr.format_string()}"

class PschemePrettyPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        #TODO: I think the next release of gdb will have gdb.print_options which will make this better I hope
        n = int(self.val)
        if tag(n) == PSCM_T_FIXNUM:
            return str(num(n))
        elif tag(n) == PSCM_T_CONS:
            return f"({list_write(n)})"
        elif tag(n) == PSCM_T_SINGLETON:
            if unum(n) == PSCM_S_NIL:
                return "()"
            elif unum(n) == PSCM_S_F:
                return "#f"
            elif unum(n) == PSCM_S_T:
                return "#t"
            elif unum(n) == PSCM_S_EOF:
                return "#<eof>"
            elif unum(n) == PSCM_S_UNSPECIFIED:
                return "#<unspecified>"
        elif tag(n) == PSCM_T_STRING:
            s = gdb.Value(ptr(n)).cast(gdb.lookup_type("char").pointer())
            return s.format_string(address=False)
        elif tag(n) == PSCM_T_CHAR:
            return f"#\\{chr(unum(n))}"
        elif tag(n) == PSCM_T_CLOSURE:
            return f"#<closure {hex(ptr(n))}>"
        elif tag(n) == PSCM_T_SYMBOL:
            return gdb.Value(ptr(n)).cast(gdb.lookup_type("char").pointer()).string()
        elif tag(n) == PSCM_T_SLOTS:
            return f"#<slots-object {hex(ptr(n))}>"
        else:
            return f"#<? {hex(unum(n))}>"

def pscheme_printer(val):
    if val.type.name == "pscheme_t":
        return PschemePrettyPrinter(val)
    else:
        return None

gdb.pretty_printers.append(pscheme_printer)
