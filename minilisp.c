#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>
#include <ctype.h>
#include <sys/mman.h>

#include <readline/readline.h>
#include <readline/history.h>

typedef unsigned long VALUE;

/*
enum {
    TFREE,
    TINT,
    TSTRING,
    TCELL,
    TSYMBOL,
    TPRIMITIVE,
    TFUNCTION,
    TMACRO,
    TSPE,

    // Spetypes
    TNIL = 100,
    TDOT,
    TCPAREN,
    TTRUE,
} Type;
*/

struct Obj;

typedef struct Env {
    VALUE vars;
    struct Env *next;
} Env;

typedef struct VALUE Primitive(Env *env, struct VALUE root, struct VALUE *args);

struct ObjectHeap;

typedef struct Obj {
    int type;
    struct ObjectHeap *heap;
    union {
        struct Obj* next;
        // Int
        struct {
            int value;
        };
        // String
        struct {
            char *strbody;
        };
        // Cell
        struct {
            struct Obj* car;
            struct Obj* cdr;
        };
        // Symbol
        struct {
            char *name;
        };
        // Primitive
        struct {
            Primitive *fn;
        };
        // Function and Macro
        struct {
            struct Obj* params;
            struct Obj* body;
        };
        // Spe
        struct {
            int spetype;
        };
    };
} Obj;

typedef enum Type {
  T_CELL     = 0x00, // 0000
  T_FIXNUM   = 0x01, // xxx1
  T_ATOM     = 0x02, // 0010
  T_FUNCTION = 0x04, // 0100
  T_MACRO    = 0x06, // 0110
} Type;

typedef enum AtomType {
  T_IMMEDIATE = 0x02, // 0000 0010
  T_STRING    = 0x12, // 0001 0010
  T_SYMBOL    = 0x22, // 0010 0010
  T_PRIMITIVE = 0x32, // 0011 0010
  T_VECTOR    = 0x42, // 0100 0010
} AtomType;

typedef enum Immediate {
  FALSE  = 0x00,
  NIL    = 0x00,
  TRUE   = 0x01,
  DOT    = 0x02,
  CPAREN = 0x03,
} Immediate;

typedef struct Object {
  int dummy1;
  struct ObjectHeap *heap;
  union {
    // Free Object
    struct {
      struct Object *next;
    } free;
    // Cell Object
    struct {
      VALUE car;
      VALUE cdr;
    } cell;
    // Atom Object
    struct {
      int type;
      union {
        struct {
          int value;
        } immediate;
        struct {
        } vector;
        struct {
          char *value;
        } string;
        struct {
          char *value;
        }symbol;
        struct {
          Primitive *value;
        } primitive;
      } as;
    } atom;
    // Function and Macro
    struct {
      VALUE params;
      VALUE body;
    } function, macro;
  } as;
} Object;

#define VALUE_OF(e, type) ((type == T_FIXNUM) ? ((((VALUE) e) << (1)) & T_FIXNUM) : (((VALUE) e) & (type)))

#define TYPE(e)           ((((VALUE) e) & (0x01)) ? T_FIXNUM : (((VALUE) e) & (0x07)))
#define FIXNUM(e)         ((int) (((VALUE) e) >> (1)))
#define OBJECT(e)         ((Object*) (((VALUE) e) & (~0x07)))

#define NEXT_FREE_OBJ(e) ((OBJECT(e))->as.free.next)
#define CELL(e)          ((OBJECT(e))->as.cell)
#define CAR(e)           ((CELL(e)).car)
#define CDR(e)           ((CELL(e)).cdr)
#define ATOM(e)          ((OBJECT(e))->as.atom)
#define ATOM_TYPE(e)     ((ATOM(e)).type)
#define IMMEDIATE(e)     ((ATOM(e)).as.immediate.value)
#define STRING(e)        ((ATOM(e)).as.string.value)
#define SYMBOL(e)        ((ATOM(e)).as.symbol.value)
#define PRIMITIVE(e)     ((ATOM(e)).as.primitive.value)
#define FUNCTION(e)      ((OBJECT(e))->as.function)
#define MACRO(e)         (FUNCTION(e))

typedef unsigned char Octet;

typedef struct ObjectHeap {
    Octet *marks;
    Octet *vectors;
    Object *ptr;
} ObjectHeap;

typedef struct ObjectSpace {
  int len;
  int capa;
  ObjectHeap **heaps;
} ObjectSpace;

static VALUE Nil;
static VALUE Dot;
static VALUE Cparen;
static VALUE True;

#define MEMORY_SIZE 1024
#define MAX_HEAPS_SIZE 1024

static ObjectSpace memory;
static Object *free_list;
static int gc_running = 0;
#define DEBUG_GC 0

void error(char *fmt, ...);
static VALUE read(Env *env, VALUE root, char **p);
VALUE read_one(Env *env, VALUE root, char **p);
VALUE make_cell(Env *env, VALUE root, VALUE car, VALUE cdr);
void gc(Env *env, VALUE root);
void print(VALUE obj);
Object *alloc_heap(size_t size);

#define VAR_CELL(X) (X ## _cell)
#define VAR_VAL(X)  (X ## _value)
#define VAR(X)                                          \
  Object VAR_CELL(X);                                   \
  VALUE  VAR_VAL(X) = VALUE_OF(&(VAR_CELL(X)), T_CELL); \
  OBJECT(VAR_VAL(X))->dummy1 = NULL; /* TODO delete*/   \
  OBJECT(VAR_VAL(X))->heap = NULL;   /* TODO delete*/   \
  CAR(VAR_VAL(X)) = NULL;                               \
  CDR(VAR_VAL(X)) = root;                               \
  root = VAR_VAL(X);                                    \
  VALUE* X = &(CAR(VAR_VAL(X)))

VALUE alloc(Env *env, VALUE root, int type) {
    if (type == T_FIXNUM) {
      // TODO error
      exit(2);
    }

    if (!free_list) gc(env, root);
    if (!free_list) {
      if (DEBUG_GC) {
        printf("FreeList is empty.\n");
      }
      free_list = alloc_heap(MEMORY_SIZE);
      if (!free_list) error("memory exhausted");
    }

    Object *object = free_list;
    free_list = object->as.free.next;

    VALUE value = VALUE_OF(object, TYPE(type));
    if (TYPE(value) == T_ATOM) {
      ATOM_TYPE(value) = type;
    }

    return value;
}

VALUE make_int(Env *env, VALUE root, int value) {
    /*
    VALUE r = alloc(env, root, TINT);
    r->value = value;
    return r;
    */
  return VALUE_OF(value, T_FIXNUM);
}

VALUE make_string(Env *env, VALUE root, char *body) {
    /*
    VALUE str = alloc(env, root, TSTRING);
    str->strbody = strdup(body);
    return str;
    */
  VALUE value = alloc(env, root, T_STRING);
  STRING(value) = body;
  return value;
}

VALUE make_cell(Env *env, VALUE root, VALUE car, VALUE cdr) {
    /*
    VALUE cell = alloc(env, root, TCELL);
    cell->car = *car;
    cell->cdr = *cdr;
    return cell;
    */
  VALUE value = alloc(env, root, T_CELL);
  CAR(value) = car;
  CDR(value) = cdr;
  return value;
}

VALUE find(char *name, Env *env);

VALUE make_symbol(Env *env, VALUE root, char *name) {
    /*
    VALUE sym = alloc(env, root, TSYMBOL);
    sym->name = strdup(name);
    return sym;
    */
  VALUE value = alloc(env, root, T_SYMBOL);
  SYMBOL(value) = name;
  return value;
}

VALUE make_primitive(Env *env, VALUE root, Primitive *fn) {
    /*
    VALUE r = alloc(env, root, TPRIMITIVE);
    r->fn = fn;
    return r;
    */
    VALUE value = alloc(env, root, T_PRIMITIVE);
    PRIMITIVE(value) = fn;
    return value;
}

VALUE make_function(Env *env, VALUE root, int type, VALUE params, VALUE body) {
    /*
    if (type != TFUNCTION && type != TMACRO)
        error("Bug: invalid argument for make_function");
    VALUE r = alloc(env, root, type);
    r->params = *params;
    r->body = *body;
    return r;
    */
    if (type != T_FUNCTION && type != T_MACRO) {
      error("Bug: invalid argument for make_function");
    }
    VALUE value = alloc(env, root, type);
    FUNCTION(value).params = params;
    FUNCTION(value).body = body;
    return value;
}

VALUE make_spe(int spetype) {
    /*
    VALUE r = malloc(sizeof(Obj));
    r->type = TSPE;
    r->spetype = spetype;
    return r;
    */
    void *p = malloc(sizeof(Object));
    VALUE value = VALUE_OF(p, T_IMMEDIATE);
    IMMEDIATE(value) = spetype;
    return value;
}

/*
void print_cframe(VALUE root) {
    VALUE *cframe = root;
    for (;;) {
        if (!*cframe) break;
        VALUE *ptr = cframe + 2;
        printf(" %s: ", (char *)cframe[1]);
        for (; *ptr != (VALUE )-1; ptr++) {
            if (*ptr) {
                print(*ptr);
            } else {
                printf("- ");
            }
            printf(" ");
        }
        printf("\n");
        cframe = *(VALUE **)cframe;
    }
}
*/

// TODO value対応
#define HEAP_OF(obj)     (OBJECT(obj)->heap)
#define POS_IN_HEAP(obj) (OBJECT(obj) - HEAP_OF(obj)->ptr)
#define MARK(obj)        (HEAP_OF(obj)->marks[POS_IN_HEAP(obj) >> 3] |= 1 << (POS_IN_HEAP(obj) & 7))
#define MARKED(obj)      (HEAP_OF(obj)->marks[POS_IN_HEAP(obj) >> 3] & (1 << (POS_IN_HEAP(obj) & 7)))

void mark_obj(VALUE obj)
{
    /*
    if (obj && obj->type != TSPE && !MARKED(obj)) {
        if (DEBUG_GC)
            printf("marking %p (type: %d)\n", obj, obj->type);

        MARK(obj);
        switch (obj->type) {
        case TCELL:
            mark_obj(obj->car);
            mark_obj(obj->cdr);
            break;
        case TFUNCTION:
        case TMACRO:
            mark_obj(obj->params);
            mark_obj(obj->body);
            break;
        }
    }
    */
  if (obj && !(TYPE(obj) == T_ATOM && ATOM_TYPE(obj) == T_IMMEDIATE) && !MARKED(obj)) {
    MARK(obj);
    switch (TYPE(obj)) {
    case T_CELL:
      mark_obj(CAR(obj));
      mark_obj(CDR(obj));
      break;
    case T_FUNCTION:
    case T_MACRO:
      mark_obj(FUNCTION(obj).params);
      mark_obj(FUNCTION(obj).body);
      break;
    }
  }
}

void mark_from_env(Env *env, VALUE root)
{
    Env *frame = env;
    for (; frame; frame = frame->next) {
        mark_obj(frame->vars);
    }
}

void mark_from_root(Env *env, VALUE root)
{
    /*
    VALUE frame = root;
    for (; frame; frame = frame->cdr) {
        mark_obj(frame->car);
    }
    */
  VALUE frame = root;
  for (; frame; frame = CDR(frame)) {
    mark_obj(CAR(frame));
  }
    /*
    VALUE *cframe = root;
    for (; cframe; cframe = (VALUE *) cframe[0]) {
        int i = 2;
        for (; cframe[i] != (VALUE ) -1; i++) {
            mark_obj(cframe[i]);
        }
    }
    */
}

void mark(Env *env, VALUE root)
{
    int i;
    for (i = 0; i < memory.len; i++) {
        ObjectHeap *heap = memory.heaps[i];
        memset(heap->marks, 0, MEMORY_SIZE / sizeof(Obj) / 8);
    }
    mark_from_env(env, root);
    mark_from_root(env, root);
}

void sweep_obj(VALUE obj)
{
/*
    if (DEBUG_GC)
        printf("sweeping %p (type: %d)\n", obj, obj->type);
        */

/*
    switch (obj->type) {
    case TSTRING:
        free(obj->strbody);
        break;
    case TSYMBOL:
        free(obj->name);
        break;
    }

    obj->type = TFREE;
    obj->next = free_list;
    free_list = obj;
    */

  switch (TYPE(obj)) {
    case T_STRING:
      free(STRING(obj));
      break;
    case T_SYMBOL:
      free(SYMBOL(obj));
      break;
  }
  Object *object = OBJECT(obj);
  object->as.free.next = free_list;
  free_list = object;
}

void sweep(Env *env, VALUE root)
{
    free_list = NULL;
    int i;
    for (i = 0; i < memory.len; i++) {
        ObjectHeap* heap = memory.heaps[i];
        int j;
        for (j = 0; j < MEMORY_SIZE / sizeof(Obj); j++) {
            if (!(heap->marks[j >> 3] & (1 << (j & 7)))) {
                sweep_obj(&heap->ptr[j]);
            }
        }
    }
}

void gc(Env *env, VALUE root) {
    if (gc_running)
        error("Bug: GC is already running");
    gc_running = 1;
    mark(env, root);
    sweep(env, root);
    gc_running = 0;
}

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

VALUE read_sexp(Env *env, VALUE root, char **p) {
    VAR(obj);
    VAR(head);
    VAR(tail);
    VAR(tmp);
    for (;;) {
        *obj = read_one(env, root, p);
        if (!*obj)
            error("unclosed parenthesis");
        if (*obj == Dot) {
            if (*head == NULL)
                error("stray dot");
            *tmp = read_one(env, root, p);
            (*tail)->cdr = *tmp;
            break;
        }
        if (*obj == Cparen) {
            if (*head == NULL)
                return Nil;
            break;
        }
        if (*head == NULL) {
            (*head) = (*tail) = make_cell(env, root, obj, &Nil);
        } else {
            *tmp = make_cell(env, root, obj, &Nil);
            (*tail)->cdr = *tmp;
            (*tail) = (*tail)->cdr;
        }
    }
    return *head;
}

VALUE intern(Env *env, VALUE root, char *name) {
    VALUE old = find(name, env);
    if (old) return old->car;
    return make_symbol(env, root, name);
}

VALUE read_quote(Env *env, VALUE root, char **p) {
    VAR(sym);
    VAR(tmp);
    *sym = intern(env, root, "quote");
    *tmp = read(env, root, p);
    *tmp = make_cell(env, root, tmp, &Nil);
    *tmp = make_cell(env, root, sym, tmp);
    return *tmp;
}

VALUE read_number(Env *env, VALUE root, char **p, int val) {
    for (;;) {
        char c = **p;
        switch (c) {
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            (*p)++;
            val = val * 10 + (c - '0');
            break;
        default:
            return make_int(env, root, val);
        }
    }
}

#define SYMBOL_MAX_LEN 200

VALUE read_symbol(Env *env, VALUE root, char **p, char c) {
    char buf[SYMBOL_MAX_LEN];
    int len = 1;
    buf[0] = c;
    for (;;) {
        char c = **p;
        if (isalnum(c) || c == '-') {
            if (SYMBOL_MAX_LEN + 1 < len)
                error("symbol name too long");
            (*p)++;
            buf[len++] = c;
            continue;
        }
        buf[len] = '\0';
        return intern(env, root, buf);
    }
}

VALUE read_one(Env *env, VALUE root, char **p) {
    switch (**p) {
    case ' ': case '\n': case '\r': case '\t':
        (*p)++;
        return read_one(env, root, p);
    case ')':
        (*p)++;
        return Cparen;
    case '.':
        (*p)++;
        return Dot;
    default:
        return read(env, root, p);
    }
}

static VALUE read(Env *env, VALUE root, char **p) {
    for (;;) {
        char c = **p;
        (*p)++;
        if (c == '\0')
            return NULL;
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            continue;
        if (c == '(')
            return read_sexp(env, root, p);
        if (c == ')')
            error("unclosed open parenthesis");
        if (c == '\'')
            return read_quote(env, root, p);
        if (isdigit(c))
            return read_number(env, root, p, c - '0');
        if (isalpha(c) || strchr("+=!@#$%^&*", c))
            return read_symbol(env, root, p, c);
        error("don't know how to handle %c", c);
    }
}

void print(VALUE obj) {
    switch (obj->type) {
    case TINT:
        printf("%d", obj->value);
        return;
    case TSTRING:
        printf("%s", obj->strbody);
        return;
    case TCELL:
        printf("(");
        for (;;) {
            print(obj->car);
            if (obj->cdr == Nil) {
                break;
            }
            if (obj->cdr->type == TCELL && !DEBUG_GC) {
                printf(" ");
                obj = obj->cdr;
                continue;
            }
            printf(" . ");
            print(obj->cdr);
            break;
        }
        printf(")");
        return;
    case TSYMBOL:
        printf("%s", obj->name);
        return;
    case TPRIMITIVE:
        printf("<primitive>");
        return;
    case TFUNCTION:
        printf("<function>");
        return;
    case TMACRO:
        printf("<macro>");
        return;
    case TSPE:
        if (obj == Nil)
            printf("()");
        else if (obj == True)
            printf("t");
        else
            error("Bug: print: Unknown SPE type: %d", obj->spetype);
        return;
    default:
        error("Bug: print: Unknown tag type: %d", obj->type);
    }
}

int list_length(VALUE list) {
    if (list == Nil) return 0;
    int len = 1;
    for (;;) {
        if (list->cdr == Nil)
            return len;
        if (list->cdr->type != TCELL)
            error("length: cannot handle incomplete list");
        list = list->cdr;
        len++;
    }
}

VALUE eval(Env *env, VALUE root, VALUE *obj);

void add_var_int(Env *env, VALUE root, VALUE *sym, VALUE *val) {
    VAR(cell);
    VAR(tmp);
    *cell = make_cell(env, root, sym, val);
    *tmp = env->vars;
    *tmp = make_cell(env, root, cell, tmp);
    env->vars = *tmp;
}

void add_env(Env *env, VALUE root,  Env *newenv, VALUE *vars, VALUE *values) {
    if (list_length(*vars) != list_length(*values))
        error("cannot apply function: number of argument does not match");
    VAR(p);
    VAR(q);
    VAR(sym);
    VAR(val);
    VAR(def);
    VAR(map);
    *map = Nil;
    int i;
    for (p = vars, q = values; *p != Nil; *p = (*p)->cdr, *q = (*q)->cdr) {
        *val = (*q)->car;
        *sym = (*p)->car;
        *def = make_cell(env, root, sym, val);
        *map = make_cell(env, root, def, map);
    }
    newenv->vars = *map;
    newenv->next = env;
}

void free_env(Env *env) {
    free(env);
}

VALUE progn(Env *env, VALUE root, VALUE *body) {
    VAR(car);
    for (;;) {
        *car = (*body)->car;
        if ((*body)->cdr == Nil)
            return eval(env, root, car);
        eval(env, root, car);
        *body = (*body)->cdr;
    }
}

VALUE eval_list(Env *env, VALUE root, VALUE *list) {
    VAR(head);
    VAR(tail);
    VAR(lp);
    VAR(tmp);
    for (lp = list; *lp != Nil; *lp = (*lp)->cdr) {
        *tmp = (*lp)->car;
        *tmp = eval(env, root, tmp);
        if (*head == NULL) {
            *head = *tail = make_cell(env, root, tmp, &Nil);
        } else {
            *tmp = make_cell(env, root, tmp, &Nil);
            (*tail)->cdr = *tmp;
            *tail = (*tail)->cdr;
        }
    }
    if (head == NULL)
        error("eval_list: empty list?");
    return *head;
}

VALUE apply(Env *env, VALUE root, VALUE *fn, VALUE *args) {
    if ((*fn)->type == TPRIMITIVE) {
        if ((*args) != Nil && (*args)->type != TCELL)
            error("argument must be a list");
        return (*fn)->fn(env, root, args);
    }
    if ((*fn)->type == TFUNCTION) {
        VAR(body);
        VAR(params);
        VAR(eargs);
        *body = (*fn)->body;
        *params = (*fn)->params;
        Env newenv;
        *eargs = eval_list(env, root, args);
        add_env(env, root, &newenv, params, eargs);
        return progn(&newenv, root, body);
    }
    error("not supported");
    return NULL;
}

VALUE find(char *name, Env *env) {
    for (; env; env = env->next) {
        VALUE cell;
        for (cell = env->vars; cell != Nil; cell = cell->cdr) {
            VALUE var = cell->car;
            char *varname = var->car->name;
            if (strcmp(name, varname) == 0)
                return var;
        }
    }
    return NULL;
}

VALUE macroexpand(Env *env, VALUE root, VALUE *obj) {
    if ((*obj)->type != TCELL || (*obj)->car->type != TSYMBOL)
        return *obj;
    VAR(macro);
    VAR(args);
    VAR(body);
    VAR(params);
    *macro = find((*obj)->car->name, env);
    if (!*macro)
        return *obj;
    *macro = (*macro)->cdr;
    if ((*macro)->type != TMACRO)
        return *obj;
    *args = (*obj)->cdr;
    *body = (*macro)->body;
    *params = (*macro)->params;
    Env newenv;
    add_env(env, root, &newenv, params, args);
    return progn(&newenv, root, body);
}

VALUE eval(Env *env, VALUE root, VALUE *obj) {
    if ((*obj)->type == TINT || (*obj)->type == TSTRING ||
        (*obj)->type == TPRIMITIVE || (*obj)->type == TFUNCTION ||
        (*obj)->type == TSPE)
        return *obj;
    if ((*obj)->type == TCELL) {
        VAR(fn);
        VAR(car);
        VAR(args);
        *car = (*obj)->car;
        *args = (*obj)->cdr;
        *fn = eval(env, root, car);
	if ((*fn)->type == TMACRO) {
	    VAR(macro);
	    *macro = macroexpand(env, root, obj);
	    return eval(env, root, macro);
	}else if ((*fn)->type != TPRIMITIVE && (*fn)->type != TFUNCTION){
            error("Car must be a function");
	}else
	    return apply(env, root, fn, args);
    }
    if ((*obj)->type == TSYMBOL) {
        VALUE val = find((*obj)->name, env);
        if (!val)
            error("undefined symbol: %s", (*obj)->name);
        return val->cdr;
    }
    error("BUG: eval: Unknown tag type: %d", (*obj)->type);
    return NULL;
}

#define BUFSIZE 250

VALUE prim_quote(Env *env, VALUE root, VALUE *list) {
    if (list_length(*list) != 1)
        error("malformed quote");
    return (*list)->car;
}

VALUE prim_list(Env *env, VALUE root, VALUE *list) {
    return eval_list(env, root, list);
}

VALUE prim_car(Env *env, VALUE root, VALUE *list) {
    VALUE args = eval_list(env, root, list);
    if (args->car->type != TCELL)
        error("car takes only a cell");
    return args->car->car;
}

VALUE prim_cdr(Env *env, VALUE root, VALUE *list)
{
    VAR(args);
    *args = eval_list(env, root, list);
    return (*args)->car->cdr;
}

VALUE prim_cons(Env *env, VALUE root, VALUE *list)
{
    VAR(args);
    *args = eval_list(env, root, list);
    return make_cell(env, root, &(*args)->car, &(*args)->cdr->car);
}

VALUE prim_setq(Env *env, VALUE root, VALUE *list) {
    if (list_length(*list) != 2 ||
        (*list)->car->type != TSYMBOL)
        error("malformed setq");
    VAR(bind);
    VAR(value);
    *bind = find((*list)->car->name, env);
    if (!*bind)
        error("unbound variable", (*list)->car->name);
    *value = (*list)->cdr->car;
    (*bind)->cdr = eval(env, root, value);
}

VALUE prim_plus(Env *env, VALUE root, VALUE *list) {
    VAR(args);
    *args = eval_list(env, root, list);
    int sum = 0;
    for (;;) {
        if ((*args)->car->type != TINT)
            error("+ takes only numbers");
        sum += (*args)->car->value;
        if ((*args)->cdr == Nil)
            break;
        if ((*args)->cdr->type != TCELL)
            error("+ does not take incomplete list");
        *args = (*args)->cdr;
    }
    return make_int(env, root, sum);
}

VALUE prim_negate(Env *env, VALUE root, VALUE *list) {
    VAR(args);
    *args = eval_list(env, root, list);
    if ((*args)->car->type != TINT || (*args)->cdr != Nil)
        error("negate takes only one number");
    return make_int(env, root, -(*args)->car->value);
}

VALUE handle_function(Env *env, VALUE root, VALUE *list, int type) {
    if ((*list)->type != TCELL || (*list)->car->type != TCELL ||
        (*list)->cdr->type != TCELL) {
        error("malformed lambda");
    }
    VALUE p = (*list)->car;
    for (;;) {
        if (p->car->type != TSYMBOL)
            error("argument must be a symbol");
        if (p->cdr == Nil)
            break;
        if (p->cdr->type != TCELL)
            error("argument is not a flat list");
        p = p->cdr;
    }
    VAR(car);
    VAR(cdr);
    car = &(*list)->car;
    cdr = &(*list)->cdr;
    return make_function(env, root, type, car, cdr);
}

VALUE prim_lambda(Env *env, VALUE root, VALUE *list) {
    handle_function(env, root, list, TFUNCTION);
}

VALUE handle_defun(Env *env, VALUE root, VALUE *list, int type) {
    if ((*list)->car->type != TSYMBOL || (*list)->cdr->type != TCELL) {
        error("malformed defun");
    }
    VAR(fn);
    VAR(var);
    VAR(sym);
    VAR(rest);
    VAR(tmp);
    *sym = (*list)->car;
    *rest = (*list)->cdr;
    *fn = handle_function(env, root, rest, type);
    add_var_int(env, root, sym, fn);
    return *fn;
}

VALUE prim_defun(Env *env, VALUE root, VALUE *list) {
    return handle_defun(env, root, list, TFUNCTION);
}

VALUE prim_define(Env *env, VALUE root, VALUE *list) {
    if (list_length(*list) != 2 ||
        (*list)->car->type != TSYMBOL)
        error("malformed setq");
    VAR(sym);
    VAR(value);
    *sym = (*list)->car;
    *value = (*list)->cdr->car;
    *value = eval(env, root, value);
    add_var_int(env, root, sym, value);
    return *value;
}

VALUE prim_defmacro(Env *env, VALUE root, VALUE *list) {
    return handle_defun(env, root, list, TMACRO);
}

VALUE prim_macroexpand(Env *env, VALUE root, VALUE *list) {
    if (list_length(*list) != 1)
        error("malformed macroexpand");
    VAR(body);
    *body = (*list)->car;
    return macroexpand(env, root, body);
}

VALUE prim_println(Env *env, VALUE root, VALUE *list) {
    VAR(tmp);
    *tmp = (*list)->car;
    *tmp = eval(env, root, tmp);
    print(*tmp);
    printf("\n");
    return Nil;
}

VALUE prim_if(Env *env, VALUE root, VALUE *list) {
    int len = list_length(*list);
    if (len < 2)
        error("malformed if");
    VAR(cond);
    VAR(then);
    VAR(els);
    *cond = (*list)->car;
    *then = (*list)->cdr->car;
    *cond = eval(env, root, cond);
    if (len == 2)
        return *cond != Nil ? eval(env, root, then) : Nil;
    *els = (*list)->cdr->cdr;
    return *cond != Nil
        ? eval(env, root, then)
        : progn(env, root, els);
}

VALUE prim_num_eq(Env *env, VALUE root, VALUE *list) {
    if (list_length(*list) != 2)
        error("malformed =");
    VAR(values);
    *values = eval_list(env, root, list);
    if ((*values)->car->type != TINT || (*values)->cdr->car->type != TINT)
        error("= only takes number");
    return (*values)->car->value == (*values)->cdr->car->value ? True : Nil;
}

VALUE prim_num_lt(Env *env, VALUE root, VALUE *list)
{
    if (list_length(*list) != 2)
        error("malformed <");
    VAR(values);
    *values = eval_list(env, root, list);
    if ((*values)->car->type != TINT || (*values)->cdr->car->type != TINT)
        error("< only takes number");
    return (*values)->car->value < (*values)->cdr->car->value ? True : Nil;
}

VALUE prim_gc(Env *env, VALUE root, VALUE *list) {
    gc(env, root);
    return Nil;
}

VALUE prim_exit(Env *env, VALUE root, VALUE *list) {
    exit(0);
}

void add_var(Env *env, VALUE root, char *name, VALUE *var) {
    VAR(sym);
    VAR(cell);
    VAR(tmp);
    *sym = intern(env, root, name);
    add_var_int(env, root, sym, var);
}

void add_primitive(Env *env, VALUE root, char *name, Primitive *fn) {
    VAR(prim);
    *prim = make_primitive(env, root, fn);
    add_var(env, root, name, prim);
}

void define_consts(Env *env, VALUE root) {
    add_var(env, root, "t", &True);
}

void define_primitives(Env *env, VALUE root) {
    add_primitive(env, root, "quote", prim_quote);
    add_primitive(env, root, "list", prim_list);
    add_primitive(env, root, "car", prim_car);
    add_primitive(env, root, "cdr", prim_cdr);
    add_primitive(env, root, "cons", prim_cons);
    add_primitive(env, root, "setq", prim_setq);
    add_primitive(env, root, "+", prim_plus);
    add_primitive(env, root, "negate", prim_negate);
    add_primitive(env, root, "define", prim_define);
    add_primitive(env, root, "defun", prim_defun);
    add_primitive(env, root, "defmacro", prim_defmacro);
    add_primitive(env, root, "macroexpand", prim_macroexpand);
    add_primitive(env, root, "lambda", prim_lambda);
    add_primitive(env, root, "if", prim_if);
    add_primitive(env, root, "=", prim_num_eq);
    add_primitive(env, root, "prim-lt", prim_num_lt);
    add_primitive(env, root, "println", prim_println);
    add_primitive(env, root, "gc", prim_gc);
    add_primitive(env, root, "exit", prim_exit);
}

Object *alloc_heap(size_t size)
{
    if (memory.len >= memory.capa) {
      return NULL;
    }

    ObjectHeap *heap = malloc(sizeof(ObjectHeap));
    if (!heap) {
        return NULL;
    }

    heap->ptr = malloc(size);
    heap->marks = calloc(size / sizeof(Obj) / 8, sizeof(Octet));
    if (!heap->ptr || !heap->marks) {
        return NULL;
    }

    int i;
    for (i = 0; i < size / sizeof(Obj) - 1; i++) {
        heap->ptr[i].heap = heap;
        heap->ptr[i].next = &heap->ptr[i + 1];
    }
    heap->ptr[size / sizeof(Obj) - 1].heap = heap;
    heap->ptr[size / sizeof(Obj) - 1].next = NULL;

    memory.heaps[memory.len++] = heap;
    return heap->ptr;
}

void do_repl(Env *env, VALUE root)
{
    VAR(sexp);
    VAR(expanded);

    for (;;) {
        char *line = readline("minilisp> ");
        char *p = line;

        if (!line) break;
        *sexp = read(env, root, &p);
        add_history(line);
        free(line);
        if (!*sexp) continue;
        *expanded = macroexpand(env, root, sexp);
        print(eval(env, root, expanded));
        printf("\n");
    }
}

void eval_file(Env *env, VALUE root, char *fname)
{
    static char buf[BUFSIZE * 100]; /* about 100 lines of lisp code */

    FILE *fp = fopen(fname, "r");
    if (!fp) error("no such file");

    VAR(sexp);
    VAR(expanded);

    fread(buf, sizeof(buf), 1, fp);
    fclose(fp);

    char *p = buf;
    while (*p) {
        *sexp = read(env, root, &p);
        if (!*sexp) error("cannot load lisp program");
        *expanded = macroexpand(env, root, sexp);
        eval(env, root, expanded);
    }
}

int main(int argc, char **argv) {
    VALUE root = NULL;
    printf("sizeof(Obj): %d  MEMORY_SIZE: %d\n", sizeof(Obj), MEMORY_SIZE);

    memory.len = 0;
    memory.capa = MAX_HEAPS_SIZE;
    memory.heaps = malloc(sizeof(ObjectHeap*) * MAX_HEAPS_SIZE);

    free_list = alloc_heap(MEMORY_SIZE);

    if (DEBUG_GC)
        printf("MEMORY: %p + %x\n", memory, MEMORY_SIZE);

    Nil = make_spe(TNIL);
    Dot = make_spe(TDOT);
    Cparen = make_spe(TCPAREN);
    True = make_spe(TTRUE);

    Env *env = malloc(sizeof(Env));
    env->vars = Nil;
    env->next = NULL;

    define_consts(env, root);
    define_primitives(env, root);

    if (argc < 2) {
        do_repl(env, root);
    }
    else {
        eval_file(env, root, argv[1]);
    }
    return 0;
}

