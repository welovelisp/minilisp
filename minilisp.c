#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <alloca.h>
#include <ctype.h>
#include <sys/mman.h>

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

struct Obj;

typedef struct Env {
    struct Obj *vars;
    struct Env *next;
} Env;

typedef struct Obj *Primitive(Env *env, struct Obj **root, struct Obj **args);

struct ObjectHeap;

typedef struct Obj {
    int type;
    struct ObjectHeap *heap;
    union {
        struct Obj *next;
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
            struct Obj *car;
            struct Obj *cdr;
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
            struct Obj *params;
            struct Obj *body;
        };
        // Spe
        struct {
            int spetype;
        };
    };
} Obj;

typedef unsigned char Octet;

typedef struct ObjectHeap {
    Octet *marks;
    Obj *ptr;
} ObjectHeap;

typedef struct ObjectSpace {
  int len;
  int capa;
  ObjectHeap **heaps;
} ObjectSpace;

static Obj *Nil;
static Obj *Dot;
static Obj *Cparen;
static Obj *True;

#define MEMORY_SIZE 1024
#define MAX_HEAPS_SIZE 1024

static ObjectSpace memory;
static Obj *free_list;
static int gc_running = 0;
#define DEBUG_GC 1

void error(char *fmt, ...);
Obj *read(Env *env, Obj **root, char **p);
Obj *read_one(Env *env, Obj **root, char **p);
Obj *make_cell(Env *env, Obj **root, Obj **car, Obj **cdr);
void gc(Env *env, Obj **root);
void print(Obj *obj);
Obj *alloc_heap(size_t size);

#define ADD_ROOT(size)                                          \
    Obj * root_ADD_ROOT_[size+3];                               \
    root_ADD_ROOT_[0] = (Obj *)root;                            \
    root_ADD_ROOT_[1] = (Obj *)__func__;                        \
    root_ADD_ROOT_[size+2] = (Obj *)-1;                         \
    memset(root_ADD_ROOT_ + 2, 0, sizeof(Obj *) * size);        \
    root = root_ADD_ROOT_;                                      \
    int count_ADD_ROOT_ = 2                                     \

#define NEXT_VAR &root[count_ADD_ROOT_++]

Obj *alloc(Env *env, Obj **root, int type) {
    if (!free_list) gc(env, root);
    if (!free_list) {
      free_list = alloc_heap(MEMORY_SIZE);
      if (!free_list) error("memory exhausted");
    }

    Obj *obj = free_list;
    free_list = obj->next;

    obj->type = type;
    return obj;
}

Obj *make_int(Env *env, Obj **root, int value) {
    Obj *r = alloc(env, root, TINT);
    r->value = value;
    return r;
}

Obj *make_string(Env *env, Obj **root, char *body) {
    Obj *str = alloc(env, root, TSTRING);
    str->strbody = strdup(body);
    return str;
}

Obj *make_cell(Env *env, Obj **root, Obj **car, Obj **cdr) {
    Obj *cell = alloc(env, root, TCELL);
    cell->car = *car;
    cell->cdr = *cdr;
    return cell;
}

Obj *find(char *name, Env *env);

Obj *make_symbol(Env *env, Obj **root, char *name) {
    Obj *sym = alloc(env, root, TSYMBOL);
    sym->name = strdup(name);
    return sym;
}

Obj *make_primitive(Env *env, Obj **root, Primitive *fn) {
    Obj *r = alloc(env, root, TPRIMITIVE);
    r->fn = fn;
    return r;
}

Obj *make_function(Env *env, Obj **root, int type, Obj **params, Obj **body) {
    if (type != TFUNCTION && type != TMACRO)
        error("Bug: invalid argument for make_function");
    Obj *r = alloc(env, root, type);
    r->params = *params;
    r->body = *body;
    return r;
}

Obj *make_spe(int spetype) {
    Obj *r = malloc(sizeof(Obj));
    r->type = TSPE;
    r->spetype = spetype;
    return r;
}

void print_cframe(Obj **root) {
    Obj **cframe = root;
    for (;;) {
        if (!*cframe) break;
        Obj **ptr = cframe + 2;
        printf(" %s: ", (char *)cframe[1]);
        for (; *ptr != (Obj *)-1; ptr++) {
            if (*ptr) {
                print(*ptr);
            } else {
                printf("- ");
            }
            printf(" ");
        }
        printf("\n");
        cframe = *(Obj ***)cframe;
    }
}

#define HEAP_OF(obj) (obj->heap)
#define POS_IN_HEAP(obj) (obj - HEAP_OF(obj)->ptr)

#define MARK(obj)   (HEAP_OF(obj)->marks[POS_IN_HEAP(obj) >> 3] |= 1 << (POS_IN_HEAP(obj) & 7))
#define MARKED(obj) (HEAP_OF(obj)->marks[POS_IN_HEAP(obj) >> 3] & (1 << (POS_IN_HEAP(obj) & 7)))

void mark_obj(Obj *obj)
{
    if (obj && obj->type != TSPE && !MARKED(obj)) {
    /*
        if (DEBUG_GC)
            printf("marking %p (type: %d)\n", obj, obj->type);
            */

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
}

void mark_from_env(Env *env, Obj **root)
{
    Env *frame = env;
    for (; frame; frame = frame->next) {
        mark_obj(frame->vars);
    }
}

void mark_from_root(Env *env, Obj **root)
{
    Obj **cframe = root;
    for (; cframe; cframe = (Obj **) cframe[0]) {
        int i = 2;
        for (; cframe[i] != (Obj *) -1; i++) {
            mark_obj(cframe[i]);
        }
    }
}

void mark(Env *env, Obj **root)
{
    int i;
    for (i = 0; i < memory.len; i++) {
        ObjectHeap *heap = memory.heaps[i];
        memset(heap->marks, 0, MEMORY_SIZE / sizeof(Obj) / 8);
    }
    mark_from_env(env, root);
    mark_from_root(env, root);
}

void sweep_obj(Obj *obj)
{
/*
    if (DEBUG_GC)
        printf("sweeping %p (type: %d)\n", obj, obj->type);
        */

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
}

void sweep(Env *env, Obj **root)
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

void gc(Env *env, Obj **root) {
    printf("gc start\n");
    if (gc_running)
        error("Bug: GC is already running");
    gc_running = 1;
    mark(env, root);
    sweep(env, root);
    gc_running = 0;
    printf("gc end\n");
}

void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

Obj *read_sexp(Env *env, Obj **root, char **p) {
    ADD_ROOT(4);
    Obj **obj = NEXT_VAR;
    Obj **head = NEXT_VAR;
    Obj **tail = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
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

Obj *intern(Env *env, Obj **root, char *name) {
    Obj *old = find(name, env);
    if (old) return old->car;
    return make_symbol(env, root, name);
}

Obj *read_quote(Env *env, Obj **root, char **p) {
    ADD_ROOT(4);
    Obj **sym = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *sym = intern(env, root, "quote");
    *tmp = read(env, root, p);
    *tmp = make_cell(env, root, tmp, &Nil);
    *tmp = make_cell(env, root, sym, tmp);
    return *tmp;
}

Obj *read_number(Env *env, Obj **root, char **p, int val) {
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

Obj *read_symbol(Env *env, Obj **root, char **p, char c) {
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

Obj *read_one(Env *env, Obj **root, char **p) {
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

Obj *read(Env *env, Obj **root, char **p) {
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

void print(Obj *obj) {
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

int list_length(Obj *list) {
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

Obj *eval(Env *env, Obj **root, Obj **obj);

void add_var_int(Env *env, Obj **root, Obj **sym, Obj **val) {
    ADD_ROOT(2);
    Obj **cell = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *cell = make_cell(env, root, sym, val);
    *tmp = env->vars;
    *tmp = make_cell(env, root, cell, tmp);
    env->vars = *tmp;
}

void add_env(Env *env, Obj **root,  Env *newenv, Obj **vars, Obj **values) {
    if (list_length(*vars) != list_length(*values))
        error("cannot apply function: number of argument does not match");
    ADD_ROOT(6);
    Obj **p = NEXT_VAR;
    Obj **q = NEXT_VAR;
    Obj **sym = NEXT_VAR;
    Obj **val = NEXT_VAR;
    Obj **def = NEXT_VAR;
    Obj **map = NEXT_VAR;
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

Obj *progn(Env *env, Obj **root, Obj **body) {
    ADD_ROOT(1);
    Obj **car = NEXT_VAR;
    for (;;) {
        *car = (*body)->car;
        if ((*body)->cdr == Nil)
            return eval(env, root, car);
        eval(env, root, car);
        *body = (*body)->cdr;
    }
}

Obj *eval_list(Env *env, Obj **root, Obj **list) {
    ADD_ROOT(4);
    Obj **head = NEXT_VAR;
    Obj **tail = NEXT_VAR;
    Obj **lp = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
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

Obj *apply(Env *env, Obj **root, Obj **fn, Obj **args) {
    if ((*fn)->type == TPRIMITIVE) {
        if ((*args) != Nil && (*args)->type != TCELL)
            error("argument must be a list");
        return (*fn)->fn(env, root, args);
    }
    if ((*fn)->type == TFUNCTION) {
        ADD_ROOT(3);
        Obj **body = NEXT_VAR;
        Obj **params = NEXT_VAR;
        Obj **eargs = NEXT_VAR;
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

Obj *find(char *name, Env *env) {
    for (; env; env = env->next) {
        Obj *cell;
        for (cell = env->vars; cell != Nil; cell = cell->cdr) {
            Obj *var = cell->car;
            char *varname = var->car->name;
            if (strcmp(name, varname) == 0)
                return var;
        }
    }
    return NULL;
}

Obj *macroexpand(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type != TCELL || (*obj)->car->type != TSYMBOL)
        return *obj;
    ADD_ROOT(4);
    Obj **macro = NEXT_VAR;
    Obj **args = NEXT_VAR;
    Obj **body = NEXT_VAR;
    Obj **params = NEXT_VAR;
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

Obj *eval(Env *env, Obj **root, Obj **obj) {
    if ((*obj)->type == TINT || (*obj)->type == TSTRING ||
        (*obj)->type == TPRIMITIVE || (*obj)->type == TFUNCTION ||
        (*obj)->type == TSPE)
        return *obj;
    if ((*obj)->type == TCELL) {
        ADD_ROOT(3);
        Obj **fn = NEXT_VAR;
        Obj **car = NEXT_VAR;
        Obj **args = NEXT_VAR;
        *car = (*obj)->car;
        *args = (*obj)->cdr;
        *fn = eval(env, root, car);
        if ((*fn)->type != TPRIMITIVE && (*fn)->type != TFUNCTION)
            error("Car must be a function");
        return apply(env, root, fn, args);
    }
    if ((*obj)->type == TSYMBOL) {
        Obj *val = find((*obj)->name, env);
        if (!val)
            error("undefined symbol: %s", (*obj)->name);
        return val->cdr;
    }
    error("BUG: eval: Unknown tag type: %d", (*obj)->type);
    return NULL;
}

#define BUFSIZE 250

Obj *prim_quote(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("malformed quote");
    return (*list)->car;
}

Obj *prim_list(Env *env, Obj **root, Obj **list) {
    return eval_list(env, root, list);
}

Obj *prim_car(Env *env, Obj **root, Obj **list) {
    Obj *args = eval_list(env, root, list);
    if (args->car->type != TCELL)
        error("car takes only a cell");
    return args->car->car;
}

Obj *prim_setq(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 ||
        (*list)->car->type != TSYMBOL)
        error("malformed setq");
    ADD_ROOT(2);
    Obj **bind = NEXT_VAR;
    Obj **value = NEXT_VAR;
    *bind = find((*list)->car->name, env);
    if (!*bind)
        error("unbound variable", (*list)->car->name);
    *value = (*list)->cdr->car;
    (*bind)->cdr = eval(env, root, value);
}

Obj *prim_plus(Env *env, Obj **root, Obj **list) {
    ADD_ROOT(1);
    Obj **args = NEXT_VAR;
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

Obj *prim_negate(Env *env, Obj **root, Obj **list) {
    ADD_ROOT(1);
    Obj **args = NEXT_VAR;
    *args = eval_list(env, root, list);
    if ((*args)->car->type != TINT || (*args)->cdr != Nil)
        error("negate takes only one number");
    return make_int(env, root, -(*args)->car->value);
}

Obj *handle_function(Env *env, Obj **root, Obj **list, int type) {
    if ((*list)->type != TCELL || (*list)->car->type != TCELL ||
        (*list)->cdr->type != TCELL) {
        error("malformed lambda");
    }
    Obj *p = (*list)->car;
    for (;;) {
        if (p->car->type != TSYMBOL)
            error("argument must be a symbol");
        if (p->cdr == Nil)
            break;
        if (p->cdr->type != TCELL)
            error("argument is not a flat list");
        p = p->cdr;
    }
    ADD_ROOT(2);
    Obj **car = NEXT_VAR;
    Obj **cdr = NEXT_VAR;
    car = &(*list)->car;
    cdr = &(*list)->cdr;
    return make_function(env, root, type, car, cdr);
}

Obj *prim_lambda(Env *env, Obj **root, Obj **list) {
    handle_function(env, root, list, TFUNCTION);
}

Obj *handle_defun(Env *env, Obj **root, Obj **list, int type) {
    if ((*list)->car->type != TSYMBOL || (*list)->cdr->type != TCELL) {
        error("malformed defun");
    }
    ADD_ROOT(5);
    Obj **fn = NEXT_VAR;
    Obj **var = NEXT_VAR;
    Obj **sym = NEXT_VAR;
    Obj **rest = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *sym = (*list)->car;
    *rest = (*list)->cdr;
    *fn = handle_function(env, root, rest, type);
    add_var_int(env, root, sym, fn);
    return *fn;
}

Obj *prim_defun(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TFUNCTION);
}

Obj *prim_define(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2 ||
        (*list)->car->type != TSYMBOL)
        error("malformed setq");
    ADD_ROOT(2);
    Obj **sym = NEXT_VAR;
    Obj **value = NEXT_VAR;
    *sym = (*list)->car;
    *value = (*list)->cdr->car;
    *value = eval(env, root, value);
    add_var_int(env, root, sym, value);
    return *value;
}

Obj *prim_defmacro(Env *env, Obj **root, Obj **list) {
    return handle_defun(env, root, list, TMACRO);
}

Obj *prim_macroexpand(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 1)
        error("malformed macroexpand");
    ADD_ROOT(1);
    Obj **body = NEXT_VAR;
    *body = (*list)->car;
    return macroexpand(env, root, body);
}

Obj *prim_println(Env *env, Obj **root, Obj **list) {
    ADD_ROOT(1);
    Obj **tmp = NEXT_VAR;
    *tmp = (*list)->car;
    *tmp = eval(env, root, tmp);
    print(*tmp);
    printf("\n");
    return Nil;
}

Obj *prim_if(Env *env, Obj **root, Obj **list) {
    int len = list_length(*list);
    if (len < 2)
        error("malformed if");
    ADD_ROOT(3);
    Obj **cond = NEXT_VAR;
    Obj **then = NEXT_VAR;
    Obj **els = NEXT_VAR;
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

Obj *prim_num_eq(Env *env, Obj **root, Obj **list) {
    if (list_length(*list) != 2)
        error("malformed =");
    ADD_ROOT(1);
    Obj **values = NEXT_VAR;
    *values = eval_list(env, root, list);
    if ((*values)->car->type != TINT || (*values)->cdr->car->type != TINT)
        error("= only takes number");
    return (*values)->car->value == (*values)->cdr->car->value ? True : Nil;
}

Obj *prim_gc(Env *env, Obj **root, Obj **list) {
    gc(env, root);
    return Nil;
}

Obj *prim_exit(Env *env, Obj **root, Obj **list) {
    exit(0);
}

void add_var(Env *env, Obj **root, char *name, Obj **var) {
    ADD_ROOT(3);
    Obj **sym = NEXT_VAR;
    Obj **cell = NEXT_VAR;
    Obj **tmp = NEXT_VAR;
    *sym = intern(env, root, name);
    add_var_int(env, root, sym, var);
}

void add_primitive(Env *env, Obj **root, char *name, Primitive *fn) {
    ADD_ROOT(4);
    Obj **prim = NEXT_VAR;
    *prim = make_primitive(env, root, fn);
    add_var(env, root, name, prim);
}

void define_consts(Env *env, Obj **root) {
    add_var(env, root, "t", &True);
}

void define_primitives(Env *env, Obj **root) {
    add_primitive(env, root, "quote", prim_quote);
    add_primitive(env, root, "list", prim_list);
    add_primitive(env, root, "car", prim_car);
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
    add_primitive(env, root, "println", prim_println);
    add_primitive(env, root, "gc", prim_gc);
    add_primitive(env, root, "exit", prim_exit);
}

Obj *alloc_heap(size_t size)
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

static char buf[BUFSIZE * 100]; /* 100 lines of lisp code */

void do_repl(Env *env, Obj **root)
{
    ADD_ROOT(2);
    Obj **sexp = NEXT_VAR;
    Obj **expanded = NEXT_VAR;

    for (;;) {
        char *p = buf;
        char *dummy = fgets(p, BUFSIZE, stdin);
        *sexp = read(env, root, &p);
        if (!*sexp) continue;
        *expanded = macroexpand(env, root, sexp);
        print(eval(env, root, expanded));
        printf("\n");
    }
}

void eval_file(Env *env, Obj **root, char *fname)
{
    FILE *fp = fopen(fname, "r");
    if (!fp) error("no such file");

    ADD_ROOT(2);
    Obj **sexp = NEXT_VAR;
    Obj **expanded = NEXT_VAR;

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
    Obj **root = NULL;
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
