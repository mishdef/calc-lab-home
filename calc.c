// Mykhailo Stdanikov 260ADB168
// (optional) https://github.com/mishdef/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: K&R chapter on recursive descent, RTU lab rules

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>

// platform folder creation: windows has _mkdir(path), linux has mkdir(path, mode)
#ifdef _WIN32
    #include <direct.h>
    #define make_dir_platform(p) _mkdir(p)
#else
    #define make_dir_platform(p) mkdir(p, 0777)
#endif

// student info used for default directory and file names
#define STUDENT_FIRST_NAME "mykhailo"
#define STUDENT_LAST_NAME  "stdanikov"
#define STUDENT_ID         "260adb168"

// type of arithmetic value: 64-bit integer or double float
typedef enum {
    NUM_INT,
    NUM_FLOAT
} NumType;

// holds either a 64-bit int or double
typedef struct {
    NumType type;
    long long ival;
    double fval;
} Number;

// zero number constant
static const Number num_zero = {NUM_INT, 0, 0.0};

// token types produced by the scanner
typedef enum {
    TK_INT,        // integer literal
    TK_FLOAT,      // float literal
    TK_ADD,        // '+'
    TK_SUB,        // '-'
    TK_MUL,        // '*'
    TK_DIV,        // '/'
    TK_POW,        // '**'
    TK_LPAREN,     // '('
    TK_RPAREN,     // ')'
    TK_EOL,        // end of line or EOF
    TK_ERR         // invalid character or bad token
} TokenKind;

// single token item with 1-based global position
typedef struct {
    TokenKind kind;
    int offset;    // 1-based character position in whole file
    Number num;    // numeric value if token is number
} LexItem;

// parser state for the active line
static LexItem* items = NULL; // dynamic array of tokens
static int item_count = 0;    // number of tokens
static int cursor = 0;        // current token index
static int fail_at = -1;      // error position (-1 if none)

// recursive descent grammar functions
static Number eval_expr(void);
static Number eval_term(void);
static Number eval_power(void);
static Number eval_factor(void);

// factor := NUMBER | '(' expr ')'
static Number eval_factor(void) {
    // stop if an error was already found
    if (fail_at != -1) return num_zero;

    LexItem current = items[cursor];

    // integer or float literal
    if (current.kind == TK_INT || current.kind == TK_FLOAT) {
        cursor++; // consume number
        return current.num;
    }

    // subexpression in parentheses
    if (current.kind == TK_LPAREN) {
        cursor++; // consume '('
        Number res = eval_expr();
        if (fail_at != -1) return num_zero;

        // expect closing ')'
        if (items[cursor].kind == TK_RPAREN) {
            cursor++; // consume ')'
            return res;
        } else {
            // missing ')': report error at line's \n
            fail_at = items[cursor].offset;
            return num_zero;
        }
    }

    // unexpected token (e.g. unary minus "-5", empty "()", or extra operators)
    fail_at = current.offset;
    return num_zero;
}

// power := factor [ '**' power ]
// right-associative: 2**3**2 == 2**(3**2) == 512
static Number eval_power(void) {
    if (fail_at != -1) return num_zero;

    // evaluate base factor
    Number lhs = eval_factor();
    if (fail_at != -1) return num_zero;

    // check for '**'
    if (items[cursor].kind == TK_POW) {
        int op_pos = items[cursor].offset; // '**' position for float overflow error
        cursor++; // consume '**'
        int rhs_start_pos = items[cursor].offset; // start position of exponent

        // recursive call on the right side makes power right-associative
        Number rhs = eval_power();
        if (fail_at != -1) return num_zero;

        // integer power
        if (lhs.type == NUM_INT && rhs.type == NUM_INT) {
            // negative int exponent is an error at start of exponent
            if (rhs.ival < 0) {
                fail_at = rhs_start_pos;
                return num_zero;
            }
            // x ** 0 is always 1 (including 0 ** 0)
            if (rhs.ival == 0) {
                lhs.ival = 1;
            } else {
                // fast exponentiation by squaring
                long long base = lhs.ival;
                long long exp = rhs.ival;
                long long acc = 1;
                while (exp > 0) {
                    if (exp & 1) acc *= base;
                    base *= base;
                    exp >>= 1;
                }
                lhs.ival = acc;
            }
        } else {
            // float power using standard pow()
            double d_lhs = (lhs.type == NUM_INT) ? (double)lhs.ival : lhs.fval;
            double d_rhs = (rhs.type == NUM_INT) ? (double)rhs.ival : rhs.fval;
            double res = pow(d_lhs, d_rhs);

            // overflow or NaN reported at '**' operator
            if (isnan(res) || isinf(res)) {
                fail_at = op_pos;
                return num_zero;
            }
            lhs.type = NUM_FLOAT;
            lhs.fval = res;
        }
    }

    return lhs;
}

// term := power { ('*' | '/') power }
// left-associative: evaluated left to right in a loop
static Number eval_term(void) {
    if (fail_at != -1) return num_zero;

    Number lhs = eval_power();
    if (fail_at != -1) return num_zero;

    // loop over '*' and '/'
    while (items[cursor].kind == TK_MUL || items[cursor].kind == TK_DIV) {
        TokenKind op = items[cursor].kind;
        int op_pos = items[cursor].offset;
        cursor++; // consume '*' or '/'
        int rhs_start_pos = items[cursor].offset; // start of divisor

        Number rhs = eval_power();
        if (fail_at != -1) return num_zero;

        if (op == TK_DIV) {
            // check division by zero for int and float
            int is_zero = 0;
            if (rhs.type == NUM_INT && rhs.ival == 0) is_zero = 1;
            if (rhs.type == NUM_FLOAT && rhs.fval == 0.0) is_zero = 1;
            if (is_zero) {
                // error position is at start of divisor
                fail_at = rhs_start_pos;
                return num_zero;
            }

            // integer division truncates towards zero
            if (lhs.type == NUM_INT && rhs.type == NUM_INT) {
                lhs.ival = lhs.ival / rhs.ival;
            } else {
                // promote to float
                double d_lhs = (lhs.type == NUM_INT) ? (double)lhs.ival : lhs.fval;
                double d_rhs = (rhs.type == NUM_INT) ? (double)rhs.ival : rhs.fval;
                double res = d_lhs / d_rhs;
                if (isnan(res) || isinf(res)) {
                    fail_at = op_pos;
                    return num_zero;
                }
                lhs.type = NUM_FLOAT;
                lhs.fval = res;
            }
        } else { // TK_MUL
            if (lhs.type == NUM_INT && rhs.type == NUM_INT) {
                lhs.ival = lhs.ival * rhs.ival;
            } else {
                double d_lhs = (lhs.type == NUM_INT) ? (double)lhs.ival : lhs.fval;
                double d_rhs = (rhs.type == NUM_INT) ? (double)rhs.ival : rhs.fval;
                double res = d_lhs * d_rhs;
                if (isnan(res) || isinf(res)) {
                    fail_at = op_pos;
                    return num_zero;
                }
                lhs.type = NUM_FLOAT;
                lhs.fval = res;
            }
        }
    }

    return lhs;
}

// expr := term { ('+' | '-') term }
// left-associative: evaluated left to right in a loop
static Number eval_expr(void) {
    if (fail_at != -1) return num_zero;

    Number lhs = eval_term();
    if (fail_at != -1) return num_zero;

    // loop over '+' and '-'
    while (items[cursor].kind == TK_ADD || items[cursor].kind == TK_SUB) {
        TokenKind op = items[cursor].kind;
        int op_pos = items[cursor].offset;
        cursor++; // consume '+' or '-'

        Number rhs = eval_term();
        if (fail_at != -1) return num_zero;

        if (op == TK_ADD) {
            if (lhs.type == NUM_INT && rhs.type == NUM_INT) {
                lhs.ival = lhs.ival + rhs.ival;
            } else {
                double d_lhs = (lhs.type == NUM_INT) ? (double)lhs.ival : lhs.fval;
                double d_rhs = (rhs.type == NUM_INT) ? (double)rhs.ival : rhs.fval;
                double res = d_lhs + d_rhs;
                if (isnan(res) || isinf(res)) {
                    fail_at = op_pos;
                    return num_zero;
                }
                lhs.type = NUM_FLOAT;
                lhs.fval = res;
            }
        } else { // TK_SUB
            if (lhs.type == NUM_INT && rhs.type == NUM_INT) {
                lhs.ival = lhs.ival - rhs.ival;
            } else {
                double d_lhs = (lhs.type == NUM_INT) ? (double)lhs.ival : lhs.fval;
                double d_rhs = (rhs.type == NUM_INT) ? (double)rhs.ival : rhs.fval;
                double res = d_lhs - d_rhs;
                if (isnan(res) || isinf(res)) {
                    fail_at = op_pos;
                    return num_zero;
                }
                lhs.type = NUM_FLOAT;
                lhs.fval = res;
            }
        }
    }

    return lhs;
}

// push token to array, doubling capacity when full
static void push_item(LexItem** arr, int* count, int* cap, TokenKind kind, int offset, Number num) {
    if (*count >= *cap) {
        *cap = (*cap == 0) ? 32 : (*cap * 2);
        *arr = (LexItem*)realloc(*arr, (size_t)(*cap) * sizeof(LexItem));
    }
    (*arr)[*count].kind = kind;
    (*arr)[*count].offset = offset;
    (*arr)[*count].num = num;
    (*count)++;
}

// scan one line into tokens and evaluate it
// global 1-based character position is (ptr - doc_text) + 1
static void parse_line(const char* doc_text, size_t doc_len,
                       const char* line_beg, const char* line_fin,
                       Number* out_num, int* out_err) {
    items = NULL;
    item_count = 0;
    int cap = 0;

    const char* ptr = line_beg;
    while (ptr < line_fin) {
        // skip whitespace
        if (*ptr == ' ' || *ptr == '\t' || *ptr == '\r') {
            ptr++;
            continue;
        }

        // character position in the file
        int pos = (int)(ptr - doc_text) + 1;

        // '#' in middle of expression is invalid
        if (*ptr == '#') {
            push_item(&items, &item_count, &cap, TK_ERR, pos, num_zero);
            ptr++;
            continue;
        }

        // scan numbers
        if (isdigit((unsigned char)*ptr)) {
            const char* digit_start = ptr;
            // scan integer part
            while (ptr < line_fin && isdigit((unsigned char)*ptr)) ptr++;

            // check if float dot follows
            if (ptr < line_fin && *ptr == '.') {
                if (ptr + 1 < line_fin && isdigit((unsigned char)*(ptr + 1))) {
                    ptr++; // skip dot
                    // scan fractional digits
                    while (ptr < line_fin && isdigit((unsigned char)*ptr)) ptr++;

                    char buf[128];
                    size_t len = (size_t)(ptr - digit_start);
                    if (len >= sizeof(buf)) len = sizeof(buf) - 1;
                    memcpy(buf, digit_start, len);
                    buf[len] = '\0';

                    Number n;
                    n.type = NUM_FLOAT;
                    n.fval = strtod(buf, NULL);
                    n.ival = 0;
                    push_item(&items, &item_count, &cap, TK_FLOAT, pos, n);
                } else {
                    // e.g. "2." -> 2 is int, dot left for next loop iteration
                    char buf[128];
                    size_t len = (size_t)(ptr - digit_start);
                    if (len >= sizeof(buf)) len = sizeof(buf) - 1;
                    memcpy(buf, digit_start, len);
                    buf[len] = '\0';

                    Number n;
                    n.type = NUM_INT;
                    n.ival = strtoll(buf, NULL, 10);
                    n.fval = 0;
                    push_item(&items, &item_count, &cap, TK_INT, pos, n);
                }
            } else {
                // regular integer
                char buf[128];
                size_t len = (size_t)(ptr - digit_start);
                if (len >= sizeof(buf)) len = sizeof(buf) - 1;
                memcpy(buf, digit_start, len);
                buf[len] = '\0';

                Number n;
                n.type = NUM_INT;
                n.ival = strtoll(buf, NULL, 10);
                n.fval = 0;
                push_item(&items, &item_count, &cap, TK_INT, pos, n);
            }
            continue;
        }

        // leading dot like ".5" is invalid
        if (*ptr == '.') {
            push_item(&items, &item_count, &cap, TK_ERR, pos, num_zero);
            ptr++;
            continue;
        }

        // arithmetic symbols
        if (*ptr == '+') { push_item(&items, &item_count, &cap, TK_ADD, pos, num_zero); ptr++; continue; }
        if (*ptr == '-') { push_item(&items, &item_count, &cap, TK_SUB, pos, num_zero); ptr++; continue; }
        if (*ptr == '*') {
            // check for '**'
            if (ptr + 1 < line_fin && *(ptr + 1) == '*') {
                push_item(&items, &item_count, &cap, TK_POW, pos, num_zero);
                ptr += 2;
            } else {
                push_item(&items, &item_count, &cap, TK_MUL, pos, num_zero);
                ptr++;
            }
            continue;
        }
        if (*ptr == '/') { push_item(&items, &item_count, &cap, TK_DIV, pos, num_zero); ptr++; continue; }
        if (*ptr == '(') { push_item(&items, &item_count, &cap, TK_LPAREN, pos, num_zero); ptr++; continue; }
        if (*ptr == ')') { push_item(&items, &item_count, &cap, TK_RPAREN, pos, num_zero); ptr++; continue; }

        // any other unknown char
        push_item(&items, &item_count, &cap, TK_ERR, pos, num_zero);
        ptr++;
    }

    // append end of line token
    int eol_pos;
    if (*line_fin == '\n') {
        eol_pos = (int)(line_fin - doc_text) + 1;
    } else {
        eol_pos = (int)doc_len + 1;
    }
    push_item(&items, &item_count, &cap, TK_EOL, eol_pos, num_zero);

    cursor = 0;
    fail_at = -1;

    // run the parser
    Number res = eval_expr();

    // check if parsing failed
    if (fail_at != -1) {
        *out_err = fail_at;
        free(items);
        items = NULL;
        return;
    }

    // check if unparsed tokens remain at end
    if (items[cursor].kind != TK_EOL) {
        *out_err = items[cursor].offset;
        free(items);
        items = NULL;
        return;
    }

    // success
    *out_err = -1;
    *out_num = res;
    free(items);
    items = NULL;
}

// read entire file into dynamic buffer
static char* read_entire_file(const char* filepath, size_t* out_len) {
    FILE* fp = fopen(filepath, "rb");
    if (!fp) return NULL;

    size_t capacity = 4096;
    size_t size = 0;
    char* buf = (char*)malloc(capacity);
    if (!buf) {
        fclose(fp);
        return NULL;
    }

    // read chunks until EOF
    while (1) {
        size_t n = fread(buf + size, 1, capacity - size - 1, fp);
        if (n == 0) {
            if (ferror(fp)) {
                free(buf);
                fclose(fp);
                return NULL;
            }
            break;
        }
        size += n;

        // grow buffer when full
        if (size >= capacity - 1) {
            size_t new_cap = capacity * 2;
            char* new_buf = (char*)realloc(buf, new_cap);
            if (!new_buf) {
                free(buf);
                fclose(fp);
                return NULL;
            }
            buf = new_buf;
            capacity = new_cap;
        }
    }

    buf[size] = '\0';
    fclose(fp);
    if (out_len) *out_len = size;
    return buf;
}

// evaluate file and write output
static int process_single_file(const char* src_file, const char* dst_file) {
    size_t text_len = 0;
    char* text = read_entire_file(src_file, &text_len);
    if (!text) {
        fprintf(stderr, "Error: cannot read file '%s': %s\n", src_file, strerror(errno));
        return 1;
    }

    char** out_lines = NULL;
    size_t line_cnt = 0;
    size_t line_cap = 0;
    int first_err = -1;

    // iterate through lines without copying
    const char* scan = text;
    while (*scan != '\0') {
        const char* l_start = scan;
        const char* l_end = strchr(scan, '\n');
        if (!l_end) {
            l_end = scan + strlen(scan);
        }

        // check if line has non-whitespace
        int blank = 1;
        const char* first_ch = NULL;
        for (const char* p = l_start; p < l_end; ++p) {
            if (!isspace((unsigned char)*p)) {
                if (!first_ch) first_ch = p;
                blank = 0;
            }
        }

        if (!blank) {
            // line starting with # is a comment
            if (first_ch && *first_ch == '#') {
                // skip comment
            } else {
                Number n = num_zero;
                int err = -1;
                parse_line(text, text_len, l_start, l_end, &n, &err);

                if (err != -1) {
                    first_err = err;
                    break; // stop on first error in file
                } else {
                    char row[64];
                    if (n.type == NUM_INT) {
                        snprintf(row, sizeof(row), "%lld\n", n.ival);
                    } else {
                        snprintf(row, sizeof(row), "%.15g\n", n.fval);
                    }

                    // append to line list
                    if (line_cnt >= line_cap) {
                        line_cap = (line_cap == 0) ? 16 : (line_cap * 2);
                        out_lines = (char**)realloc(out_lines, line_cap * sizeof(char*));
                    }
                    out_lines[line_cnt] = (char*)malloc(strlen(row) + 1);
                    strcpy(out_lines[line_cnt], row);
                    line_cnt++;
                }
            }
        }

        // advance to next line
        if (*l_end == '\n') {
            scan = l_end + 1;
        } else {
            scan = l_end;
        }
    }

    // binary mode "wb" to write standard '\n' (LF)
    FILE* out = fopen(dst_file, "wb");
    if (!out) {
        fprintf(stderr, "Error: cannot open output file '%s': %s\n", dst_file, strerror(errno));
        free(text);
        for (size_t i = 0; i < line_cnt; ++i) free(out_lines[i]);
        free(out_lines);
        return 1;
    }

    // write error or all computed lines
    if (first_err != -1) {
        fprintf(out, "ERROR:%d\n", first_err);
    } else {
        for (size_t i = 0; i < line_cnt; ++i) {
            fputs(out_lines[i], out);
        }
    }

    // cleanup
    fclose(out);
    free(text);
    for (size_t i = 0; i < line_cnt; ++i) free(out_lines[i]);
    free(out_lines);
    return 0;
}

// extract file stem name without directory and without .txt
static char* extract_stem_name(const char* filepath) {
    size_t len = strlen(filepath);

    // trim trailing slashes
    while (len > 0 && (filepath[len - 1] == '/' || filepath[len - 1] == '\\')) {
        len--;
    }
    if (len == 0) {
        char* d = (char*)malloc(2);
        strcpy(d, ".");
        return d;
    }

    // find last slash
    const char* p = filepath + len - 1;
    while (p >= filepath && *p != '/' && *p != '\\') {
        p--;
    }
    const char* stem_start = p + 1;
    size_t stem_len = (size_t)(filepath + len - stem_start);

    // trim .txt if present
    if (stem_len > 4 && strncmp(stem_start + stem_len - 4, ".txt", 4) == 0) {
        stem_len -= 4;
    }

    char* res = (char*)malloc(stem_len + 1);
    if (!res) return NULL;
    strncpy(res, stem_start, stem_len);
    res[stem_len] = '\0';
    return res;
}

int main(int argc, char** argv) {
    // at least one parameter required
    if (argc < 2) {
        fprintf(stderr, "Usage: %s [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n", argv[0]);
        return 1;
    }

    const char* target_dir = NULL;
    const char* dest_dir = NULL;
    const char* target_file = NULL;

    // parse flags
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (++i < argc) {
                target_dir = argv[i];
            } else {
                fprintf(stderr, "Error: missing argument after '%s'\n", argv[i - 1]);
                return 1;
            }
        } else if (strncmp(argv[i], "--dir=", 6) == 0) {
            target_dir = argv[i] + 6;
        } else if (strcmp(argv[i], "-o") == 0 || strcmp(argv[i], "--output-dir") == 0) {
            if (++i < argc) {
                dest_dir = argv[i];
            } else {
                fprintf(stderr, "Error: missing argument after '%s'\n", argv[i - 1]);
                return 1;
            }
        } else if (strncmp(argv[i], "--output-dir=", 13) == 0) {
            dest_dir = argv[i] + 13;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Error: unknown option '%s'\n", argv[i]);
            return 1;
        } else {
            // positional input file
            if (target_file == NULL) {
                target_file = argv[i];
            } else {
                fprintf(stderr, "Error: unexpected argument '%s'\n", argv[i]);
                return 1;
            }
        }
    }

    // check if input source was given
    if (target_dir == NULL && target_file == NULL) {
        fprintf(stderr, "Error: no input file or directory specified\n");
        return 1;
    }

    // default folder if -o not given: <stem>_<name>_<id>
    char fallback_folder[512];
    char* stem = NULL;
    if (dest_dir == NULL) {
        if (target_dir != NULL) {
            stem = extract_stem_name(target_dir);
        } else {
            stem = extract_stem_name(target_file);
        }
        snprintf(fallback_folder, sizeof(fallback_folder), "%s_%s_%s",
                 stem, STUDENT_FIRST_NAME, STUDENT_ID);
        dest_dir = fallback_folder;
    }

    // create directory if it does not exist
    struct stat out_st;
    if (stat(dest_dir, &out_st) != 0) {
        if (make_dir_platform(dest_dir) != 0 && errno != EEXIST) {
            fprintf(stderr, "Error: cannot create output directory '%s': %s\n", dest_dir, strerror(errno));
            if (stem) free(stem);
            return 1;
        }
    } else if (!S_ISDIR(out_st.st_mode)) {
        fprintf(stderr, "Error: '%s' exists and is not a directory\n", dest_dir);
        if (stem) free(stem);
        return 1;
    }

    int exit_code = 0;

    // directory mode (-d)
    if (target_dir != NULL) {
        struct stat st;
        if (stat(target_dir, &st) != 0 || !S_ISDIR(st.st_mode)) {
            fprintf(stderr, "Error: directory '%s' does not exist or is not a directory\n", target_dir);
            if (stem) free(stem);
            return 1;
        }

        DIR* d = opendir(target_dir);
        if (!d) {
            fprintf(stderr, "Error: cannot open directory '%s': %s\n", target_dir, strerror(errno));
            if (stem) free(stem);
            return 1;
        }

        struct dirent* entry;
        while ((entry = readdir(d)) != NULL) {
            size_t name_len = strlen(entry->d_name);
            // only *.txt
            if (name_len > 4 && strcmp(entry->d_name + name_len - 4, ".txt") == 0) {
                char in_path[1024];
                snprintf(in_path, sizeof(in_path), "%s/%s", target_dir, entry->d_name);

                // ignore subdirectories
                struct stat file_st;
                if (stat(in_path, &file_st) == 0 && S_ISREG(file_st.st_mode)) {
                    char* file_stem = extract_stem_name(entry->d_name);
                    char out_path[1024];
                    // <dest_dir>/<file_stem>_<name>_<lastname>_<id>.txt
                    snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s_%s.txt",
                             dest_dir, file_stem, STUDENT_FIRST_NAME, STUDENT_LAST_NAME, STUDENT_ID);
                    free(file_stem);

                    process_single_file(in_path, out_path);
                }
            }
        }
        closedir(d);
    } else {
        // single file mode
        struct stat st;
        if (stat(target_file, &st) != 0 || !S_ISREG(st.st_mode)) {
            fprintf(stderr, "Error: cannot open input file '%s'\n", target_file);
            if (stem) free(stem);
            return 1;
        }

        char* file_stem = extract_stem_name(target_file);
        char out_path[1024];
        // <dest_dir>/<file_stem>_<name>_<lastname>_<id>.txt
        snprintf(out_path, sizeof(out_path), "%s/%s_%s_%s_%s.txt",
                 dest_dir, file_stem, STUDENT_FIRST_NAME, STUDENT_LAST_NAME, STUDENT_ID);
        free(file_stem);

        process_single_file(target_file, out_path);
    }

    if (stem) free(stem);
    return exit_code;
}
