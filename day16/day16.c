#include <stdio.h>

struct state {
    FILE *file;
    unsigned char bits;
    unsigned char bits_len;
    unsigned pos;
};

void init_state(struct state *s) {
    s->bits_len = 0;
    s->pos = 0;
}

unsigned read_bits(unsigned n, struct state *s) {
    unsigned res = 0;
    for (unsigned i = 0; i < n; ++i) {
        if (s->bits_len) {
            --s->bits_len;
        } else {
            fscanf(s->file, "%1hhx", &s->bits);
            s->bits_len = 3;
        }
        res = res << 1 | (s->bits >> s->bits_len & 1);
        ++s->pos;
    }
    return res;
}

unsigned parse_versions(struct state *s) {
    unsigned res = read_bits(3, s);
    unsigned type = read_bits(3, s);
    if (type == 4) {
        while (read_bits(5, s) >> 4);
    } else {
        unsigned length_type = read_bits(1, s);
        if (length_type) {
            unsigned subpacket_count = read_bits(11, s);
            for (unsigned i = 0; i < subpacket_count; ++i) {
                res += parse_versions(s);
            }
        } else {
            unsigned subpackets_length = read_bits(15, s);
            unsigned end_pos = s->pos + subpackets_length;
            while (s->pos < end_pos) {
                res += parse_versions(s);
            }
        }
    }
    return res;
}

unsigned long long
sum(unsigned long long x, unsigned long long y) { return x + y; }
unsigned long long
product(unsigned long long x, unsigned long long y) { return x * y; }
unsigned long long
min(unsigned long long x, unsigned long long y) { return x < y ? x : y; }
unsigned long long
max(unsigned long long x, unsigned long long y) { return x > y ? x : y; }
unsigned long long
gt(unsigned long long x, unsigned long long y) { return x > y; }
unsigned long long
lt(unsigned long long x, unsigned long long y) { return x < y; }
unsigned long long
eq(unsigned long long x, unsigned long long y) { return x == y; }

unsigned long long eval(struct state *s) {
    unsigned long long res;
    read_bits(3, s);
    unsigned type = read_bits(3, s);
    if (type == 4) {
        res = 0;
        unsigned continue_bit;
        do {
            continue_bit = read_bits(1, s);
            res = res << 4 | read_bits(4, s);
        } while (continue_bit);
    } else {
        unsigned long long (*op)(unsigned long long, unsigned long long);
        switch (type) {
            case 0: op = sum; break;
            case 1: op = product; break;
            case 2: op = min; break;
            case 3: op = max; break;
            case 5: op = gt; break;
            case 6: op = lt; break;
            case 7: op = eq; break;
        }
        unsigned length_type = read_bits(1, s);
        if (length_type) {
            unsigned subpacket_count = read_bits(11, s);
            res = eval(s);
            for (unsigned i = 1; i < subpacket_count; ++i) {
                res = op(res, eval(s));
            }
        } else {
            unsigned subpackets_length = read_bits(15, s);
            unsigned end_pos = s->pos + subpackets_length;
            res = eval(s);
            while (s->pos < end_pos) {
                res = op(res, eval(s));
            }
        }
    }
    return res;
}

int main() {
    struct state s;
    s.file = fopen("input.txt", "r");
    init_state(&s);
    printf("%u\n", parse_versions(&s));
    rewind(s.file);
    init_state(&s);
    printf("%llu\n", eval(&s));
    fclose(s.file);
}
