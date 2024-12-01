#include <stdio.h>
#include <stdlib.h>

int compare_ints(const void* a, const void* b) {
    int arg1 = *(const int*)a;
    int arg2 = *(const int*)b;

    if (arg1 < arg2) return -1;
    if (arg1 > arg2) return 1;
    return 0;
}

int main(void) {
    FILE *fp = fopen("input", "r");
    if(!fp) {
        printf("YIKES!");
        return -1;
    }

    char cr = 0;
    size_t lines = 0;

    while(cr != EOF) {
        if(cr == '\n') {
            lines++;
        }
        cr = getc(fp);
    }
    rewind(fp);

    int fst[lines];
    int snd[lines];

    for(size_t i = 0; i < lines; i++) {
        fscanf(fp, "%i   %i\n", &fst[i], &snd[i]);
    }
    fclose(fp);

    size_t size = sizeof fst  / sizeof *fst;
    qsort(fst, size, sizeof(int), compare_ints);
    qsort(snd, size, sizeof(int), compare_ints);

    int answer = 0;

    for(size_t i = 0; i < lines; i++) {
        answer += abs(fst[i] - snd[i]);
    }

    printf("%i\n", answer);

    return 0;
    }
