#include <stdio.h>

int main(void)
{
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
		fscanf(fp, "%i%i\n", &fst[i], &snd[i]);
	}
	fclose(fp);

	int answer = 0;

	for(size_t i = 0; i < lines; i++) {
		for(size_t j = 0; j < lines; j++) {
			if (fst[i] == snd[j]) answer += fst[i];
		}
	}

	printf("%i\n", answer);

	return 0;
}
