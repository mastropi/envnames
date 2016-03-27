#include <stdio.h>		// printf
#include <stdlib.h>		// malloc
#include <string.h>		// strcpy
//#include <Defn.h>
//#include <Internal.h>
#include <R_ext/Print.h>

// Declare functions used
int show(int);
static void address(char*);

static void address(char* x) {
	printf("<%p>", (void *)x);
}

int main(int argc, char *argv[]) {
	int y = show(3);
	char* z = (char*) malloc(sizeof(char) * 20);
	strcpy(z, "this is a test");
	char w[30];
	snprintf(w, sizeof(w), "<%p>", z);
	printf("%s", w);
	address(z);
	address(w);
	free(z);
	return 0;
}

int show(int x) {
	return x + 1;
}
