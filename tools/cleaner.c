#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
	fprintf(stderr, "%s: clean sorted matches of duplicate banks\n\n", argv[0]);
	char line[256] = "";
	char id[15] = "";
	uint64_t size=0;
	char nextid[15] = "";
	uint64_t nextsize=0;
	while(fgets(line, 256, stdin)) {
		strncpy(nextid, line, 14);	
		if(strlen(line) > 14)
			sscanf(line+14, "%lu", &nextsize);
		else {
			fputs("Invalid line read.\n", stderr);
			exit(-1);
		}

		if(!strcmp(id, nextid)) { /* Duplicate ID */
			if(size < nextsize)  /* New size is bigger */
				size = nextsize;
		} else { /* New ID, print finished segment */
			if(size)
				printf("%lu %s\n", size, id);
			strcpy(id, nextid);
			size = nextsize;
		}
	}
	printf("%lu %s\n", nextsize, nextid);
	return 0;
}
