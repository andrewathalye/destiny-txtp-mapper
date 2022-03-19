#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>

#define VGMSTREAM123_PATH "~/bin/vgmstream123"
#define VGMSTREAMCLI_PATH "~/bin/vgmstream-cli"

#define FLAG_IDENTIFY 0b1
#define FLAG_CONFIRM 0b10
#define FLAG_APPROX 0b100
#define FLAG_QUIET 0b1000
uint64_t FLAGS = 0;
extern int optind;

void export(char * entryid, char * entryname) {
        printf("[Todo] Export %s (%s)\n", entryname, entryid);
}

void playconfirm(char * entryid, char * entryname) {
	printf("[Info] Confirm %s (%s)\n", entryname, entryid);
	/* Replace processed name with true name */
	char entryid_tmp[256];
	for(int i=0; i < strlen(entryid); i++)
		entryid_tmp[i] = (entryid[i] == '_') ? ' ' : entryid[i];
	char buf[256];
	snprintf(buf, 256, "%s txtp/\"%s\".txtp >/dev/null", VGMSTREAM123_PATH, entryid_tmp); 
	/* Potential loop */
	char response = '?';
	while(response == '?') {
		system(buf); /* Play txtp */
		fputs("[Interactive] Is this title correct? (y/n/?) ", stdout);
		response=fgetc(stdin);
		char c;
		while((c = fgetc(stdin)) != '\n' && c != EOF) {} /* Clear buffer */
		if(response == 'y')
			strcpy(buf, entryname);
		else if(response == 'n') {
			fputs("[Interactive] Please input the title: ", stdout);
			fgets(buf, 256, stdin);
			if(strlen(buf)>1)
				buf[strlen(buf)-1] = 0;
		}
		else /* Default if invalid input */
			response = '?';
	}
	fprintf(stderr, ". %s %s\n", entryid, buf);
}

void playidentify(char * entryid, char * entryname) {
	printf("[Info] Identify %s (%s)\n", entryname, entryid);
	DIR * txtpdir;
	if(!(txtpdir = opendir("txtp"))) {
		fprintf(stderr, "[Error] Unable to open txtp directory.\n");
		exit(-1);
	}

	struct dirent * txtpdirent;
	char cmd_buf[256];
	char txtpdirent_buf[256];
	char response;
	while(txtpdirent = readdir(txtpdir)) {
		strncpy(txtpdirent_buf, txtpdirent->d_name, 256);
		txtpdirent_buf[14]=0; /* Shorten file name to test whether it matches bank name */
		if(!strcmp(txtpdirent_buf, entryid)) {
			printf("[Info] Found %s\n", txtpdirent->d_name);
			snprintf(cmd_buf, 256, "%s -m txtp/\"%s\" | tail -n1", VGMSTREAMCLI_PATH, txtpdirent->d_name);
			system(cmd_buf); /* List length of txtp */
			snprintf(cmd_buf, 256, "%s txtp/\"%s\" >/dev/null", VGMSTREAM123_PATH, txtpdirent->d_name);
			response = '?';
			while(response == '?') {
				system(cmd_buf); /* Play txtp */
				fputs("[Interactive] Keep piece? (y/n/?) ", stdout);
				response=fgetc(stdin);
				char c;
				while((c = fgetc(stdin)) != '\n' && c != EOF) {} /* Clear buffer */
				if(response == 'y') {
					for(int i=0; i<256 && i<strlen(txtpdirent->d_name); i++) {
						txtpdirent_buf[i] = (txtpdirent->d_name[i] == ' ') ? '_' : txtpdirent->d_name[i];
						if(txtpdirent_buf[i] == '.') { /* Omit extension */
							txtpdirent_buf[i] = 0;
							break;
						}
					}
					txtpdirent_buf[255]=0; /* Prevent printing boundless string */
					fprintf(stderr, "! %s UNCONFIRMED\n", txtpdirent_buf);
				}
			}
		}
	}
	closedir(txtpdir);
}

int main(int argc, char *argv[]) { 
	puts("txtp renamer tool v0.1\n");
	const char * usage = "Usage: %s [options] [input] [outputdir])\n -i: Identify mode. Entries marked with + will be played for you to identify. No output files will be produced.\n -c: Confirm mode. Entries marked with ! will be played for you to identify. No output files will be produced.\n -y: Approximate mode. Generate output for entries marked with !. Must not be paired with -i or -c.\n -q: Quiet mode. Do not generate any warnings for unconfirmed or unidentified entries.\n";

	{
		int opt;
		while((opt=getopt(argc, argv, "icyq")) != -1) {
			switch(opt) {
				case 'i':
					FLAGS |= FLAG_IDENTIFY;
					break;
				case 'c':
					FLAGS |= FLAG_CONFIRM;
					break;
				case 'y':
					FLAGS |= FLAG_APPROX;
					break;
				case 'q':
					FLAGS |= FLAG_QUIET;
					break;
				case '?':
					printf(usage, argv[0]);
					exit(-1);
					break;
			}
		}
	}

	/* Check for invalid argument combinations or too few arguments*/
	if(argc < (optind + 2) || ((FLAGS & FLAG_APPROX) && ((FLAGS & FLAG_IDENTIFY) | (FLAGS & FLAG_CONFIRM)))) {
		printf(usage, argv[0]);
		exit(-1);
	}

	char * filename = argv[optind];
	char * outputdir = argv[optind + 1];
	#ifdef DEBUG
	printf("[Debug] Flags: %lu\n", FLAGS);
	printf("[Debug] Infile, outdir: %s,%s\n", filename, outputdir);
	fflush(stdout);
	#endif

	FILE * file;
	if(!(file=fopen(filename, "r"))) {
		fprintf(stderr, "[Error] Unable to open file.\n");
		exit(-1);
	}

	/* Process entries */
	char entry[256];
	char entrytmp[256]; /* Used for strtok manipulation */

	char entrytype;
	char * entryid;
	char * entryname;
	char * saveptr = NULL;
	while(fgets(entry, 256, file)) {
		if(strlen(entry)>1)
			entry[strlen(entry)-1] = 0; /* Remove \n */

		#ifdef DEBUG
		printf("[Debug] %s\n", entry); /* Print current entry being processed */
		#endif

		strcpy(entrytmp, entry);
		entrytype = strtok(entrytmp, " ")[0]; 
		entryid = strtok(NULL, " ");
		entryname = "Name";
		if(entryid)
			entryname = entry + 3 + strlen(entryid); /* Offset of name. One byte for entrytype and two for spaces */
		if((entrytype != '\n') && !(entrytype | (uintptr_t) entryid | (uintptr_t) entryname)) {
			fclose(file);
			fprintf(stderr, "[Error] Malformed entry data.\n");
			exit(-1);
		}

		switch(entrytype) {
			case '!': /* Approximate entry */
				if(FLAGS & FLAG_APPROX)
					export(entryid, entryname);
				else if(FLAGS & FLAG_CONFIRM)
					playconfirm(entryid, entryname);
				else if(!(FLAGS & FLAG_QUIET))
					printf("[Warn] Unconfirmed %s (%s)\n", entryname, entryid);
				break;
			case '+': /* Unidentified entry */
				if(FLAGS & FLAG_IDENTIFY)
					playidentify(entryid, entryname);
				else if(!(FLAGS & FLAG_QUIET))
					printf("[Warn] Unidentified %s (%s)\n", entryname, entryid);
				break;
			case '.': /* Confirmed entry */
				if(!((FLAGS & FLAG_IDENTIFY)|(FLAGS & FLAG_CONFIRM))) export(entryid, entryname);
				break;
			case '\n': /* Skip blank lines */
				break;
			default:
				fprintf(stderr, "[Error] Unknown entry type.\n");
				fclose(file);
				exit(-1);
				break;
		}
	}
	fclose(file);
	return 0;
}
