#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>

#include <libmtp.h>
#include <tag_c.h>

static LIBMTP_folder_t *folders;
static LIBMTP_file_t *files;
static LIBMTP_mtpdevice_t *device;
static LIBMTP_track_t *tracks;
static LIBMTP_playlist_t *playlists;

void gettracks(){
	tracks = device? LIBMTP_Get_Tracklisting_With_Callback(device, NULL, NULL) : NULL;
}

int hastrack(){
	return !!tracks;
}

char *curtrack(){
	return tracks->filename;
}

int curtrackn(){
	return tracks->item_id;
}

void nexttrack(){
	LIBMTP_track_t *tmp = tracks;

	tracks = tracks->next;
	LIBMTP_destroy_track_t(tmp);
}

void getplaylists(){
	playlists = device? LIBMTP_Get_Playlist_List(device) : NULL;
}

int hasplaylist(){
	return !!playlists;
}

char *curplaylist(){
	return playlists->name;
}

int curplaylistn(){
	return playlists->playlist_id;
}

void nextplaylist(){
	LIBMTP_playlist_t *tmp = playlists;

	playlists = playlists->next;
	LIBMTP_destroy_playlist_t(tmp);
}

int delete(int id){
	return !LIBMTP_Delete_Object(device, id);
}

void makeplaylist(char *name, uint32_t *ids, int len){
	LIBMTP_playlist_t *pl;
	
	pl = LIBMTP_new_playlist_t();
	pl->name = name;
	pl->no_tracks = len;
	pl->tracks = ids;
	pl->parent_id = pl->storage_id = 0;
	if(LIBMTP_Create_New_Playlist(device, pl)){
		fprintf(stderr, "Couldn't make playlist %s\n", name);
		fflush(stderr);
	}
	pl->tracks = NULL;
	pl->name = NULL;
	LIBMTP_destroy_playlist_t(pl);
}

static int progress (uint64_t sent, uint64_t total, const void *data){
	printf("%llu/%llu (%u%%)\r", (unsigned long long)sent, (unsigned long long)total, (unsigned int)(sent*100/total));
	fflush(stdout);

	return 0;
}

int initmtp(){
	LIBMTP_Init();

	if(!(device = LIBMTP_Get_First_Device())){
		fprintf(stderr, "Couldn't grab a first device\n");
		return 1;
	}

	files = LIBMTP_Get_Filelisting_With_Callback(device, NULL, NULL);
	folders = LIBMTP_Get_Folder_List(device);

	return 0;
}

void endmtp(){
	LIBMTP_Release_Device(device);
	device = NULL;
}

int sendmp3(char *from, char *to){
	char date[80];
	int ret;
	struct stat stats;
	LIBMTP_track_t *trackmeta;
	TagLib_File *tlfile;
	TagLib_Tag *tltag;

	if(stat(from, &stats) == -1){
		fprintf(stderr, "%s: ", from);
		perror("stat");
	}else if(S_ISREG(stats.st_mode)){
		trackmeta = LIBMTP_new_track_t();

		trackmeta->filesize = stats.st_size;
		trackmeta->filename = to;
		trackmeta->filetype = LIBMTP_FILETYPE_MP3;
		trackmeta->parent_id = device->default_music_folder;

		if((tlfile = taglib_file_new(from)) && (tltag = taglib_file_tag(tlfile))){
			trackmeta->title = taglib_tag_title(tltag);
			trackmeta->album = taglib_tag_album(tltag);
			trackmeta->artist = taglib_tag_artist(tltag);
			snprintf(trackmeta->date = date, sizeof date, "%4d0101T0000.0", taglib_tag_year(tltag));
			trackmeta->tracknumber = taglib_tag_track(tltag);
		}else
			fprintf(stderr, "Note: Missing tags in %s\n", from);
/*			
		printf(
		  "  title : %s\n"
		  "  album : %s\n"
		  "  artist: %s\n"
		  "  date  : %s\n"
		  "  track : %i\n", trackmeta->title, trackmeta->album, trackmeta->artist, trackmeta->date, trackmeta->tracknumber);
		printf("Sending track...\n"); */

		ret = LIBMTP_Send_Track_From_File(device, from, trackmeta, progress, NULL);
		printf("\n");
		if(ret){
			fprintf(stderr, "Error sending track.\n");
			LIBMTP_Dump_Errorstack(device);
			LIBMTP_Clear_Errorstack(device);
		}

		trackmeta->filename = trackmeta->title = trackmeta->album = trackmeta->artist = trackmeta->date = NULL;
		LIBMTP_destroy_track_t(trackmeta);

		taglib_tag_free_strings();
		taglib_file_free(tlfile);

		return 1;
	}
	return 0;
}
