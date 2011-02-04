/** 
 * \file sendtr.c
 * Example program to send a music track to a device.
 * This program is derived from the exact equivalent in libnjb.
 * based on Enrique Jorreto Ledesma's work on the original program by 
 * Shaun Jackman and Linus Walleij.
 *
 * Copyright (C) 2003-2009 Linus Walleij <triad@df.lth.se>
 * Copyright (C) 2003-2005 Shaun Jackman
 * Copyright (C) 2003-2005 Enrique Jorrete Ledesma
 * Copyright (C) 2006 Chris A. Debenham <chris@adebenham.com>
 * Copyright (C) 2008 Nicolas Pennequin <nicolas.pennequin@free.fr>
 * Copyright (C) 2008 Joseph Nahmias <joe@nahmias.net>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

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

int delete(int id){
	return !LIBMTP_Delete_Object(device, id);
}

static int progress (const uint64_t sent, const uint64_t total, void const * const data){
	int percent = (sent*100)/total;
#ifdef __WIN32__
	printf("Progress: %I64u of %I64u (%d%%)\r", sent, total, percent);
#else
	printf("Progress: %llu of %llu (%d%%)\r", sent, total, percent);
#endif
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
	char tmp[80];
	unsigned long long filesize;
	unsigned long parent_id = 0;
	int ret, rc;
	struct stat sb;
	LIBMTP_track_t *trackmeta;
	LIBMTP_album_t *albuminfo;
	LIBMTP_devicestorage_t *pds;
	TagLib_File *tlfile;
	TagLib_Tag *tltag;

	printf("Sending track %s to %s\n", from, to);



	/* taglib_tag_free_strings */


	parent_id = device->default_music_folder;

	if(stat(from, &sb) == -1){
		fprintf(stderr, "%s: ", from);
		perror("stat");
		fflush(stderr);
		return 1;
	}else if (S_ISREG (sb.st_mode)){
		trackmeta = LIBMTP_new_track_t();
/*		albuminfo = LIBMTP_new_album_t(); */

		trackmeta->filesize = sb.st_size;
		trackmeta->filename = to;
		trackmeta->filetype = LIBMTP_FILETYPE_MP3;
		trackmeta->parent_id = parent_id;
		if(!LIBMTP_FILETYPE_IS_TRACK(trackmeta->filetype))
			fprintf(stderr, "Not a valid track codec: \"%s\"\n", LIBMTP_Get_Filetype_Description(trackmeta->filetype));

		if((tlfile = taglib_file_new(from)) && (tltag = taglib_file_tag(tlfile))){
			trackmeta->title = taglib_tag_title(tltag);
			/* albuminfo->name = */ trackmeta->album = taglib_tag_album(tltag);
			/* albuminfo->artist = */ trackmeta->artist = taglib_tag_artist(tltag);
			snprintf(trackmeta->date = tmp, sizeof tmp, "%4d0101T0000.0", taglib_tag_year(tltag));
			trackmeta->tracknumber = taglib_tag_track(tltag);
		}else
			fprintf(stderr, "Missing tags in %s\n", from);

		printf("Sending track:\n");
		printf("Codec:     %s\n", LIBMTP_Get_Filetype_Description(trackmeta->filetype));

/*
		if(rc = LIBMTP_Get_Storage(device, LIBMTP_STORAGE_SORTBY_NOTSORTED))
			perror("LIBMTP_Get_Storage");
		else{
			for(desc = NULL, pds = device->storage; pds; pds = pds->next) if(pds->id == storageid){
				desc = pds->StorageDescription;
				break;
			}
			if (desc)
				printf("Storage ID: %s (%u)\n", desc, storageid);
			else
				printf("Storage ID: %u\n", storageid);
			trackmeta->storage_id = storageid;
		}
		*/
			
		printf(
		  "  pathid: %lu\n"
		  "  title : %s\n"
		  "  album : %s\n"
		  "  artist: %s\n"
		  "  date  : %s\n"
		  "  track : %i\n", parent_id, trackmeta->title, trackmeta->album, trackmeta->artist, trackmeta->date, trackmeta->tracknumber);
		printf("Sending track...\n");
		ret = LIBMTP_Send_Track_From_File(device, from, trackmeta, progress, NULL);
		printf("\n");
		if (ret != 0) {
			printf("Error sending track.\n");
			LIBMTP_Dump_Errorstack(device);
			LIBMTP_Clear_Errorstack(device);
		} else {
			printf("New track ID: %d\n", trackmeta->item_id);
		}

		/* Add here add to album call */
/*		if(trackmeta->album)
			ret = add_track_to_album(albuminfo, trackmeta);
*/
		trackmeta->filename = trackmeta->title = trackmeta->album = /* albuminfo->name = */trackmeta->artist =
		/* albuminfo->artist = */trackmeta->date = NULL;

/*		LIBMTP_destroy_album_t(albuminfo); */
		LIBMTP_destroy_track_t(trackmeta);

		taglib_tag_free_strings();
		taglib_file_free(tlfile);
	}
	fflush(stdout);
	fflush(stderr);
	return 0;
}
