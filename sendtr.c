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
#include <libgen.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <libmtp.h>
#include "pathutils.h"

#include <tag_c.h>

extern LIBMTP_folder_t *folders;
extern LIBMTP_file_t *files;
extern LIBMTP_mtpdevice_t *device;

static int add_track_to_album(LIBMTP_album_t *albuminfo, LIBMTP_track_t *trackmeta)
{
  LIBMTP_album_t *album;
  LIBMTP_album_t *found_album = NULL;
  int ret;

  /* Look for the album */
  album = LIBMTP_Get_Album_List(device);
  while(album != NULL) {
    if ((album->name != NULL &&
	album->artist != NULL &&
	!strcmp(album->name, albuminfo->name) &&
	!strcmp(album->artist, albuminfo->artist)) ||
	  (album->name != NULL &&
	album->composer != NULL &&
	!strcmp(album->name, albuminfo->name) &&
	!strcmp(album->composer, albuminfo->composer))) {
      /* Disconnect this album for later use */
      found_album = album;
      album = album->next;
      found_album->next = NULL;
    } else {
      LIBMTP_album_t *tmp;

      tmp = album;
      album = album->next;
      LIBMTP_destroy_album_t(tmp);
    }
  }
  
  if (found_album != NULL) {
    uint32_t *tracks;

    tracks = (uint32_t *)malloc((found_album->no_tracks+1) * sizeof(uint32_t));
    printf("Album \"%s\" found: updating...\n", found_album->name);
    if (!tracks) {
      printf("failed malloc in add_track_to_album()\n");
      return 1;
    }
    found_album->no_tracks++;
    if (found_album->tracks != NULL) {
      memcpy(tracks, found_album->tracks, found_album->no_tracks * sizeof(uint32_t));
      free(found_album->tracks);
    }
    tracks[found_album->no_tracks-1] = trackmeta->item_id;
    found_album->tracks = tracks;
    ret = LIBMTP_Update_Album(device, found_album);
    LIBMTP_destroy_album_t(found_album);
  } else {
    uint32_t *trackid;
    
    trackid = (uint32_t *)malloc(sizeof(uint32_t));
    *trackid = trackmeta->item_id;
    albuminfo->tracks = trackid;
    albuminfo->no_tracks = 1;
    albuminfo->storage_id = trackmeta->storage_id;
    printf("Album doesn't exist: creating...\n");
    ret = LIBMTP_Create_New_Album(device, albuminfo);
    /* albuminfo will be destroyed later by caller */
  }
  
  if (ret != 0) {
    printf("Error creating or updating album.\n");
    printf("(This could be due to that your device does not support albums.)\n");
    LIBMTP_Dump_Errorstack(device);
    LIBMTP_Clear_Errorstack(device);
  } else {
    printf("success!\n");
  }
  return ret;
}

int sendtrack_function(char * from_path, char * to_path){
	char tmp[80];
	char *filename, *parent, *desc, *dn, *bn;
	unsigned long long filesize;
	unsigned long parent_id = 0;
	int ret, rc;
	struct stat sb;
	LIBMTP_track_t *trackmeta;
	LIBMTP_album_t *albuminfo;
	LIBMTP_devicestorage_t *pds;
	TagLib_File *tlfile;
	TagLib_Tag *tltag;

	printf("Sending track %s to %s\n",from_path,to_path);



	/* taglib_tag_free_strings */


	parent = dirname(dn = strdup(to_path));
	filename = basename(bn = strdup(to_path));
	parent_id = parse_path(parent,files,folders);

	if(stat(from_path, &sb) == -1){
		fprintf(stderr, "%s: ", from_path);
		perror("stat");
		return 1;
	}else if (S_ISREG (sb.st_mode)){
		trackmeta = LIBMTP_new_track_t();
		albuminfo = LIBMTP_new_album_t();

		trackmeta->filesize = sb.st_size;
		trackmeta->filename = filename;
		trackmeta->filetype = find_filetype(from_path);
		trackmeta->parent_id = parent_id;
		if(!LIBMTP_FILETYPE_IS_TRACK(trackmeta->filetype))
			fprintf(stderr, "Not a valid track codec: \"%s\"\n", LIBMTP_Get_Filetype_Description(trackmeta->filetype));

		if((tlfile = taglib_file_new(from_path)) && (tltag = taglib_file_tag(tlfile))){
			trackmeta->title = taglib_tag_title(tltag);
			albuminfo->name = trackmeta->album = taglib_tag_album(tltag);
			albuminfo->artist = trackmeta->artist = taglib_tag_artist(tltag);
			snprintf(trackmeta->date = tmp, sizeof tmp, "%4d0101T0000.0", taglib_tag_year(tltag));
		}else
			fprintf(stderr, "Missing tags in %s\n", from_path);

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
		  "  title : %s\n"
		  "  album : %s\n"
		  "  artist: %s\n"
		  "  date  : %n\n", trackmeta->title, trackmeta->album, trackmeta->artist, trackmeta->date);
		printf("Sending track...\n");
		ret = LIBMTP_Send_Track_From_File(device, from_path, trackmeta, progress, NULL);
		printf("\n");
		if (ret != 0) {
			printf("Error sending track.\n");
			LIBMTP_Dump_Errorstack(device);
			LIBMTP_Clear_Errorstack(device);
		} else {
			printf("New track ID: %d\n", trackmeta->item_id);
		}

		/* Add here add to album call */
		if(trackmeta->album)
			ret = add_track_to_album(albuminfo, trackmeta);

		taglib_tag_free_strings();
		taglib_file_free(tlfile);

		trackmeta->filename = trackmeta->title = trackmeta->album = albuminfo->name = trackmeta->artist =
		albuminfo->artist = trackmeta->date = NULL;

		LIBMTP_destroy_album_t(albuminfo);
		LIBMTP_destroy_track_t(trackmeta);

		free(dn);
		free(bn);
	}
	return 0;
}
