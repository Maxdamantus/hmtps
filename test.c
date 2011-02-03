#include <libmtp.h>

LIBMTP_folder_t *folders;
LIBMTP_file_t *files;
LIBMTP_mtpdevice_t *device;

int sendtrack_function(char *, char *);

int main(){
	LIBMTP_Init();

	if(!(device = LIBMTP_Get_First_Device())){
		fprintf(stderr, "Couldn't grab a first device\n");
		return 1;
	}

	sendtrack_function("/hd/media/music/Thom Yorke/The Eraser/08 Harrowdown Hill.mp3","Music/test.mp3");

	LIBMTP_Release_Device(device);

	return 0;
}
