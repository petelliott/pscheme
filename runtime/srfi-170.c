#include "object.h"
#include <sys/stat.h>

void pscheme_populate_file_info(const char *path, pscheme_t file_info, bool follow) {
    struct stat stats;
    if (follow) {
        stat(path, &stats);
    } else {
        lstat(path, &stats);
    }

    pscheme_t* data = ptr(file_info);
    data[1] = make_pscm_fixnum(stats.st_dev);
    data[2] = make_pscm_fixnum(stats.st_ino);
    data[3] = make_pscm_fixnum(stats.st_mode);
    data[4] = make_pscm_fixnum(stats.st_nlink);
    data[5] = make_pscm_fixnum(stats.st_uid);
    data[6] = make_pscm_fixnum(stats.st_gid);
    data[7] = make_pscm_fixnum(stats.st_rdev);
    data[8] = make_pscm_fixnum(stats.st_size);
    data[9] = make_pscm_fixnum(stats.st_blksize);
    data[10] = make_pscm_fixnum(stats.st_blocks);
    data[11] = make_pscm_fixnum(stats.st_atime);
    data[12] = make_pscm_fixnum(stats.st_mtime);
    data[13] = make_pscm_fixnum(stats.st_ctime);
}
