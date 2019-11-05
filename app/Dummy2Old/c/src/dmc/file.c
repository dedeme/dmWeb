// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/file.h"
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include "dmc/std.h"
#include "dmc/cryp.h"

struct file_FileLck {
  struct flock *lock;
  FILE *f;
};

static FileLck *lckFile_new(struct flock *lock, FILE *f) {
  FileLck *this = malloc(sizeof(FileLck));
  this->lock = lock;
  this->f = f;
  return this;
}

void file_tmp (char **prefix) {
  char *k = cryp_genk_new(10);
  str_creplace(&k, '/', '_');
  char *r = str_f_new("/tmp/%s%s", *prefix, k);
  free(k);
  if (file_exists(r)) {
    free(r);
    file_tmp(prefix);
  }
  free(*prefix);
  *prefix = r;
}

char *file_cwd_new (void) {
  char *d = getcwd(NULL, 0);
  if (!d) {
    FAIL(str_f_new("Working directory can no be find: %s", strerror(errno)))
  }
  return d;
}

void file_cd (const char *path) {
  if (chdir(path)) {
    FAIL(str_f_new(
      "Fail changing the working directory to %s: %s",
      path, strerror(errno)
    ))
  }
}

void file_mkdir (const char *path) {
  if (!*path) {
    return;
  }

  char *p;
  if (*path != '/') {
    char *cwd = file_cwd_new();
    p = path_cat_new(cwd, path, NULL);
    free(cwd);
  } else {
    p = str_new(path);
  }
  path_parent(&p);
  file_mkdir(p);
  free(p);

  if (mkdir(path, 0755) && errno != EEXIST) {
    FAIL(str_f_new("Fail making directory %s: %s" , path, strerror(errno)))
  }
}

// Arr[char]
Arr *file_dir_new (const char *path) {
  DIR *d = opendir(path);
  if (!d) {
    FAIL(str_f_new("Fail reading directory %s: %s", path, strerror(errno)))
  }
  // Arr[char]
  Arr *a = arr_new(free);
  struct dirent *res;
  while ((res = readdir(d))) {
    char *name = res->d_name;
    if (str_eq(name, ".") || str_eq(name, "..")) {
      continue;
    }
    arr_push(a, str_new(name));
  }
  closedir(d);
  return a;
}

void file_del (const char *path) {
  struct stat buf;
  if (stat(path, &buf)) {
    if (errno != ENOENT) {
      FAIL(str_f_new("Fail deleting %s: %s", path, strerror(errno)))
    }
  }

  if (S_ISDIR(buf.st_mode)) {
    // Arr[char]
    Arr *a = file_dir_new(path);
    EACH(a, char, f) {
      char *p = path_cat_new(path, f, NULL);
      file_del(p);
      free(p);
    }_EACH
    arr_free(a);
    if (rmdir(path) && errno != ENOENT) {
      FAIL(str_f_new("Fail deleting %s: %s", path, strerror(errno)))
    }
  } else {
    if (unlink(path) && errno != ENOENT) {
      FAIL(str_f_new("Fail deleting %s: %s", path, strerror(errno)))
    }
  }
}

void file_rename (const char *oldname, const char *newname) {
  if (rename(oldname, newname) == -1) {
    FAIL(str_f_new(
      "Fail renaming '%s' to '%s: %s", oldname, newname, strerror(errno)
    ))
  }
}

void file_link (const char *oldpath, const char *newpath) {
  if (symlink(oldpath, newpath) == -1) {
    FAIL(str_f_new(
      "Fail linking '%s' to '%s: %s", newpath, oldpath, strerror(errno)
    ))
  }
}

int file_exists (const char *path) {
  struct stat buf;
  if (stat(path, &buf)) {
    if (errno == ENOENT) {
      return 0;
    }
    FAIL(str_f_new("Fail reading %s: %s", path, strerror(errno)))
  }
  return 1;
}

int file_is_directory (const char *path) {
  struct stat buf;
  if (stat(path, &buf)) {
    if (errno == ENOENT) {
      return 1;
    }
    FAIL(str_f_new("Fail reading %s: %s", path, strerror(errno)))
  }
  if (S_ISDIR(buf.st_mode))
    return 1;
  return 0;
}

struct stat *file_info_new (const char *path) {
  struct stat *r = malloc(sizeof(struct stat));
  if (stat(path, r)) {
    FAIL(str_f_new("Fail reading %s: %s", path, strerror(errno)))
  }
  return r;
}

int file_size(const char *path) {
  struct stat *i = file_info_new (path);
  int r = i->st_size;
  free(i);
  return r;
}

time_t file_modified(const char *path) {
  struct stat *i = file_info_new (path);
  time_t r = i->st_mtime;
  free(i);
  return r;
}

char *file_read_new (const char *path) {
  FILE *fl;
  size_t len = 0;
  struct flock lck = {
    .l_whence = SEEK_SET,
    .l_start = 0,
    .l_len = 0,
  };

  fl = fopen(path, "r");
  if (!fl) {
    FAIL(str_f_new("Fail openning %s: %s", path, strerror(errno)))
  }

  lck.l_type = F_RDLCK;
  fcntl (fileno(fl), F_SETLKW, &lck);

  Buf *bf = buf_new();
  char *line = NULL;
  while (getline(&line, &len, fl) != -1) {
    buf_add(bf, line);
    free(line);
    line = NULL;
  }
  free(line);

  lck.l_type = F_UNLCK;
  fcntl (fileno(fl), F_SETLK, &lck);
  fclose(fl);

  char *r = buf_to_str_new(bf);
  buf_free(bf);
  return r;
}

void file_write (const char *path, const char *text) {
  FILE *fl;
  int error;
  struct flock lck = {
    .l_whence = SEEK_SET,
    .l_start = 0,
    .l_len = 0,
  };

  fl = fopen(path, "w");
  if (!fl) {
    FAIL(str_f_new("Fail openning %s: %s", path, strerror(errno)))
  }

  lck.l_type = F_WRLCK;
  fcntl (fileno(fl), F_SETLKW, &lck);

  error = fputs(text, fl);

  lck.l_type = F_UNLCK;
  fcntl (fileno(fl), F_SETLK, &lck);

  if (error == EOF || error < 0) {
    fclose(fl);
    FAIL(str_f_new("Fail writing '%s': %s", path, strerror(error)))
  }
  fclose(fl);
}

void file_append (const char *path, const char *text) {
  FILE *fl;
  int error;
  struct flock lck = {
    .l_whence = SEEK_SET,
    .l_start = 0,
    .l_len = 0,
  };

  fl = fopen(path, "a");
  if (!fl) {
    FAIL(str_f_new("Fail openning %s: %s", path, strerror(errno)))
  }

  lck.l_type = F_WRLCK;
  fcntl (fileno(fl), F_SETLKW, &lck);

  error = fputs(text, fl);

  lck.l_type = F_UNLCK;
  fcntl (fileno(fl), F_SETLK, &lck);

  if (error == EOF || error < 0) {
    fclose(fl);
    FAIL(str_f_new("Fail writing '%s': %s", path, strerror(error)))
  }
  fclose(fl);
}

void file_copy (const char *source_path, const char *target_path) {
  if (!strcmp(source_path, target_path)) {
    return;
  }

  FILE *f1;
  FILE *f2;

  char  buffer[32678];
  size_t  n;

  if ((f1 = fopen(source_path, "rb")) == 0) {
    FAIL(str_f_new("Fail openning '%s': %s", source_path, strerror(errno)))
  }
  if ((f2 = fopen(target_path, "wb")) == 0) {
    FAIL(str_f_new("Fail openning '%s': %s", target_path, strerror(errno)))
  }

  while ((n = fread(buffer, sizeof(char), sizeof(buffer), f1)) > 0) {
    if (fwrite(buffer, sizeof(char), n, f2) != n) {
      FAIL(str_f_new("Fail writing '%s': %s", target_path, strerror(errno)))
    }
  }

  fclose(f1);
  fclose(f2);
}

static FileLck *lck_new(FILE *file) {
  struct flock *lck = malloc(sizeof(struct flock));
  lck->l_whence = SEEK_SET;
  lck->l_start = 0;
  lck->l_len = 0;
  return lckFile_new(lck, file);
}

FileLck *file_ropen (const char *path) {
  FILE *file = fopen(path, "r");
  if (!file) {
    FAIL(str_f_new("Fail opening '%s': %s", path, strerror(errno)))
  }

  FileLck *r = lck_new(file);
  r->lock->l_type = F_RDLCK;
  fcntl (fileno(file), F_SETLKW, r->lock);
  return r;
}

FileLck *file_wopen (const char *path) {
  FILE *file = fopen(path, "w");
  if (!file) {
    FAIL(str_f_new("Fail opening '%s': %s", path, strerror(errno)))
  }

  FileLck *r = lck_new(file);
  r->lock->l_type = F_WRLCK;
  fcntl (fileno(file), F_SETLKW, r->lock);
  return r;
}

FileLck *file_aopen (const char *path) {
  FILE *file = fopen(path, "a");
  if (!file) {
    FAIL(str_f_new("Fail opening '%s': %s", path, strerror(errno)))
  }

  FileLck *r = lck_new(file);
  r->lock->l_type = F_WRLCK;
  fcntl (fileno(file), F_SETLKW, r->lock);
  return r;
}

char *file_read_line_new (FileLck *lck) {
  size_t len = 0;
  errno = 0;
  char *line= NULL;
  if (getline(&line, &len, lck->f) != -1) {
    char *r = str_new(line);
    free(line);
    return r;
  }
  free(line);
  if (errno) {
    file_close(lck);
    FAIL(str_f_new("Fail file_read_line: %s", strerror(errno)))
  }
  return str_new("");
}

void file_write_text (FileLck *lck, const char *text) {
  int error = fputs(text, lck->f);
  if (error == EOF || error < 0) {
    file_close(lck);
    FAIL(str_f_new("Fail file_write_line: %s", strerror(errno)))
  }
}

Bytes *file_read_bin_buf_new (FileLck *lck, int buffer) {
  unsigned char bs[buffer];
  int len = (int)fread(bs, 1, buffer, lck->f);
  if (len == -1) {
    file_close(lck);
    FAIL(str_f_new("Fail file_read_bin_buf: %s", strerror(errno)))
  }
  if (len == 0) {
    return bytes_new();
  }
  return bytes_from_bytes_new(bs, len);
}

Bytes *file_read_bin_new (FileLck *lck) {
  return file_read_bin_buf_new(lck, 8192);
}

void file_write_bin (FileLck *lck, Bytes *bs) {
  if (fwrite(bytes_bs(bs), bytes_len(bs), 1, lck->f) == -1) {
    FAIL(str_f_new("Fail file_write_bin: %s", strerror(errno)))
  }
}

void file_close (FileLck *lck) {
  FILE *fl = lck->f;
  struct flock *lock = lck->lock;

  lock->l_type = F_UNLCK;
  fcntl (fileno(fl), F_SETLK, &lock);
  fclose(fl);

  free(lock);
  free(lck);
}
