#ifndef Py_FILEUTILS_H
#define Py_FILEUTILS_H

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(Py_LIMITED_API) || Py_LIMITED_API+0 >= 0x03050000
PyAPI_FUNC(wchar_t *) Py_DecodeLocale(
    const char *arg,
    size_t *size);

PyAPI_FUNC(char*) Py_EncodeLocale(
    const wchar_t *text,
    size_t *error_pos);

PyAPI_FUNC(char*) _Py_EncodeLocaleRaw(
    const wchar_t *text,
    size_t *error_pos);
#endif

#ifdef Py_BUILD_CORE
PyAPI_FUNC(int) _Py_DecodeUTF8Ex(
    const char *arg,
    Py_ssize_t arglen,
    wchar_t **wstr,
    size_t *wlen,
    const char **reason,
    int surrogateescape);

PyAPI_FUNC(int) _Py_EncodeUTF8Ex(
    const wchar_t *text,
    char **str,
    size_t *error_pos,
    const char **reason,
    int raw_malloc,
    int surrogateescape);

PyAPI_FUNC(wchar_t*) _Py_DecodeUTF8_surrogateescape(
    const char *arg,
    Py_ssize_t arglen);

PyAPI_FUNC(int) _Py_DecodeLocaleEx(
    const char *arg,
    wchar_t **wstr,
    size_t *wlen,
    const char **reason,
    int current_locale,
    int surrogateescape);

PyAPI_FUNC(int) _Py_EncodeLocaleEx(
    const wchar_t *text,
    char **str,
    size_t *error_pos,
    const char **reason,
    int current_locale,
    int surrogateescape);
#endif

#ifndef Py_LIMITED_API
PyAPI_FUNC(PyObject *) _Py_device_encoding(int);

#ifdef MS_WINDOWS
struct _Py_stat_struct {
    unsigned long st_dev;
    uint64_t st_ino;
    unsigned short st_mode;
    int st_nlink;
    int st_uid;
    int st_gid;
    unsigned long st_rdev;
    __int64 st_size;
    time_t st_atime;
    int st_atime_nsec;
    time_t st_mtime;
    int st_mtime_nsec;
    time_t st_ctime;
    int st_ctime_nsec;
    unsigned long st_file_attributes;
};
#else
#  define _Py_stat_struct stat
#endif

PyAPI_FUNC(int) _Py_fstat(
    int fd,
    struct _Py_stat_struct *status);

PyAPI_FUNC(int) _Py_fstat_noraise(
    int fd,
    struct _Py_stat_struct *status);

PyAPI_FUNC(int) _Py_stat(
    PyObject *path,
    struct stat *status);

PyAPI_FUNC(int) _Py_open(
    const char *pathname,
    int flags);

PyAPI_FUNC(int) _Py_open_noraise(
    const char *pathname,
    int flags);

PyAPI_FUNC(FILE *) _Py_wfopen(
    const wchar_t *path,
    const wchar_t *mode);

PyAPI_FUNC(FILE*) _Py_fopen(
    const char *pathname,
    const char *mode);

PyAPI_FUNC(FILE*) _Py_fopen_obj(
    PyObject *path,
    const char *mode);

PyAPI_FUNC(Py_ssize_t) _Py_read(
    int fd,
    void *buf,
    size_t count);

PyAPI_FUNC(Py_ssize_t) _Py_write(
    int fd,
    const void *buf,
    size_t count);

PyAPI_FUNC(Py_ssize_t) _Py_write_noraise(
    int fd,
    const void *buf,
    size_t count);

#ifdef HAVE_READLINK
PyAPI_FUNC(int) _Py_wreadlink(
    const wchar_t *path,
    wchar_t *buf,
    size_t bufsiz);
#endif

#ifdef HAVE_REALPATH
PyAPI_FUNC(wchar_t*) _Py_wrealpath(
    const wchar_t *path,
    wchar_t *resolved_path,
    size_t resolved_path_size);
#endif

PyAPI_FUNC(wchar_t*) _Py_wgetcwd(
    wchar_t *buf,
    size_t size);

PyAPI_FUNC(int) _Py_get_inheritable(int fd);

PyAPI_FUNC(int) _Py_set_inheritable(int fd, int inheritable,
                                    int *atomic_flag_works);

PyAPI_FUNC(int) _Py_set_inheritable_async_safe(int fd, int inheritable,
                                               int *atomic_flag_works);

PyAPI_FUNC(int) _Py_dup(int fd);

#ifndef MS_WINDOWS
PyAPI_FUNC(int) _Py_get_blocking(int fd);

PyAPI_FUNC(int) _Py_set_blocking(int fd, int blocking);
#endif   /* !MS_WINDOWS */

PyAPI_FUNC(int) _Py_GetLocaleconvNumeric(
    PyObject **decimal_point,
    PyObject **thousands_sep,
    const char **grouping);

#endif   /* Py_LIMITED_API */

#ifdef __cplusplus
}
#endif

#endif /* !Py_FILEUTILS_H */
