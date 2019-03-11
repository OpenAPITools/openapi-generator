
/* System module interface */

#ifndef Py_SYSMODULE_H
#define Py_SYSMODULE_H
#ifdef __cplusplus
extern "C" {
#endif

PyAPI_FUNC(PyObject *) PySys_GetObject(const char *);
PyAPI_FUNC(int) PySys_SetObject(const char *, PyObject *);
#ifndef Py_LIMITED_API
PyAPI_FUNC(PyObject *) _PySys_GetObjectId(_Py_Identifier *key);
PyAPI_FUNC(int) _PySys_SetObjectId(_Py_Identifier *key, PyObject *);
#endif

PyAPI_FUNC(void) PySys_SetArgv(int, wchar_t **);
PyAPI_FUNC(void) PySys_SetArgvEx(int, wchar_t **, int);
PyAPI_FUNC(void) PySys_SetPath(const wchar_t *);

PyAPI_FUNC(void) PySys_WriteStdout(const char *format, ...)
                 Py_GCC_ATTRIBUTE((format(printf, 1, 2)));
PyAPI_FUNC(void) PySys_WriteStderr(const char *format, ...)
                 Py_GCC_ATTRIBUTE((format(printf, 1, 2)));
PyAPI_FUNC(void) PySys_FormatStdout(const char *format, ...);
PyAPI_FUNC(void) PySys_FormatStderr(const char *format, ...);

PyAPI_FUNC(void) PySys_ResetWarnOptions(void);
PyAPI_FUNC(void) PySys_AddWarnOption(const wchar_t *);
PyAPI_FUNC(void) PySys_AddWarnOptionUnicode(PyObject *);
PyAPI_FUNC(int) PySys_HasWarnOptions(void);

PyAPI_FUNC(void) PySys_AddXOption(const wchar_t *);
PyAPI_FUNC(PyObject *) PySys_GetXOptions(void);

#ifndef Py_LIMITED_API
PyAPI_FUNC(size_t) _PySys_GetSizeOf(PyObject *);
#endif

#ifdef Py_BUILD_CORE
PyAPI_FUNC(int) _PySys_AddXOptionWithError(const wchar_t *s);
PyAPI_FUNC(int) _PySys_AddWarnOptionWithError(PyObject *option);
#endif

#ifdef __cplusplus
}
#endif
#endif /* !Py_SYSMODULE_H */
