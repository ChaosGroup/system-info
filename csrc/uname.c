#include <sys/utsname.h>
#include <wchar.h>
#include <stdlib.h>

#define OS_VERSION_MAX_SIZE 128


wchar_t* getOS() {
  struct utsname system_info;
  if (uname(&system_info) != 0) return NULL;

  wchar_t* os = malloc(sizeof(os) * OS_VERSION_MAX_SIZE);
  if (swprintf(os, OS_VERSION_MAX_SIZE - 1, L"%s %s", system_info.sysname, system_info.release) == -1) {
    free(os);
    os = NULL;
  }
  return os;
}
