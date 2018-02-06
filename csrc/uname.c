#include <sys/utsname.h>
#include <wchar.h>
#include <stdlib.h>

#define OS_VERSION_MAX_SIZE 128


wchar_t* getOS() {
  struct utsname system_info;
  if (uname(&system_info) != 0) return NULL;

  wchar_t* os = malloc(sizeof(os) * OS_VERSION_MAX_SIZE);
#ifdef __APPLE__
  int version = atoi(system_info.release);

  // Since Darwin 5.1 (released 2001), Darwin xx corresponds to Mac OS X 10.(xx - 4)
  const wchar_t* format;
  if (version < 5) {
    format = L"Mac OS X";
  } else {
    format = L"Mac OS X 10.%d";
  }

  if (swprintf(os, OS_VERSION_MAX_SIZE - 1, format, atoi(system_info.release) - 4) == -1) {
#else
  if (swprintf(os, OS_VERSION_MAX_SIZE - 1, L"%s %s", system_info.sysname, system_info.release) == -1) {
#endif
    free(os);
    os = NULL;
  }
  return os;
}
