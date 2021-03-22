#include <sys/utsname.h>
#include <wchar.h>
#include <stdlib.h>

#define OS_VERSION_MAX_SIZE 128


wchar_t* getOS() {
  struct utsname system_info;
  if (uname(&system_info) != 0) return NULL;

  wchar_t* os = malloc(sizeof(os) * OS_VERSION_MAX_SIZE);
#ifdef __APPLE__
  int major_version = atoi(system_info.release);
  int minor_version = atoi(system_info.release + 3);

  // Since Darwin 5.1 (released 2001), Darwin xx corresponds to Mac OS X 10.(xx - 4)
  // Since Darwin 20.1 (released 2020), Darwin xx.yy corresponds to Mac OS X (xx - 9).(yy - 1)
  const wchar_t* format;
  if (major_version < 5) {
    format = L"Mac OS X";
  } else if (major_version < 20) {
    format = L"Mac OS X %d.%d";
    minor_version = major_version - 4;
    major_version = 10;
  } else {
    format = L"Mac OS X %d.%d";
    minor_version = minor_version - 1;
    major_version = major_version - 9;
  }

  if (swprintf(os, OS_VERSION_MAX_SIZE, format, major_version, minor_version) == -1) {
#else
  if (swprintf(os, OS_VERSION_MAX_SIZE, L"%s %s", system_info.sysname, system_info.release) == -1) {
#endif
    free(os);
    os = NULL;
  }
  return os;
}
