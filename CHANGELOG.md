## 0.5.0.0
  * drop the functionality for getting the name and number of available CPUs
  * fix the problem with getting the name of the operating system multiple times
## 0.4.0.1
	* fix the threading problems with getting the OS's name (on Windows)
## 0.4.0.0
	* make os returning an IO OS, since it caused problems on Windows
## 0.3.0.0
	* make os returning an OS instead of a String
## 0.2.0.0
	* rename getOS to os and make it a pure function
	* use wmi queries for getting OS' names on Windows
	* use system calls for getting OS' names on Unixes
## 0.1.0.13
	* remove trailing newlines from OS' names
## 0.1.0.12
	* fix the library to build on Windows
## 0.1.0.11
	* fallback to uname on non-Windows OSes, if the other tools fail
## 0.1.0.10
	* use attoparsec instead of regexes for matching strings
## 0.1.0.9
	* get OS name and cpu information on macOS
	* build system-info with lts-9.11
## 0.1.0.8
	* build system-info with lts-9.0
## 0.1.0.7
	* provide a custom Show OS instance
	* relax build-depends constraints
## 0.1.0.6
	* get a more concrete OS name (e.g. Arch Linux and Windows 7 Professional
	instead of just Linux and Windows)

## Unreleased changes
