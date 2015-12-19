/*----------------------------------------------------------------------------
--                                                                          --
--            FLORIST (FSU Implementation of POISX.5) COMPONENTS            --
--                                                                          --
--                                                                          --
--                       P O S I X - M A C R O S . C                        --
--                                                                          --
--  Copyright (c) 1996 Florida State University (FSU), All Rights Reserved. --
--                     Copyright (C) 1997-2010, AdaCore                     --
--                                                                          --
--  This file is a component of FLORIST, an  implementation of an  Ada API  --
--  for the POSIX OS services, for use with  the  GNAT  Ada  compiler  and  --
--  the FSU Gnu Ada Runtime Library (GNARL).   The  interface  is intended  --
--  to be close to that specified in  IEEE STD  1003.5: 1990  and IEEE STD  --
--  1003.5b: 1996.                                                          --
--                                                                          --
--  FLORIST is free software;  you can  redistribute  it and/or  modify it  --
--  under terms of the  GNU  General  Public  License as  published by the  --
--  Free Software Foundation;  either version  2, or (at  your option) any  --
--  later version.  FLORIST is distributed  in  the hope  that  it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without  even the implied  warranty  --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR  PURPOSE.  See  the GNU  --
--  General Public License for more details.  You  should have  received a  --
--  copy of the GNU General Public License  distributed  with  GNARL;  see  --
--  file  COPYING.  If not,  write to  the  Free  Software  Foundation, 59  --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.                   --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
----------------------------------------------------------------------------*/

/* file: posix-macros.c
   --------------------
   These subprograms provide access to POSIX functionality that is
   provided for C programs via macros.
 */

#define _REENTRANT

#include "pconfig.h"
#include <unistd.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <errno.h>

/* This definition is need for multi-threaded error codes on Solaris */

int s_isdir(mode_t mode) {
#ifdef S_ISDIR
  return S_ISDIR(mode);
#else
  return -1;
#endif
}

int s_ischr(mode_t mode) {
#ifdef S_ISCHR
  return S_ISCHR(mode);
#else
  return -1;
#endif
}

int s_isblk(mode_t mode) {
#ifdef S_ISBLK
  return S_ISBLK(mode);
#else
  return -1;
#endif
}

int s_isreg(mode_t mode) {
#ifdef S_ISREG
  return S_ISREG(mode);
#else
  return -1;
#endif
}

int s_islnk(mode_t mode) {
#ifdef S_ISLNK
  return S_ISLNK(mode);
#else
  return -1;
#endif
}

int s_issock(mode_t mode) {
#ifdef S_ISSOCK
  return S_ISSOCK(mode);
#else
  return -1;
#endif
}

int s_isfifo(mode_t mode) {
#ifdef S_ISFIFO
  return S_ISFIFO(mode);
#else
  return -1;
#endif
}

int s_ismsg(mode_t mode) {
#ifdef S_ISMSG
  return S_ISMSG(mode);
#else
  return -1;
#endif
}

int s_typeismq(struct stat *p) {
#ifdef S_TYPEISMQ
  return S_TYPEISMQ(p);
#else
  return 0;
#endif
}

int s_issem(mode_t mode) {
#ifdef S_ISSEM
  return S_ISSEM(mode);
#else
  return -1;
#endif
}

int s_typeissem(struct stat *p) {
#ifdef S_TYPEISSEM
  return S_TYPEISSEM(p);
#else
  return 0;
#endif
}

int s_isshm(mode_t mode) {
#ifdef S_ISSHM
  return S_ISSHM(mode);
#else
  return -1;
#endif
}

int s_typeisshm(struct stat *p) {
#ifdef S_TYPEISSHM
  return S_TYPEISSHM(p);
#else
  return 0;
#endif
}

int wifexited(int stat_val) {
#ifdef WIFEXITED
  return WIFEXITED(stat_val);
#else
  return -1;
#endif
}

int wexitstatus(int stat_val) {
#ifdef WEXITSTATUS
  return WEXITSTATUS(stat_val);
#else
  return -1;
#endif
}

int wifsignaled(int stat_val) {
#ifdef WIFSIGNALED
  return WIFSIGNALED(stat_val);
#else
  return -1;
#endif
}

int wtermsig(int stat_val) {
#ifdef WTERMSIG
  return WTERMSIG(stat_val);
#else
  return -1;
#endif
}

int wifstopped(int stat_val) {
#ifdef WIFSTOPPED
  return WIFSTOPPED(stat_val);
#else
  return -1;
#endif
}

int wstopsig(int stat_val) {
#ifdef WSTOPSIG
  return WSTOPSIG(stat_val);
#else
  return -1;
#endif
}

int fetch_errno() {
  return errno;
}

void store_errno(int value) {
  errno = value;
}

/* The following are variadic functions and on some platforms, for
   instance x86-64, calling a variadic function directly from Ada can
   cause problems. Here we provide wrappers that we import instead. */

int __gnat_florist_open(const char *path, int oflag, mode_t mode) {
  return open (path, oflag, mode);
}

sem_t *__gnat_florist_sem_open
(char *name, int oflag, mode_t mode, unsigned value) {
  return sem_open (name, oflag, mode, value);
}

/* The following wrappers work around problems on systems where the
   stat family of functions are implemented using macros. (eg. Tru64
   5.1A and Linux.) */

int __gnat_florist_stat(const char *path, struct stat *buf) {
  return stat(path, buf);
}

int __gnat_florist_lstat(const char *path, struct stat *buf) {
  return lstat(path, buf);
}

int __gnat_florist_fstat(int fd, struct stat *buf) {
  return fstat(fd, buf);
}

/* The following wrapper ensures that uname(3) is mapped correctly even
   when it is defined as an inlined function. */
int __gnat_florist_uname (struct utsname *s) {
  return uname (s);
}
