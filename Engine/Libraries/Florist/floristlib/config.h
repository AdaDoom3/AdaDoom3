/* confsrc/config.h.  Generated from config.h.in by configure.  */
/* When adding new names (header-files, types, functions, etc.)
   to this file, don't forget to also add the corresponding names
   to configure.in.

   There are some non-POSIX names here, that we have learned by
   experience are defined on certain systems and may be used in place
   of the proper POSIX names.  In particular, the names of the form
   __posix_XXX are not real POSIX names, but rather are stand-ins for
   the proper POSIX function XXX, specific to Solaris 2.6.  I apologize
   for this ugly hack.  Suggestions for a cleaner workaround would be
   welcome (see related comment on SOLARIS in the file README).

 */

/* header files
 */

#define HAVE_DIRENT_H 1


/* functions
 */

#define HAVE_accept 1
#define HAVE_access 1
#define HAVE_aio_cancel 1
#define HAVE_aio_error 1
#define HAVE_aio_fsync 1
#define HAVE_aio_read 1
#define HAVE_aio_return 1
#define HAVE_aio_suspend 1
#define HAVE_aio_write 1

#define HAVE_bind 1

#define HAVE_cfgetispeed 1
#define HAVE_cfgetospeed 1
#define HAVE_cfsetispeed 1
#define HAVE_cfsetospeed 1
#define HAVE_chdir 1
#define HAVE_chmod 1
#define HAVE_chown 1
#define HAVE_clock_getres 1
#define HAVE_clock_gettime 1
#define HAVE_clock_settime 1
#define HAVE_close 1
#define HAVE_closedir 1
#define HAVE_connect 1
#define HAVE_ctermid 1
#define HAVE_ctime 1
#define HAVE_ctime_r 1

#define HAVE_dup 1
#define HAVE_dup2 1

/* not referenced: 1 */
#define HAVE_endhostent 1
#define HAVE_endnetent 1
#define HAVE_endprotoent 1
/* not referenced: 1 */
#define HAVE_endservent 1
#define HAVE_execl 1
#define HAVE_execle 1
#define HAVE_execlp 1
#define HAVE_execv 1
#define HAVE_execve 1
#define HAVE_execvp 1

#define HAVE_fchmod 1
#define HAVE_fcntl 1
#define HAVE_fdatasync 1
#define HAVE_fork 1
#define HAVE_fpathconf 1
#define HAVE_fstat 1
#define HAVE_fsync 1
#define HAVE_ftruncate 1

#define HAVE_getaddrinfo 1
#define HAVE_getcwd 1
#define HAVE_getegid 1
#define HAVE_getenv 1
#define HAVE_geteuid 1
#define HAVE_getgid 1
/* not referenced: 2 */
#define HAVE_getgrgid 1
#define HAVE_getgrgid_r 1
/* not referenced: 2 */
#define HAVE_getgrnam 1
#define HAVE_getgrnam_r 1
#define HAVE_getgroups 1
/* not referenced: 5 */
#define HAVE_gethostbyaddr 1
#define HAVE_gethostbyaddr_r 1
#define HAVE_gethostbyname 1
#define HAVE_gethostbyname_r 1
#define HAVE_gethostname 1
#define HAVE_getlogin 1
#define HAVE_getlogin_r 1
#define HAVE_getnetbyaddr_r 1
#define HAVE_getnetbyname_r 1
/* not referenced: 2 */
#define HAVE_getnetbyaddr 1
#define HAVE_getnetbyname 1
#define HAVE_getpeername 1
#define HAVE_getpgrp 1
#define HAVE_getpid 1
#define HAVE_getppid 1
#define HAVE_getprotobyname_r 1
#define HAVE_getprotobynumber_r 1
/* not referenced: 2 */
#define HAVE_getprotobyname 1
#define HAVE_getprotobynumber 1
#define HAVE_getpwnam 1
#define HAVE_getpwnam_r 1
#define HAVE_getpwuid 1
/* unreferenced: 4 */
#define HAVE_getservbyname 1
#define HAVE_getservbyname_r 1
#define HAVE_getservbyport 1
#define HAVE_getservbyport_r 1
#define HAVE_getsockname 1
#define HAVE_getsockopt 1
#define HAVE_gettimeofday 1
#define HAVE_getuid 1
/* unreferenced: 1 */
#define HAVE_gmtime_r 1

/* not referenced: 5 */
#define HAVE_inet_addr 1
#define HAVE_inet_lnaof 1
#define HAVE_inet_makeaddr 1
#define HAVE_inet_netof 1
#define HAVE_inet_network 1
#define HAVE_inet_ntoa 1
#define HAVE_isatty 1
/* not referenced: 1 */
#define HAVE_isfdtype 1

#define HAVE_kill 1

#define HAVE_link 1
#define HAVE_lio_listio 1
#define HAVE_listen 1
#define HAVE_lseek 1
#define HAVE_lstat 1

#define HAVE_mkdir 1
#define HAVE_mkfifo 1
#define HAVE_mlock 1
#define HAVE_mlockall 1
#define HAVE_mmap 1
#define HAVE_mprotect 1
#define HAVE_mq_close 1
#define HAVE_mq_getattr 1
#define HAVE_mq_notify 1
#define HAVE_mq_open 1
#define HAVE_mq_receive 1
#define HAVE_mq_send 1
#define HAVE_mq_setattr 1
#define HAVE_mq_unlink 1
#define HAVE_msync 1
#define HAVE_munlock 1
#define HAVE_munlockall 1
#define HAVE_munmap 1

#define HAVE_open 1
#define HAVE_opendir 1

#define HAVE_pathconf 1
#define HAVE_pipe 1
#define HAVE_poll 1

#define HAVE_pthread_cond_broadcast 1
#define HAVE_pthread_cond_destroy 1
#define HAVE_pthread_cond_init 1
#define HAVE_pthread_cond_signal 1
#define HAVE_pthread_cond_timedwait 1
#define HAVE_pthread_cond_wait 1
#define HAVE_pthread_condattr_destroy 1
#define HAVE_pthread_condattr_getpshared 1
#define HAVE_pthread_condattr_init 1
#define HAVE_pthread_condattr_setpshared 1

#define HAVE_pthread_mutex_destroy 1
#define HAVE_pthread_mutex_getprioceiling 1
#define HAVE_pthread_mutex_init 1
#define HAVE_pthread_mutex_lock 1
#define HAVE_pthread_mutex_setprioceiling 1
#define HAVE_pthread_mutex_trylock 1
#define HAVE_pthread_mutex_unlock 1
#define HAVE_pthread_mutexattr_destroy 1
#define HAVE_pthread_mutexattr_getprioceiling 1
#define HAVE_pthread_mutexattr_getprotocol 1
#define HAVE_pthread_mutexattr_getpshared 1
#define HAVE_pthread_mutexattr_init 1
#define HAVE_pthread_mutexattr_setprioceiling 1
#define HAVE_pthread_mutexattr_setprotocol 1
#define HAVE_pthread_mutexattr_setpshared 1

#define HAVE_pthread_sigmask 1

#define HAVE_putenv 1

#define HAVE_read 1
#define HAVE_readdir 1
/* not referenced: 1 */
#define HAVE_readdir_r 1
#define HAVE_recv 1
#define HAVE_recvfrom 1
#define HAVE_recvmsg 1
#define HAVE_rename 1
#define HAVE_rmdir 1

#define HAVE_sched_get_priority_max 1
#define HAVE_sched_get_priority_min 1
#define HAVE_sched_getparam 1
#define HAVE_sched_getscheduler 1
#define HAVE_sched_rr_get_interval 1
#define HAVE_sched_setparam 1
#define HAVE_sched_setscheduler 1
#define HAVE_sched_yield 1

#define HAVE_select 1
#define HAVE_sem_close 1
#define HAVE_sem_destroy 1
#define HAVE_sem_getvalue 1
#define HAVE_sem_init 1
#define HAVE_sem_open 1
#define HAVE_sem_post 1
#define HAVE_sem_trywait 1
#define HAVE_sem_unlink 1
#define HAVE_sem_wait 1
#define HAVE_send 1
#define HAVE_sendmsg 1
#define HAVE_sendto 1
#define HAVE_setenv 1
#define HAVE_setgid 1
/* not referenced: 1 */
#define HAVE_sethostent 1
#define HAVE_setnetent 1
#define HAVE_setpgid 1
#define HAVE_setprotoent 1
/* not referenced: 1 */
#define HAVE_setservent 1
#define HAVE_setsid 1
#define HAVE_setsockopt 1
#define HAVE_setuid 1
#define HAVE_shm_open 1
#define HAVE_shm_unlink 1
#define HAVE_shutdown 1
#define HAVE_sigaction 1
#define HAVE_sigaddset 1
#define HAVE_sigdelset 1
#define HAVE_sigemptyset 1
#define HAVE_sigfillset 1
#define HAVE_sigismember 1
#define HAVE_sigpending 1
#define HAVE_sigprocmask 1
#define HAVE_sigqueue 1
#define HAVE_sigtimedwait 1
#define HAVE_sigwait 1
#define HAVE_sigwaitinfo 1
#define HAVE_sockatmark 1
#define HAVE_socket 1
#define HAVE_socketpair 1
#define HAVE_stat 1
#define HAVE_sysconf 1

#define HAVE_t_accept 0
/* not referenced: 1 */
#define HAVE_t_alloc 0
#define HAVE_t_bind 0
/* not referenced: 1 */
#define HAVE_t_blocking 0
#define HAVE_t_close 0
#define HAVE_t_connect 0
#define HAVE_t_error 0
#define HAVE_t_free 0
#define HAVE_t_getinfo 0
#define HAVE_t_getprotaddr 0
#define HAVE_t_getstate 0
#define HAVE_t_listen 0
#define HAVE_t_look 0
/* not referenced: 1 */
#define HAVE_t_nonblocking 0
#define HAVE_t_open 0
#define HAVE_t_optmgmt 0
#define HAVE_t_rcv 0
#define HAVE_t_rcvconnect 0
#define HAVE_t_rcvdis 0
#define HAVE_t_rcvrel 0
#define HAVE_t_rcvreldata 0
#define HAVE_t_rcvudata 0
#define HAVE_t_rcvuderr 0
#define HAVE_t_rcvv 0
#define HAVE_t_rcvvudata 0
#define HAVE_t_snd 0
#define HAVE_t_snddis 0
#define HAVE_t_sndrel 0
#define HAVE_t_sndreldata 0
#define HAVE_t_sndudata 0
#define HAVE_t_sndv 0
#define HAVE_t_sndvudata 0
#define HAVE_t_strerror 0
#define HAVE_t_sync 0
#define HAVE_t_unbind 0

#define HAVE_tcdrain 1
#define HAVE_tcflow 1
#define HAVE_tcflush 1
#define HAVE_tcgetattr 1
#define HAVE_tcgetpgrp 1
#define HAVE_tcsendbreak 1
#define HAVE_tcsetattr 1
#define HAVE_tcsetpgrp 1
#define HAVE_time 1
#define HAVE_timer_create 1
#define HAVE_timer_delete 1
#define HAVE_timer_getoverrun 1
#define HAVE_timer_gettime 1
#define HAVE_timer_settime 1
#define HAVE_times 1
#define HAVE_ttyname 1
/* not referenced: 1 */
#define HAVE_ttyname_r 1

#define HAVE_umask 1
#define HAVE_uname 1
#define HAVE_unlink 1
#define HAVE_unsetenv 1
#define HAVE_utime 1

#define HAVE_waitpid 1
#define HAVE_write 1

/* SOLARIS hacks
 */

#define HAVE___posix_ctime_r 0
#define HAVE___posix_readdir_r 0
#define HAVE___posix_sigwait 0
#define HAVE___posix_ttyname_r 0

/* types
 */

#define HAVE_cc_t 1
#define HAVE_clockid_t 1
#define HAVE_clock_t 1
#define HAVE_dev_t 1
#define HAVE_fd_set 1
#define HAVE_in_addr_t 1
#define HAVE_in_port_t 1
#define HAVE_inet_netof 1
#define HAVE_ino_t 1
#define HAVE_mqd_t 1
#define HAVE_nlink_t 1
#define HAVE_pthread_attr_t 1
#define HAVE_pthread_cond_t 1
#define HAVE_pthread_condattr_t 1
#define HAVE_pthread_key_t 1
#define HAVE_pthread_mutex_t 1
#define HAVE_pthread_mutexattr_t 1
#define HAVE_pthread_once_t 1
#define HAVE_pthread_t 1
#define HAVE_sa_family_t 1
#define HAVE_sem_t 1
#define HAVE_siginfo_t 1
#define HAVE_sigset_t 1
#define HAVE_sigval 1
#define HAVE_socklen_t 1
#define HAVE_speed_t 1
#define HAVE_suseconds_t 1
#define HAVE_tcflag_t 1
#define HAVE_timer_t 1

/* error return macros or variables
 */
/* #undef HAVE_t_errno */
/* #undef HAVE_t_nerr */

/* struct types
 */

#define HAVE_struct_addrinfo 1
#define HAVE_struct_aiocb 1
#define HAVE_struct_cmsghdr 1
#define HAVE_struct_dirent 1
#define HAVE_struct_flock 1
#define HAVE_struct_group 1
#define HAVE_struct_hostent 1
#define HAVE_struct_itimerspec 1
#define HAVE_struct_in_addr 1
#define HAVE_struct_ip_opts 1
#define HAVE_struct_iovec 1
#define HAVE_struct_linger 1
#define HAVE_struct_msghdr 1
#define HAVE_struct_mq_attr 1
/* #undef HAVE_struct_netbuf */
#define HAVE_struct_netent 1
#define HAVE_struct_passwd 1
#define HAVE_struct_pollfd 1
#define HAVE_struct_protoent 1
#define HAVE_struct_sched_param 1
#define HAVE_struct_servent 1
#define HAVE_struct_sigaction 1
/* #undef HAVE_struct_cma_sigaction */
#define HAVE_struct_sigevent 1
#define HAVE_struct_sockaddr 1
#define HAVE_struct_sockaddr_in 1
#define HAVE_struct_sockaddr_un 1
#define HAVE_struct_stat 1
#define HAVE_struct_termios 1
#define HAVE_struct_timespec 1
#define HAVE_struct_timeval 1
#define HAVE_struct_tm 1
#define HAVE_struct_tms 1
/* #undef HAVE_struct_t_bind */
/* #undef HAVE_struct_t_call */
/* #undef HAVE_struct_t_discon */
/* #undef HAVE_struct_t_info */
/* #undef HAVE_struct_t_iovec */
/* #undef HAVE_struct_t_kpalive */
/* #undef HAVE_struct_t_linger */
/* #undef HAVE_struct_t_opthdr */
/* #undef HAVE_struct_t_optmgmt */
/* #undef HAVE_struct_t_uderr */
/* #undef HAVE_struct_t_unitdata */
#define HAVE_struct_utimbuf 1
#define HAVE_struct_utsname 1
/* #undef HAVE_component_sa_sigaction */
#define HAVE_component_sigev_notify_function 1
#define HAVE_component_msg_control 1
#define HAVE_component_msg_controllen 1
#define HAVE_component_msg_flags 1
