
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	atomic.stdout.exp atomic.stderr.exp atomic.vgtest \
	branch.stdout.exp branch.stderr.exp branch.vgtest \
	float.stdout.exp float.stderr.exp float.vgtest \
	integer.stdout.exp integer.stderr.exp integer.vgtest \
	memory.stdout.exp memory.stderr.exp memory.vgtest

check_PROGRAMS = \
	allexec \
	atomic \
	branch \
	float \
	integer \
	memory

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
