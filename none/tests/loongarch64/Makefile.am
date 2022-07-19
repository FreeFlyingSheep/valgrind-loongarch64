
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	atomic.stdout.exp atomic.stderr.exp atomic.vgtest \
	branch.stdout.exp branch.stderr.exp branch.vgtest \
	cpucfg.stdout.exp cpucfg.stderr.exp cpucfg.vgtest \
	fault.stdout.exp fault.stderr.exp fault.vgtest \
	fault_fp.stdout.exp fault_fp.stderr.exp fault_fp.vgtest \
	float.stdout.exp float.stderr.exp float.vgtest \
	integer.stdout.exp integer.stderr.exp integer.vgtest \
	llsc.stdout.exp llsc.stderr.exp llsc.vgtest \
	memory.stdout.exp memory.stderr.exp memory.vgtest \
	move.stdout.exp move.stderr.exp move.vgtest \
	pc.stdout.exp pc.stderr.exp pc.vgtest \
	special.stdout.exp special.stderr.exp special.vgtest

check_PROGRAMS = \
	allexec \
	atomic \
	branch \
	cpucfg \
	fault \
	fault_fp \
	float \
	integer \
	llsc \
	memory \
	move \
	pc \
	special

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
