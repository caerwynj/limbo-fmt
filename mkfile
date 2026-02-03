<$ROOT/mkconfig

TARG=lfmt.dis

MODULES=\
	lfmt.m\
	lfmt.b\

SYSMODULES=\
	arg.m\
	bufio.m\
	draw.m\
	string.m\
	sys.m\

DISBIN=dis

<$ROOT/mkfiles/mkdis

test:V:
	inferno ./lfmt.dis /appl/cmd/ls.b

