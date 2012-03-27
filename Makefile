LISP=/usr/local/bin/sbcl

clisp_BUILDOPTS=-K base -on-error exit ./make-image.lisp
sbcl_BUILDOPTS=--load ./make-image.lisp

clisp_INFOOPTS=-K full -on-error exit -x "(require 'asdf '(\"asdf.lisp\")) (load \"dswm.asd\") (load \"@PPCRE_PATH@/cl-ppcre.asd\") (asdf:operate 'asdf:load-op :dswm) (load (compile-file \"manual.lisp\")) (dswm::generate-manual) (ext:exit)"
sbcl_INFOOPTS=--eval "(progn (require 'asdf) (require 'dswm) (load \"manual.lisp\"))" --eval "(progn (dswm::generate-manual) (sb-ext:quit))"
datarootdir = ${prefix}/share
prefix=/usr/local
exec_prefix= ${prefix}
bindir=${exec_prefix}/bin
infodir=${datarootdir}/info

root_dir=${PWD}
dest_dir=dswm-0.0.4-git

# You shouldn't have to edit past this

# This is copied from the .asd file. It'd be nice to have the list in
# one place, but oh well.
FILES=dswm.asd package.lisp primitives.lisp wrappers.lisp		\
pathnames.lisp keysyms.lisp keytrans.lisp kmap.lisp input.lisp		\
core.lisp command.lisp menu.lisp screen.lisp head.lisp group.lisp	\
window.lisp floating-group.lisp tile-window.lisp window-placement.lisp	\
message-window.lisp selection.lisp user.lisp iresize.lisp		\
bindings.lisp events.lisp help.lisp fdump.lisp mode-line.lisp		\
time.lisp color.lisp module.lisp dswm.lisp

all: deps dswm

deps:	install-deps.lisp
	$(LISP) $(sbcl_BUILDOPTS)

dswm.info: dswm.texi
	makeinfo dswm.texi

# FIXME: This rule is too hardcoded
dswm.texi: dswm.texi.in
	$(LISP) $(sbcl_INFOOPTS)

dswm: $(FILES)
	$(LISP) $(sbcl_BUILDOPTS)

release:
	git tag -a -m "version 0.0.4-git" 0.0.4-git
	git archive --format=tar --prefix=dswm-0.0.4-git/ HEAD > dswm-0.0.4-git.tar
	tar -xf dswm-0.0.4-git.tar
	rm -rf dswm-0.0.4-git/.git* dswm-0.0.4-git/autom4te.cache dswm-0.0.4-git/version.lisp dswm-0.0.4-git/module.lisp dswm-0.0.4-git/make-image.lisp dswm-0.0.4-git/dswm.desktop
	git log > dswm-0.0.4-git/ChangeLog
	cp configure dswm-0.0.4-git/
	tar -zcf dswm-0.0.4-git.tar.gz dswm-0.0.4-git
	tar -jcf dswm-0.0.4-git.tar.bz2 dswm-0.0.4-git
	rm -fr dswm-0.0.4-git/ dswm-0.0.4-git.tar
release-upload:
#	gpg -b dswm-0.0.4-git.tgz
	ssh cosmonaut-ok,dswm@shell.sourceforge.net create
	ssh cosmonaut-ok,dswm@shell.sourceforge.net test -d /home/frs/project/d/ds/dswm/0.0.4-git || mkdir /home/frs/project/d/ds/dswm/0.0.4-git/
	scp dswm-0.0.4-git.tar.gz cosmonaut-ok,dswm@frs.sourceforge.net:/home/frs/project/d/ds/dswm/0.0.4-git/dswm-0.0.4-git.tar.gz
	scp dswm-0.0.4-git.tar.bz2 cosmonaut-ok,dswm@frs.sourceforge.net:/home/frs/project/d/ds/dswm/0.0.4-git/dswm-0.0.4-git.tar.bz2
testbuild:
	cd $(dirname $0)
	rm -rf ~/.cache/common-lisp/
	test -z "/tmp/$(dest_dir).build" || rm -rf /tmp/$(dest_dir).build
	cp -r $(root_dir) /tmp/$(dest_dir).build
	cd /tmp/$(dest_dir).build
	autoconf
	./configure --prefix=/usr/local
	printf '\n\nNow, run \"cd /tmp/$(dest_dir).build && sudo rm -rf /etc/dss/dswm && sudo make install && cd $(root_dir)\" to install DSWM\n\n'
clean:
	rm -f *.fasl *.fas *.lib *.*fsl
	rm -f *.log *.fns *.fn *.aux *.cp *.ky *.log *.toc *.pg *.tp *.vr *.vrs
	rm -rf dswm dswm.texi dswm.info autom4te.cache  *.tar.* version.lisp module.lisp make-image.lisp help.lisp dswm.desktop
install: dswm.info dswm
	test -z "$(destdir)$(bindir)" || mkdir -p "$(destdir)$(bindir)"
	install -m 755 dswm "$(destdir)$(bindir)"
	test -z "$(destdir)$(infodir)" || mkdir -p "$(destdir)$(infodir)"
	install -m 644 dswm.info "$(destdir)$(infodir)"
	install-info --info-dir="$(destdir)$(infodir)" "$(destdir)$(infodir)/dswm.info"
	test -d "/etc/dss/dswm/" || mkdir -p "/etc/dss/dswm"
	test -f "/etc/dss/dswm/dswm.lisp" || install -m 644 dswm.conf "/etc/dss/dswm/dswm.lisp"
	mkdir -p "$(destdir)$(datarootdir)/dswm/modules"
	install -m 644 COPYING "$(destdir)$(datarootdir)/dswm/"
	install -m 644 AUTHORS "$(destdir)$(datarootdir)/dswm/"
	## Install dswm.desktop for desktop managers
	test -d "$(destdir)$(datarootdir)/xsessions" && install -m 644 dswm.desktop "$(destdir)$(datarootdir)/xsessions" || install -m 644 dswm.desktop "/usr/share/xsessions/"
	## Install dswm.desktop for KDM (f*cking KDE)
	test -d "$(destdir)$(datarootdir)/apps/kdm/sessions" && install -m 644 dswm.desktop "$(destdir)$(datarootdir)/apps/kdm/sessions" || test -d "/usr/share/apps/kdm/sessions" && install -m 644 dswm.desktop "/usr/share/apps/kdm/sessions" || true

uninstall:
	rm "$(destdir)$(bindir)/dswm"
	rm -r "$(destdir)$(datarootdir)/dswm"
	rm "/etc/dss/dswm/dswm.lisp"
	install-info --info-dir="$(destdir)$(infodir)" --remove "$(destdir)$(infodir)/dswm.info"
	rm "$(destdir)$(infodir)/dswm.info"
	test -f "$(destdir)$(datarootdir)/xsessions/dswm.desktop" && "$(destdir)$(datarootdir)/xsessions/dswm.desktop" || test -f "/usr/share/xsessions/dswm.desktop" && rm "/usr/share/xsessions/dswm.desktop"
	test -f "$(destdir)$(datarootdir)/apps/kdm/sessions/dswm.desktop" && rm "$(destdir)$(datarootdir)/apps/kdm/sessions/dswm.desktop" || test -f "/usr/share/apps/kdm/sessions/dswm.desktop" && rm "/usr/share/apps/kdm/sessions/dswm.desktop" || true

# End of file
