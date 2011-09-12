VERSION=$(shell grep -P -o '(?<=Version: )[0-9.]+' pony-mode.el)
RELEASEDIR=pony-mode-$(VERSION)
BUILDDIR=build/$(RELEASEDIR)

release: clean package tag

all:
	@echo "What do you want from me?"

tag:
	$(shell git tag $(VERSION))

clean:
	-@ rm -rf build

package:
	-@ mkdir -p $(BUILDDIR)
	-@ cp pony-mode.el $(BUILDDIR)
	-@ cp pony-mode-pkg.el $(BUILDDIR)
	-@ cp -ra snippets $(BUILDDIR)
	-@ cp README.rst $(BUILDDIR)/README
	-@ cd build && tar -cf pony-mode-$(VERSION).tar $(RELEASEDIR)