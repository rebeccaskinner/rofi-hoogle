INSTALL_ROOT ?= $(shell pkg-config --variable=pluginsdir rofi)

CC=gcc

LIBS= glib-2.0         \
      rofi             \
      cairo            \
      rofiHoogleNative \

LIB_CFLAGS= $(shell for lib in ${LIBS}; do pkg-config --cflags $${lib}; done)
LIB_LFLAGS= $(shell for lib in ${LIBS}; do pkg-config --libs $${lib}; done)

CFLAGS= -std=gnu11  \
	-g          \
	-Os         \
	-Wall       \
	-Wextra     \
	-Wno-unused \
	-Werror     \
	${LIB_CFLAGS}

LFLAGS= ${LIB_LFLAGS}

CCLS_FLAGS=

.PHONY: all
all: rofi-hoogle.so

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c $(<) -o $(@)

rofi-hoogle.so: plugin.o
	$(CC) $(LFLAGS) -shared -o $(@) $(<)

.PHONY: clean
clean:
	-rm -f plugin.so
	-rm -f *.o

.PHONY: install
install: rofi-hoogle.so
	cp rofi-hoogle.so ${INSTALL_ROOT}/lib/rofi/rofi-hoogle.so

.PHONY: .ccls
.ccls:
	@echo "clang"
	@for flag in $(CFLAGS) ; do \
		echo $$flag; \
	done
