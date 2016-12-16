einfo = $(shell ENABLE_VALGRIND= erl -noshell -s init stop \
		        -eval "io:format(\"~s\", [$1]).")

ERTS_ROOT ?= $(call einfo,code:root_dir())
ERTS_VERSION ?= $(call einfo,erlang:system_info(version))
ERTS_INCLUDE_DIR ?= $(ERTS_ROOT)/erts-$(ERTS_VERSION)/include
ERL_INTERFACE_INCLUDE_DIR ?= $(call einfo,code:lib_dir(erl_interface, include))
ERL_INTERFACE_LIB_DIR ?= $(call einfo,code:lib_dir(erl_interface, lib))

SOURCES = $(wildcard *.c)
OBJECTS = $(patsubst %.c,%.o,$(SOURCES))

CFLAGS += -O3 -std=c11 -finline-functions -Wall -Wstrict-prototypes -fPIC
CFLAGS += -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
CFLAGS += `pkg-config --cflags libftdi1`

LDFLAGS += -shared `pkg-config --libs libftdi1`

OUTPUT = $(CURDIR)/../priv/ftdi_drv.so

$(OUTPUT): $(OBJECTS)
	mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) -o $@ $^

ftdi_drv.o: ftdi_drv.h context.h transfer.h
transfer.o: context.h transfer.h

ftdi_drv.h: $(CURDIR)/../src/ftdi.hrl
	sed -n 's/^-define( *\([A-Z0-9_]\+\) *, *\([^ )]\+\).*/#define \1 \2/p' \
		$^ > $@

.PHONY: clean
clean:
	$(RM) $(OUTPUT) $(OBJECTS)