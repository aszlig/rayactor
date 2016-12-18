einfo = $(shell ENABLE_VALGRIND= erl -noshell -s init stop \
		        -eval "io:format(\"~s\", [$1]).")

ERTS_ROOT ?= $(call einfo,code:root_dir())
ERTS_VERSION ?= $(call einfo,erlang:system_info(version))
ERTS_INCLUDE_DIR ?= $(ERTS_ROOT)/erts-$(ERTS_VERSION)/include
ERL_INTERFACE_INCLUDE_DIR ?= $(call einfo,code:lib_dir(erl_interface, include))
ERL_INTERFACE_LIB_DIR ?= $(call einfo,code:lib_dir(erl_interface, lib))

REBAR_DEPS_DIR ?= ../..
BUILDOUT = $(REBAR_DEPS_DIR)/ftdi

OBJECT_DIR = $(BUILDOUT)/c_obj
TARGET_DIR = $(BUILDOUT)/priv
GENERATED = $(BUILDOUT)/c_src

SOURCES = $(wildcard *.c)
OBJECTS = $(patsubst %.c,$(OBJECT_DIR)/%.o,$(SOURCES))

CFLAGS += -std=c11 -Wall -Wstrict-prototypes -fPIC
CFLAGS += -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(GENERATED)
CFLAGS += `pkg-config --cflags libftdi1`

LDFLAGS += -shared `pkg-config --libs libftdi1`

OUTPUT = $(TARGET_DIR)/ftdi_drv.so

all: CFLAGS += -O3 -finline-functions
all: $(OUTPUT)

debug: CFLAGS += -ggdb
debug: $(OUTPUT)

$(OUTPUT): $(OBJECTS)
	@[ ! -L $(TARGET_DIR) ] || unlink $(TARGET_DIR)
	@mkdir -p $(TARGET_DIR)
	$(CC) $(LDFLAGS) -o $@ $^

$(OBJECT_DIR)/%.o: %.c
	@mkdir -p $(OBJECT_DIR)
	$(CC) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<

$(OBJECT_DIR)/ftdi_drv.o: $(GENERATED)/ftdi_drv.h context.h transfer.h
$(OBJECT_DIR)/transfer.o: context.h transfer.h

$(GENERATED)/ftdi_drv.h: $(CURDIR)/../src/ftdi.hrl
	@mkdir -p $(GENERATED)
	sed -n 's/^-define( *\([A-Z0-9_]\+\) *, *\([^ )]\+\).*/#define \1 \2/p' \
		$^ > $@

.PHONY: clean
clean:
	$(RM) $(OUTPUT) $(OBJECTS)
