SMLSHARP=/usr/local/bin/smlsharp
LDFLAGS=
TARGET=main

all: jit

jit: jit.o main.o
	$(SMLSHARP) $(LDFLAGS) main.smi -o $(TARGET)

%.o: %.sml %.smi
	$(SMLSHARP) $(SMLSHARP_CFLAGS) $(SMLSHARP_FLAGS) -c -o $@ $<
