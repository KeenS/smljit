SMLSHARP=/usr/local/bin/smlsharp
LDFLAGS=

all: jit

jit: jit.o main.o
	$(SMLSHARP) $(LDFLAGS) -o jit jit.smi

%.o: %.sml %.smi
	$(SMLSHARP) $(SMLSHARP_CFLAGS) $(SMLSHARP_FLAGS) -c -o $@ $<
