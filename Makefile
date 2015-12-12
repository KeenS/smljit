SMLSHARP=/usr/local/bin/smlsharp
LDFLAGS=
TARGET=main

all: $(TARGET)

$(TARGET): jit.o main.o emit.o inst.o asm.o att.o
	$(SMLSHARP) $(LDFLAGS) main.smi -o $(TARGET)

%.o: %.sml %.smi
	$(SMLSHARP) $(SMLSHARP_CFLAGS) $(SMLSHARP_FLAGS) -c -o $@ $<

clean:
	find . -name '*.o' | xargs rm -f
	rm -rf $(TARGET)
