SMLSHARP=/usr/local/bin/smlsharp
LDFLAGS=
SRC=main.sml jit.sml emit.sml inst.sml asm.sml att.sml
OBJ=$(SRC:.sml=.o)
TEST=test/main.sml test/it.sml
TEST_OBJ=$(TEST:.sml=.o)

TARGET=main
TEST_TARGET=test/test


all: $(TARGET)
test: $(TEST_TARGET)

$(TARGET): $(OBJ)
	$(SMLSHARP) $(LDFLAGS) main.smi -o $(TARGET)

$(TEST_TARGET): $(TEST_OBJ) $(OBJ)
	$(SMLSHARP) $(LDFLAGS) test/main.smi -o $(TEST_TARGET)
	./$(TEST_TARGET)

%.o: %.sml %.smi
	$(SMLSHARP) $(SMLSHARP_CFLAGS) $(SMLSHARP_FLAGS) -c -o $@ $<

clean:
	rm -f $(OBJ) $(TEST_OBJ)
	rm -f $(TARGET) $(TEST_TARGET)

.PHONY: all test clean	
