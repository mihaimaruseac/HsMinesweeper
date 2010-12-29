.PHONY: all clean

TARGET = ./Minesweeper

all: $(TARGET)

test: $(TARGET)
	$(TARGET)

$(TARGET): *.hs
	ghc --make *.hs

clean:
	$(RM) *.hi *.o $(TARGET)

