.PHONY: all clean

TARGET = ./Minesweeper

all: $(TARGET)

test1: $(TARGET)
	$(TARGET) 16 4 5 8 42

test2: $(TARGET)
	$(TARGET) 25 2 5 8 42

test3: $(TARGET)
	$(TARGET) 46 1 10 8 30

$(TARGET): *.hs
	ghc --make *.hs

clean:
	$(RM) *.hi *.o $(TARGET)

