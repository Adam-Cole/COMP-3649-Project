#Variables
SRC = main
OUT = main

# Target: compile the full program
all:
	ghc -o $(OUT) -no-keep-hi-files -no-keep-o-files $(SRC)

# Clean up: delete executable if needed
clean:
	rm -f $(OUT)

# Run the compiled program
run: all
	./$(OUT)
