# Compiler settings
FPC = fpc
FPCFLAGS = -O3 -XX -Xs

# Directories
SRC_DIR = src
BIN_DIR = bin

# Source and target
TARGET = pasmd5
SOURCE = $(SRC_DIR)/$(TARGET).pas

# Default target
all: $(BIN_DIR)/$(TARGET)

# Ensure bin directory exists
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Compile the program
$(BIN_DIR)/$(TARGET): $(SOURCE) | $(BIN_DIR)
	$(FPC) $(FPCFLAGS) -FE$(BIN_DIR) $(SOURCE)

# Clean build artifacts
clean:
	rm -rf $(BIN_DIR)
	rm -f $(SRC_DIR)/*.o $(SRC_DIR)/*.ppu

# Install target (optional)
install: $(BIN_DIR)/$(TARGET)
	install -d $(DESTDIR)/usr/local/bin
	install $(BIN_DIR)/$(TARGET) $(DESTDIR)/usr/local/bin

.PHONY: all clean install