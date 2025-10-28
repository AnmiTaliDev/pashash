#!/bin/bash

# PasHash Build Configuration Script

set -e

# Color codes
RESET='\033[0m'
BOLD='\033[1m'
DIM='\033[2m'

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'

BOLD_RED='\033[1;31m'
BOLD_GREEN='\033[1;32m'
BOLD_YELLOW='\033[1;33m'
BOLD_BLUE='\033[1;34m'
BOLD_CYAN='\033[1;36m'

# Configuration defaults
PREFIX="/usr/local"
BINDIR=""
FPC="fpc"
FPCFLAGS="-O3 -XX -Xs"
BUILD_TYPE="Release"
ENABLE_DEBUG="OFF"
ENABLE_VERBOSE="OFF"
ENABLE_STATIC="OFF"
STRIP_BINARY="OFF"
SRC_DIR="src"
BIN_DIR="bin"
TARGET="pashash"

# Detection results
FPC_VERSION=""
FPC_PATH=""
PLATFORM=""
ARCH=""

# Helper Functions

print_header() {
    echo
    echo -e "${BOLD_CYAN}$1${RESET}"
    echo
}

print_section() {
    echo -e "${BOLD}-- $1${RESET}"
}

print_status() {
    local status=$1
    local message=$2
    if [ "$status" = "OK" ]; then
        echo -e "   ${BOLD_GREEN}[OK]${RESET} ${message}"
    elif [ "$status" = "WARN" ]; then
        echo -e "   ${BOLD_YELLOW}[WARN]${RESET} ${message}"
    elif [ "$status" = "ERROR" ]; then
        echo -e "   ${BOLD_RED}[ERROR]${RESET} ${message}"
    elif [ "$status" = "INFO" ]; then
        echo -e "   ${BOLD_BLUE}[INFO]${RESET} ${message}"
    fi
}

print_config() {
    local key=$1
    local value=$2
    printf "   ${BOLD}%-25s${RESET} : ${CYAN}%s${RESET}\n" "$key" "$value"
}

print_error() {
    echo
    echo -e "${BOLD_RED}ERROR: $1${RESET}"
    echo
    exit 1
}

# Help Message

show_help() {
    cat << EOF
${BOLD}PasHash Configuration Script${RESET}

${BOLD}USAGE:${RESET}
    ./configure.sh [OPTIONS]

${BOLD}OPTIONS:${RESET}
    --prefix=PREFIX           Installation prefix (default: /usr/local)
    --bindir=DIR             Binary installation directory (default: PREFIX/bin)
    --fpc=PATH               Path to Free Pascal Compiler (default: auto-detect)
    --build-type=TYPE        Build type: Release, Debug (default: Release)
    --enable-debug           Enable debug symbols in Release build
    --enable-verbose         Enable verbose compilation output
    --enable-static          Enable static linking
    --enable-strip           Strip binary after compilation
    --help                   Show this help message

${BOLD}BUILD TYPES:${RESET}
    Release                  Optimized build with -O3 -XX -Xs flags
    Debug                    Debug build with -g -gl -O- flags

${BOLD}EXAMPLES:${RESET}
    ./configure.sh
    ./configure.sh --prefix=/usr --build-type=Debug
    ./configure.sh --fpc=/usr/local/bin/fpc --enable-verbose
    ./configure.sh --enable-static --enable-strip
    ./configure.sh --build-type=Release --enable-debug

For detailed documentation, see BUILD.md

EOF
    exit 0
}

# Argument Parsing

parse_arguments() {
    for arg in "$@"; do
        case $arg in
            --prefix=*)
                PREFIX="${arg#*=}"
                ;;
            --bindir=*)
                BINDIR="${arg#*=}"
                ;;
            --fpc=*)
                FPC="${arg#*=}"
                ;;
            --build-type=*)
                BUILD_TYPE="${arg#*=}"
                ;;
            --enable-debug)
                ENABLE_DEBUG="ON"
                ;;
            --enable-verbose)
                ENABLE_VERBOSE="ON"
                ;;
            --enable-static)
                ENABLE_STATIC="ON"
                ;;
            --enable-strip)
                STRIP_BINARY="ON"
                ;;
            --help)
                show_help
                ;;
            *)
                print_error "Unknown option: $arg (use --help for usage)"
                ;;
        esac
    done

    if [ -z "$BINDIR" ]; then
        BINDIR="$PREFIX/bin"
    fi
}

# System Detection

detect_system() {
    print_section "Detecting System Configuration"

    if [ "$(uname -s)" = "Linux" ]; then
        PLATFORM="Linux"
    elif [ "$(uname -s)" = "Darwin" ]; then
        PLATFORM="macOS"
    elif [ "$(uname -s)" = "FreeBSD" ]; then
        PLATFORM="FreeBSD"
    else
        PLATFORM="$(uname -s)"
    fi
    print_status "OK" "Platform: ${PLATFORM}"

    ARCH="$(uname -m)"
    print_status "OK" "Architecture: ${ARCH}"
    echo
}

# Compiler Detection

detect_compiler() {
    print_section "Checking for Free Pascal Compiler"

    if ! command -v "$FPC" &> /dev/null; then
        print_status "ERROR" "Free Pascal Compiler not found: $FPC"
        print_error "Please install FPC or specify path with --fpc=PATH"
    fi

    FPC_PATH="$(command -v "$FPC")"
    print_status "OK" "Found FPC: ${FPC_PATH}"

    FPC_VERSION=$($FPC -iV 2>/dev/null || echo "unknown")
    print_status "OK" "Version: ${FPC_VERSION}"

    local major minor
    IFS='.' read -r major minor _ <<< "$FPC_VERSION"

    if [ "$major" -lt 3 ] || ([ "$major" -eq 3 ] && [ "$minor" -lt 2 ]); then
        print_status "WARN" "FPC version 3.2+ recommended (found: ${FPC_VERSION})"
    else
        print_status "OK" "Version check passed"
    fi
    echo
}

# Source Files Check

check_sources() {
    print_section "Checking Source Files"

    if [ ! -d "$SRC_DIR" ]; then
        print_status "ERROR" "Source directory not found: $SRC_DIR"
        print_error "Source directory is missing"
    fi
    print_status "OK" "Source directory: ${SRC_DIR}/"

    local main_source="$SRC_DIR/$TARGET.pas"
    if [ ! -f "$main_source" ]; then
        print_status "ERROR" "Main source file not found: $main_source"
        print_error "Main source file is missing"
    fi
    print_status "OK" "Main source: ${main_source}"

    local pas_count=$(find "$SRC_DIR" -name "*.pas" 2>/dev/null | wc -l)
    print_status "INFO" "Found ${pas_count} Pascal source file(s)"
    echo
}

# Build Flags Configuration

configure_build_flags() {
    print_section "Configuring Build Flags"

    FPCFLAGS=""

    case "$BUILD_TYPE" in
        Release)
            FPCFLAGS="-O3 -XX -Xs"
            print_status "OK" "Build type: Release (optimized)"
            ;;
        Debug)
            FPCFLAGS="-g -gl -O-"
            ENABLE_DEBUG="ON"
            print_status "OK" "Build type: Debug (with symbols)"
            ;;
        *)
            print_status "WARN" "Unknown build type: $BUILD_TYPE, using Release"
            FPCFLAGS="-O3 -XX -Xs"
            BUILD_TYPE="Release"
            ;;
    esac

    if [ "$ENABLE_DEBUG" = "ON" ] && [ "$BUILD_TYPE" = "Release" ]; then
        FPCFLAGS="$FPCFLAGS -g -gl"
        print_status "OK" "Debug symbols: enabled"
    fi

    if [ "$ENABLE_VERBOSE" = "ON" ]; then
        FPCFLAGS="$FPCFLAGS -vwni"
        print_status "OK" "Verbose output: enabled"
    fi

    if [ "$ENABLE_STATIC" = "ON" ]; then
        FPCFLAGS="$FPCFLAGS -static"
        print_status "OK" "Static linking: enabled"
    fi

    print_status "INFO" "Compiler flags: ${FPCFLAGS}"
    echo
}

# Generate Makefile

generate_makefile() {
    print_section "Generating Makefile"

    local strip_command=""
    if [ "$STRIP_BINARY" = "ON" ]; then
        strip_command="	@echo \"Stripping binary...\"\n	@strip \$(BIN_DIR)/\$(TARGET)"
    fi

    cat > Makefile << EOF
# Makefile generated by configure.sh on $(date)
# DO NOT EDIT - regenerate with ./configure.sh

# Configuration
FPC = $FPC
FPCFLAGS = $FPCFLAGS
PREFIX = $PREFIX
BINDIR = $BINDIR
SRC_DIR = $SRC_DIR
BIN_DIR = $BIN_DIR
TARGET = $TARGET

# Build info
BUILD_TYPE = $BUILD_TYPE
PLATFORM = $PLATFORM
ARCH = $ARCH

# Source files
SOURCE = \$(SRC_DIR)/\$(TARGET).pas

# Default target
all: \$(BIN_DIR)/\$(TARGET)

# Ensure bin directory exists
\$(BIN_DIR):
	@mkdir -p \$(BIN_DIR)

# Compile the program
\$(BIN_DIR)/\$(TARGET): \$(SOURCE) | \$(BIN_DIR)
	@echo ""
	@echo "\033[1;36mBuilding PasHash\033[0m"
	@echo ""
	@echo "\033[1m-- Compiling \$(TARGET)\033[0m"
	@echo "   \033[0;36mCompiler:\033[0m \$(FPC)"
	@echo "   \033[0;36mFlags:\033[0m    \$(FPCFLAGS)"
	@echo ""
	@\$(FPC) \$(FPCFLAGS) -FE\$(BIN_DIR) \$(SOURCE)
$strip_command
	@echo ""
	@echo "\033[1;32mBuild completed successfully!\033[0m"
	@echo ""
	@echo "   \033[1mBinary:\033[0m \033[1;36m\$(BIN_DIR)/\$(TARGET)\033[0m"
	@if [ -f "\$(BIN_DIR)/\$(TARGET)" ]; then \\
		SIZE=\$\$(du -h "\$(BIN_DIR)/\$(TARGET)" | cut -f1); \\
		echo "   \033[1mSize:\033[0m   \$\$SIZE"; \\
	fi
	@echo ""

# Install target
install: \$(BIN_DIR)/\$(TARGET)
	@echo ""
	@echo "\033[1;36mInstalling PasHash\033[0m"
	@echo ""
	@echo "\033[1m-- Installing to: \033[0;36m\$(BINDIR)\033[0m"
	@if [ ! -w "\$\$(dirname "\$(BINDIR)")" ] && [ ! -w "\$(BINDIR)" 2>/dev/null ]; then \\
		echo "   \033[1;33m[WARN]\033[0m Requires root privileges"; \\
		echo ""; \\
		sudo install -d "\$(BINDIR)"; \\
		sudo install -m 755 "\$(BIN_DIR)/\$(TARGET)" "\$(BINDIR)/\$(TARGET)"; \\
	else \\
		install -d "\$(BINDIR)"; \\
		install -m 755 "\$(BIN_DIR)/\$(TARGET)" "\$(BINDIR)/\$(TARGET)"; \\
	fi
	@echo ""
	@echo "\033[1;32mInstallation completed successfully!\033[0m"
	@echo ""
	@echo "   \033[1mInstalled to:\033[0m \033[1;36m\$(BINDIR)/\$(TARGET)\033[0m"
	@echo ""

# Uninstall target
uninstall:
	@echo ""
	@echo "\033[1;36mUninstalling PasHash\033[0m"
	@echo ""
	@if [ -f "\$(BINDIR)/\$(TARGET)" ]; then \\
		if [ ! -w "\$(BINDIR)" ]; then \\
			echo "   \033[1;33m[WARN]\033[0m Requires root privileges"; \\
			echo ""; \\
			sudo rm -f "\$(BINDIR)/\$(TARGET)"; \\
		else \\
			rm -f "\$(BINDIR)/\$(TARGET)"; \\
		fi; \\
		echo "\033[1;32mUninstall completed!\033[0m"; \\
		echo ""; \\
		echo "   \033[1mRemoved:\033[0m \$(BINDIR)/\$(TARGET)"; \\
	else \\
		echo "\033[1;33m[WARN]\033[0m Binary not found: \$(BINDIR)/\$(TARGET)"; \\
	fi
	@echo ""

# Clean build artifacts
clean:
	@echo ""
	@echo "\033[1;36mCleaning PasHash\033[0m"
	@echo ""
	@echo "\033[1m-- Removing build artifacts\033[0m"
	@if [ -d "\$(BIN_DIR)" ]; then \\
		rm -rf "\$(BIN_DIR)"; \\
		echo "   \033[1;32m[OK]\033[0m Removed: \$(BIN_DIR)/"; \\
	fi
	@find "\$(SRC_DIR)" -type f \\( -name "*.o" -o -name "*.ppu" \\) -delete 2>/dev/null || true
	@echo "   \033[1;32m[OK]\033[0m Removed: *.o, *.ppu files"
	@echo ""
	@echo "\033[1;32mClean completed!\033[0m"
	@echo ""

# Distclean - remove generated files
distclean: clean
	@echo "\033[1m-- Removing generated configuration\033[0m"
	@rm -f Makefile
	@echo "   \033[1;32m[OK]\033[0m Removed: Makefile"
	@echo ""

# Show configuration
config:
	@echo ""
	@echo "\033[1;36mBuild Configuration\033[0m"
	@echo ""
	@echo "\033[1mSystem:\033[0m"
	@echo "   \033[1mPlatform\033[0m              : \033[0;36m\$(PLATFORM)\033[0m"
	@echo "   \033[1mArchitecture\033[0m          : \033[0;36m\$(ARCH)\033[0m"
	@echo ""
	@echo "\033[1mCompiler:\033[0m"
	@echo "   \033[1mFPC Path\033[0m              : \033[0;36m\$(FPC)\033[0m"
	@echo "   \033[1mFPC Flags\033[0m             : \033[0;36m\$(FPCFLAGS)\033[0m"
	@echo ""
	@echo "\033[1mBuild:\033[0m"
	@echo "   \033[1mBuild Type\033[0m            : \033[0;36m\$(BUILD_TYPE)\033[0m"
	@echo ""
	@echo "\033[1mDirectories:\033[0m"
	@echo "   \033[1mSource Directory\033[0m      : \033[0;36m\$(SRC_DIR)\033[0m"
	@echo "   \033[1mBuild Directory\033[0m       : \033[0;36m\$(BIN_DIR)\033[0m"
	@echo "   \033[1mInstall Prefix\033[0m        : \033[0;36m\$(PREFIX)\033[0m"
	@echo "   \033[1mBinary Directory\033[0m      : \033[0;36m\$(BINDIR)\033[0m"
	@echo ""

# Help target
help:
	@echo ""
	@echo "\033[1mPasHash Makefile Targets:\033[0m"
	@echo ""
	@echo "   \033[1;36mall\033[0m         Build the project (default)"
	@echo "   \033[1;36minstall\033[0m     Install binary to \$(BINDIR)"
	@echo "   \033[1;36muninstall\033[0m   Uninstall binary from \$(BINDIR)"
	@echo "   \033[1;36mclean\033[0m       Remove build artifacts"
	@echo "   \033[1;36mdistclean\033[0m   Remove build artifacts and Makefile"
	@echo "   \033[1;36mconfig\033[0m      Show build configuration"
	@echo "   \033[1;36mhelp\033[0m        Show this help message"
	@echo ""
	@echo "To reconfigure, run: \033[1;36m./configure.sh\033[0m"
	@echo ""

.PHONY: all install uninstall clean distclean config help
EOF

    print_status "OK" "Generated: Makefile"
    echo
}

# Print Summary

print_summary() {
    print_header "Configuration Summary"

    echo -e "${BOLD}System:${RESET}"
    print_config "Platform" "$PLATFORM"
    print_config "Architecture" "$ARCH"

    echo
    echo -e "${BOLD}Compiler:${RESET}"
    print_config "FPC Path" "$FPC_PATH"
    print_config "FPC Version" "$FPC_VERSION"
    print_config "FPC Flags" "$FPCFLAGS"

    echo
    echo -e "${BOLD}Build Configuration:${RESET}"
    print_config "Build Type" "$BUILD_TYPE"
    print_config "Debug Symbols" "$ENABLE_DEBUG"
    print_config "Verbose Output" "$ENABLE_VERBOSE"
    print_config "Static Linking" "$ENABLE_STATIC"
    print_config "Strip Binary" "$STRIP_BINARY"

    echo
    echo -e "${BOLD}Directories:${RESET}"
    print_config "Source Directory" "$SRC_DIR"
    print_config "Build Directory" "$BIN_DIR"
    print_config "Install Prefix" "$PREFIX"
    print_config "Binary Directory" "$BINDIR"

    echo
    echo -e "${BOLD_GREEN}Configuration completed successfully!${RESET}"
    echo
    echo -e "${BOLD}Next steps:${RESET}"
    echo -e "   ${CYAN}1.${RESET} Run ${BOLD_CYAN}make${RESET} to compile"
    echo -e "   ${CYAN}2.${RESET} Run ${BOLD_CYAN}make install${RESET} to install (may need sudo)"
    echo -e "   ${CYAN}3.${RESET} Run ${BOLD_CYAN}make clean${RESET} to clean build files"
    echo -e "   ${CYAN}4.${RESET} Run ${BOLD_CYAN}make help${RESET} to see all available targets"
    echo
}

# Main Execution

main() {
    print_header "PasHash Configuration"

    parse_arguments "$@"
    detect_system
    detect_compiler
    check_sources
    configure_build_flags
    generate_makefile
    print_summary
}

main "$@"
