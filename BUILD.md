# Building PasHash

This document describes how to build PasHash from source using the configure script and make.

## Quick Start

```bash
./configure.sh
make
```

## Requirements

- **Free Pascal Compiler (FPC)** version 3.2.0 or higher
- Standard POSIX-compatible shell (bash, sh)
- make utility

## Build Process

PasHash uses a configure script similar to autotools to generate a Makefile tailored to your system.

### Step 1: Configuration

Run the configure script:

```bash
./configure.sh [OPTIONS]
```

The configure script will:
- Detect your system platform and architecture
- Check for Free Pascal Compiler
- Verify source files
- Generate a Makefile with your specified options

### Step 2: Compilation

After successful configuration, build the project:

```bash
make
```

The compiled binary will be placed in `bin/pashash`.

### Step 3: Installation (Optional)

To install system-wide:

```bash
make install
```

Note: This may require root privileges depending on the installation prefix.

## Configure Script Options

### General Options

#### `--help`

Display help message with all available options and exit.

```bash
./configure.sh --help
```

### Installation Directories

#### `--prefix=PREFIX`

Set the installation prefix directory.

- **Default:** `/usr/local`
- **Example:** `./configure.sh --prefix=/usr`

This determines where files will be installed. The binary will be installed to `PREFIX/bin` by default.

**Common values:**
- `/usr/local` - Local installations (default)
- `/usr` - System-wide installations
- `$HOME/.local` - User-local installations

#### `--bindir=DIR`

Override the binary installation directory.

- **Default:** `PREFIX/bin`
- **Example:** `./configure.sh --bindir=/opt/bin`

Use this if you want to install the binary to a specific location different from `PREFIX/bin`.

### Compiler Configuration

#### `--fpc=PATH`

Specify the path to the Free Pascal Compiler.

- **Default:** Auto-detected from PATH
- **Example:** `./configure.sh --fpc=/usr/local/bin/fpc`

Use this option if:
- FPC is not in your PATH
- You have multiple FPC installations
- You want to use a specific FPC version

### Build Type

#### `--build-type=TYPE`

Set the build type (Release or Debug).

- **Default:** `Release`
- **Valid values:** `Release`, `Debug`

**Release Build:**
```bash
./configure.sh --build-type=Release
```

Compiler flags: `-O3 -XX -Xs`
- `-O3` - Level 3 optimization (maximum optimization)
- `-XX` - Enable smart linking
- `-Xs` - Strip all symbols from executable

Produces an optimized binary with smaller size and better performance.

**Debug Build:**
```bash
./configure.sh --build-type=Debug
```

Compiler flags: `-g -gl -O-`
- `-g` - Generate debug information
- `-gl` - Use line info unit (better backtraces)
- `-O-` - Disable optimizations

Produces a binary with debug symbols, useful for development and debugging.

### Build Options

#### `--enable-debug`

Enable debug symbols in a Release build.

```bash
./configure.sh --build-type=Release --enable-debug
```

This adds `-g -gl` flags to a Release build, giving you an optimized binary that still contains debug information for troubleshooting.

#### `--enable-verbose`

Enable verbose compilation output.

```bash
./configure.sh --enable-verbose
```

Adds `-vwni` flag which shows:
- Warnings
- Notes
- Informational messages

Useful for:
- Debugging build issues
- Understanding what the compiler is doing
- Catching potential problems

#### `--enable-static`

Enable static linking.

```bash
./configure.sh --enable-static
```

Adds `-static` flag to link all libraries statically. This creates a standalone binary that doesn't depend on shared libraries.

**Benefits:**
- Binary works on systems without required shared libraries
- No library version conflicts
- Easier distribution

**Drawbacks:**
- Larger binary size
- No automatic security updates from shared libraries

#### `--enable-strip`

Strip the binary after compilation.

```bash
./configure.sh --enable-strip
```

Runs `strip` command on the compiled binary to remove all symbol table and relocation information.

**Benefits:**
- Significantly reduces binary size
- Makes reverse engineering harder

**Drawbacks:**
- No debug symbols (even with `--enable-debug`)
- Can't get meaningful backtraces

Note: Don't use with `--enable-debug` unless you only need debug info during compilation.

## Configuration Examples

### Standard Release Build

```bash
./configure.sh
make
```

### Debug Build for Development

```bash
./configure.sh --build-type=Debug --enable-verbose
make
```

### Optimized Build with Debug Info

```bash
./configure.sh --build-type=Release --enable-debug
make
```

### Static Binary for Distribution

```bash
./configure.sh --enable-static --enable-strip
make
```

### Custom Installation Location

```bash
./configure.sh --prefix=$HOME/.local
make
make install
```

### System-wide Installation

```bash
./configure.sh --prefix=/usr
make
sudo make install
```

### Using Specific Compiler

```bash
./configure.sh --fpc=/opt/fpc/bin/fpc --enable-verbose
make
```

## Makefile Targets

After running `configure.sh`, the generated Makefile provides several targets:

### `make` or `make all`

Build the project (default target).

```bash
make
```

### `make install`

Install the compiled binary to the configured location.

```bash
make install
```

May require sudo:
```bash
sudo make install
```

### `make uninstall`

Remove the installed binary.

```bash
make uninstall
```

May require sudo if installed to system directory.

### `make clean`

Remove build artifacts (compiled binary and intermediate files).

```bash
make clean
```

This removes:
- `bin/` directory
- `*.o` files (object files)
- `*.ppu` files (Pascal unit files)

### `make distclean`

Remove all generated files including the Makefile itself.

```bash
make distclean
```

After this, you need to run `./configure.sh` again.

### `make config`

Display the current build configuration.

```bash
make config
```

Shows:
- System information (platform, architecture)
- Compiler settings (path, flags)
- Build type
- Installation directories

### `make help`

Show available Makefile targets.

```bash
make help
```

## Compiler Flags Reference

### Optimization Flags

- `-O-` - Disable optimizations (Debug builds)
- `-O1` - Basic optimizations
- `-O2` - More optimizations
- `-O3` - Maximum optimizations (Release builds)

### Debug Flags

- `-g` - Generate debugging information
- `-gl` - Use line info unit (better stack traces)
- `-gw` - Generate DWARF debugging information

### Linking Flags

- `-XX` - Smart linking (remove unused code)
- `-Xs` - Strip symbols from executable
- `-static` - Static linking

### Verbosity Flags

- `-v` - Verbose output
- `-vw` - Show warnings
- `-vn` - Show notes
- `-vi` - Show informational messages
- `-vwni` - Show warnings, notes, and info (used with `--enable-verbose`)

### Output Flags

- `-FE<dir>` - Set executable output directory
- `-FU<dir>` - Set unit output directory

## Troubleshooting

### "Free Pascal Compiler not found"

**Problem:** configure.sh can't find FPC.

**Solution:**
1. Install FPC: `sudo apt install fpc` (Debian/Ubuntu) or `sudo pacman -S fpc` (Arch)
2. Or specify path: `./configure.sh --fpc=/path/to/fpc`

### "FPC version 3.2+ recommended"

**Problem:** Your FPC version is older than 3.2.0.

**Solution:**
- Update FPC to 3.2.0 or newer
- Or proceed anyway (may work but not guaranteed)

### Build fails with linking errors

**Problem:** Missing libraries or linker issues.

**Solution:**
- Try static linking: `./configure.sh --enable-static`
- Install required development libraries

### Permission denied during `make install`

**Problem:** No write permission to installation directory.

**Solution:**
```bash
sudo make install
```

### Want to test changes without installing

**Problem:** Don't want to install system-wide for testing.

**Solution:**
Run directly from build directory:
```bash
./bin/pashash --help
```

Or install to local directory:
```bash
./configure.sh --prefix=$HOME/.local
make install
# Ensure ~/.local/bin is in your PATH
```

## Advanced Usage

### Cross-Compilation

To cross-compile for a different architecture, specify the target:

```bash
./configure.sh --fpc=ppcross<arch>
make
```

### Multiple Build Configurations

You can maintain multiple configurations:

```bash
# Debug build
./configure.sh --build-type=Debug
make
cp bin/pashash pashash-debug

# Release build
make distclean
./configure.sh --build-type=Release
make
cp bin/pashash pashash-release
```

### Environment Variables

You can also set configuration via environment variables:

```bash
FPC=/usr/local/bin/fpc PREFIX=/opt ./configure.sh
```

## CI/CD Integration

### Example GitHub Actions

```yaml
- name: Configure
  run: ./configure.sh --build-type=Release --enable-verbose

- name: Build
  run: make

- name: Test binary
  run: ./bin/pashash --version
```

### Example GitLab CI

```yaml
build:
  script:
    - ./configure.sh --enable-static
    - make
  artifacts:
    paths:
      - bin/pashash
```

## Project Structure

After building, the project structure looks like:

```
pashash/
├── configure.sh          # Configuration script
├── Makefile             # Generated by configure.sh
├── BUILD.md             # This file
├── README.md            # User documentation
├── src/                 # Source code
│   ├── pashash.pas     # Main program
│   ├── CLIParser.pas   # CLI argument parser
│   ├── FileProcessor.pas
│   ├── HashCalculator.pas
│   └── PathUtils.pas
└── bin/                 # Created during build
    └── pashash         # Compiled binary
```

## See Also

- [README.md](README.md) - User documentation and usage examples
- Free Pascal documentation: https://www.freepascal.org/docs.html
