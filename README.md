# pashash

CLI utility for calculating MD5 hashes of files, written in Pascal (more hash algorithms coming soon).

## Features

- Calculate MD5 hashes for single file or all files in directory
- Output to console or file
- No subdirectories processing
- Simple and fast

## Installation

### Requirements
- Free Pascal Compiler (FPC) 3.2+
- make

### Build from source

```sh
git clone https://github.com/anmitalidev/pashash
cd pashash
make
```

The compiled binary will be placed in `bin/pashash`

### Install system-wide (optional)

```sh
sudo make install
```

## Usage

```sh
pashash [OPTIONS]
```

### Options

- `--version` - Show version information
- `-d <dir>` - Process all files in directory
- `-f <file>` - Process single file
- `-o <file>` - Output file (if omitted, prints to console)

### Examples

Process all files in current directory, output to console:
```sh
pashash -d .
```

Process all files in specific directory, save to file:
```sh
pashash -d /path/to/dir -o hashes.txt
```

Calculate hash for single file:
```sh
pashash -f document.pdf
```

Calculate hash for single file and save to file:
```sh
pashash -f document.pdf -o hash.txt
```

### Output format

Each line contains filename and its MD5 hash separated by space:
```
file1.txt d41d8cd98f00b204e9800998ecf8427e
image.png 98d2c7313ff79b4df8b6c96d8c276d8c
document.pdf a6b1a01f8cb1c4e8b3e53b9c8d7f1d3b
```

## Building from source

1. Clone the repository:
```sh
git clone <repository-url>
cd pashash
```

2. Build using make:
```sh
make
```

3. (Optional) Install system-wide:
```sh
sudo make install
```

## Development

Project structure:
```
pashash/
├── src/         # Source code (modular units)
├── bin/         # Compiled binary (created after build)
├── Makefile     # Build configuration
└── README.md    # This file
```

Build options in Makefile:
- `-O3`: Level 3 optimization
- `-XX`: Smart linking on
- `-Xs`: Smart linking on

## License

PasHash licenced under MIT