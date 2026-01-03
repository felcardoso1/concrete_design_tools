# Contributing

Contributions are welcome! This project provides CLI tools for reinforced concrete design according to NBR 6118.

## Quick Start

### Prerequisites
- Free Pascal Compiler (fpc) - [Download](https://www.freepascal.org/download.html)

### Compilation
```bash
# Build all tools (Linux/macOS)
./build.sh

# Build all tools (Windows)
build.bat

# Or compile a single tool manually
fpc -O2 -Xs -XX -CX -o./AGENT/flexao src/flexao.pas
```

## Tool Conventions

Every tool must follow these conventions:

### Output Formats
All tools must support four output modes:
- **Default**: Concise key=value format (`As=2.98 As'=0.00 dominio=2`)
- **`--verbose`**: Human-readable formatted output
- **`--json`**: Structured JSON output
- **`--field=X`**: Extract single field value

### Exit Codes
- `0`: Success
- `1`: Calculation error (e.g., section insufficient)
- `2`: Invalid arguments or missing parameters

### I/O Rules
- All input via command line arguments (no interactive prompts)
- Errors to stderr, results to stdout
- Support `--help` flag

## Adding a New Tool

1. Look at existing tools in `src/` for patterns (especially `flexao.pas`)
2. Implement all four output modes
3. Follow the units convention: cm, kN, kN.m, MPa
4. Test with reasonable engineering values
5. Update SKILLS.md with usage documentation

## Units Convention

- **Dimensions**: cm
- **Forces**: kN
- **Moments**: kN.m
- **Stresses**: MPa
- **Steel areas**: cm²

## Algorithm Reference

Algorithms are based on "PROGRAMAS PARA DIMENSIONAMENTO E VERIFICAÇÃO DE CONCRETO ARMADO" by José Milton de Araújo, following NBR 6118:2014.

## Pull Request Guidelines

1. Follow the existing code style
2. Ensure all output modes work correctly
3. Test with edge cases (minimum reinforcement, insufficient sections)
4. Update documentation (SKILLS.md) if adding new tools
