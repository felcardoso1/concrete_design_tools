# Concrete Design Tools

**Fast, lightweight CLI tools for reinforced concrete design according to NBR 6118 (Brazilian concrete design code)**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Language: Pascal](https://img.shields.io/badge/Language-Pascal-blue.svg)](https://www.freepascal.org/)

## Overview

This is a suite of command-line tools written in Free Pascal for structural engineering calculations. Each tool performs a specific reinforced concrete design calculation based on NBR 6118 (Brazilian concrete design standard).

### Why Pascal?

- **⚡ Microsecond startup**: No runtime initialization, no interpreter overhead
- **📦 Single static binary**: Zero dependencies, just compile and run
- **🎯 Deterministic**: Perfect for AI agent workflows and automation
- **🔧 Simple**: Easy to understand, modify, and extend

## Features

- **Optimized for AI agents**: Concise output mode reduces token usage by 10-40x
- **Multiple output formats**: Concise (default), verbose, JSON, or single-field extraction
- **Fast execution**: Native compiled binaries with microsecond startup time
- **Zero dependencies**: Static binaries work anywhere
- **NBR 6118 compliant**: Follows Brazilian concrete design code

## Available Tools

| Tool | Description | Status |
|------|-------------|--------|
| `flexao` | Bending design for rectangular sections | ✅ Complete |
| `cisalhamento` | Shear design (stirrup calculation) | 🚧 In Progress |
| `flexaot` | T-beam bending design | 🚧 In Progress |
| `flexao_verif` | Bending capacity verification | 🚧 In Progress |
| `flexo_compressao` | Column design (compression + bending) | 🚧 In Progress |
| `flexo_tracao` | Tension member design | 🚧 In Progress |
| `torcao` | Torsion design | 🚧 In Progress |

## Installation

### Prerequisites

- Free Pascal Compiler (FPC) - [Download here](https://www.freepascal.org/download.html)

### Compilation

```bash
# Clone the repository
git clone http://192.168.3.213:3000/felcardoso/concrete_design_tools.git
cd concrete_design_tools

# Compile a tool (e.g., flexao)
cd src
fpc -O2 -Xs flexao.pas

# Move to bin directory
mv flexao.exe ../bin/

# Or compile all tools at once (Windows)
for %f in (*.pas) do fpc -O2 -Xs %f && move %~nf.exe ..\bin\
```

### Compilation Flags

- `-O2`: Optimization level 2 (faster execution)
- `-Xs`: Strip debug symbols (smaller binary size)

## Usage

### Quick Start Example - Bending Design

Design a rectangular beam section for bending:

```bash
# Concise output (default) - optimized for AI agents
./bin/flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
# Output: As=6.89 As'=0.00 dominio=3

# Human-readable output
./bin/flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --verbose
# Output: Formatted table with all results

# JSON output (for programmatic use)
./bin/flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --json
# Output: {"As": 6.89, "As_comp": 0.00, "dominio": 3, ...}

# Extract single field
./bin/flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --field=As
# Output: 6.89
```

### Parameters

**flexao** (Bending Design):
- `--bw`: Section width (cm)
- `--h`: Section height (cm)
- `--d`: Effective depth (cm)
- `--fck`: Characteristic concrete strength (MPa)
- `--fyk`: Characteristic steel yield strength (MPa)
- `--mk`: Characteristic bending moment (kN.m)

### Output Formats

All tools support four output modes:

1. **Default (Concise)**: `result1=X result2=Y` - Minimal output for AI workflows
2. **`--verbose`**: Human-readable formatted tables
3. **`--json`**: Structured JSON for programmatic parsing
4. **`--field=X`**: Extract single field value (surgical precision)

### Standard Flags

Every tool supports:
- `--help`: Display usage information and available parameters
- `--verbose`: Human-readable output with tables
- `--json`: JSON structured output
- `--field=X`: Extract specific field (use `--help` to see valid fields)

## Units Convention

- **Dimensions**: cm (centimeters)
- **Forces**: kN (kilonewtons)
- **Moments**: kN.m (kilonewton-meters)
- **Stresses**: MPa (megapascals)

## Exit Codes

- `0`: Success
- `1`: Calculation error (e.g., section insufficient, structural failure)
- `2`: Invalid arguments or missing parameters

## Examples

### Example 1: Compare Different Section Sizes

```bash
# Using concise output for quick comparison
for h in 40 45 50 55 60; do
  echo "h=$h: $(./bin/flexao --bw=20 --h=$h --d=$((h-4)) --fck=25 --fyk=500 --mk=120)"
done
```

### Example 2: Extract Steel Area for Bar Count Calculation

```bash
# Get required steel area
AS=$(./bin/flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --field=As)

# Calculate number of 12.5mm bars (area = 1.23 cm²)
echo "Number of bars needed: $(echo "scale=1; $AS / 1.23" | bc)"
```

### Example 3: JSON Parsing with jq

```bash
# Extract multiple fields from JSON output
./bin/flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --json | jq '{As, dominio}'
```

## Documentation

- **[CLAUDE.md](CLAUDE.md)**: Implementation guide for developers and AI agents
- **[SKILLS.md](SKILLS.md)**: AI agent usage patterns and workflows
- **[IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md)**: Development roadmap and status

## Project Structure

```
.
├── README.md              # This file
├── CLAUDE.md             # Implementation guide
├── SKILLS.md             # AI agent usage guide
├── src/                  # Pascal source files
│   ├── flexao.pas        # Bending design (complete)
│   ├── cisalhamento.pas  # Shear design
│   ├── flexaot.pas       # T-beam design
│   └── ...
├── bin/                  # Compiled binaries
│   └── flexao.exe        # Ready to use
├── tests/                # Test cases
├── docs/                 # Additional documentation
└── reference_book/       # Algorithm source (Fortran)
```

## Technical Details

### Algorithm Source

The algorithms are based on:
- **Book**: "PROGRAMAS PARA DIMENSIONAMENTO E VERIFICAÇÃO DE CONCRETO ARMADO"
- **Author**: José Milton de Araújo
- **Standard**: NBR 6118:2014 (Brazilian concrete design code)

### Design Philosophy

These tools follow the Unix philosophy: **do one thing well**. They are designed to be:
- Fast (microsecond startup, instant calculation)
- Composable (chain with other Unix tools)
- Predictable (deterministic output, clear exit codes)
- AI-friendly (optimized for repeated calls in agentic workflows)

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Follow the existing code style and conventions
4. Ensure all output modes work (default, --verbose, --json, --field)
5. Test thoroughly with reasonable engineering values
6. Submit a pull request

## Development

To add a new tool:

1. Read the corresponding chapter in `reference_book/`
2. Use the template in `CLAUDE.md`
3. Implement all four output modes
4. Compile and test
5. Update this README and SKILLS.md

## License

MIT License - See LICENSE file for details

## Safety Factor Defaults (NBR 6118)

- **γc** = 1.4 (concrete partial safety factor)
- **γs** = 1.15 (steel partial safety factor)
- **fck**: Typically 20-50 MPa
- **fyk**: Typically 500 MPa (CA-50 steel)

## Author

**Felipe Cardoso** (felcardoso)

## Acknowledgments

- Algorithms from José Milton de Araújo's book
- NBR 6118 Brazilian concrete design standard
- Free Pascal Compiler team

---

**Note**: This project is optimized for use with AI agents like Claude Code. The concise output format significantly reduces token usage during iterative design workflows while maintaining all necessary engineering information.
