# Concrete Design Tools

**Fast, lightweight CLI tools for reinforced concrete design**

Currently supports **NBR 6118** (Brazilian concrete design code). Future versions will add support for **ACI** (American) and **Eurocode** (European) standards.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Language: Pascal](https://img.shields.io/badge/Language-Pascal-blue.svg)](https://www.freepascal.org/)

## Overview

This is a suite of command-line tools for structural engineering calculations, aimed at being used mainly by AI agents rather than humans. Each tool performs a specific reinforced concrete design calculation.

## Features

- **Optimized for AI agents**: Concise output mode reduces token usage by 10-40x
- **Multiple output formats**: Concise (default), verbose (more suited to be read by humans), JSON, or single-field extraction
- **Fast execution**: Native compiled binaries with microsecond startup time
- **Zero dependencies**: Static binaries work anywhere
- **NBR 6118 compliant**: Follows Brazilian concrete design code

## Available Tools

| Tool | Description | Status |
|------|-------------|--------|
| `flexao` | Bending design for rectangular sections | ✅ Complete |
| `cisalhamento` | Shear design (stirrup calculation) | ✅ Complete |
| `flexaot` | T-beam bending design | ✅ Complete |
| `flexao_verif` | Bending capacity verification | ✅ Complete |
| `flexo_compressao` | Column design (compression + bending) | ✅ Complete |
| `flexo_tracao` | Tension member design | ✅ Complete |
| `torcao` | Torsion design | ✅ Complete |



### Why Pascal?

These calculations are all short and simple—any CPU can execute them instantly. The real bottleneck is **startup time and loading the program from disk**. Pascal excels here because of its fast startup time, achieved through classic static linking and mature, self-contained compiled libraries with decades of optimization. The code is pure Pascal with zero external dependencies.

See the [startup time benchmarks](https://github.com/bdrung/startup-time#results) for comparative data.

- **⚡ Microsecond startup**: No runtime initialization, no interpreter overhead
- **📦 Single static binary**: Zero dependencies, just compile and run
- **🎯 Deterministic**: Perfect for AI agent workflows and automation
- **🔧 Simple**: Easy to understand, modify, and extend


## Installation

### Prerequisites

- Free Pascal Compiler (FPC) - [Download here](https://www.freepascal.org/download.html)

### Compilation

```bash
# Clone the repository
git clone https://github.com/felcardoso/concrete_design_tools.git
cd concrete_design_tools

# Build all tools (Linux/macOS)
./build.sh

# Build all tools (Windows)
build.bat

# Or compile manually (output goes to AGENT/ folder)
fpc -O2 -Xs -XX -CX -o./AGENT/flexao src/flexao.pas
```

The `build.sh` script compiles all Pascal source files and places the binaries directly in the `AGENT/` folder.

### Compilation Flags (used by build.sh)

| Flag | Purpose |
|------|---------|
| `-O2` | Optimization level 2 (faster execution) |
| `-Xs` | Strip debug symbols |
| `-XX` | Smart linking (link only used code from units) |
| `-CX` | Create smart-linkable units (enables dead code elimination) |

The smart linking flags (`-XX -CX`) reduce binary size from ~536KB to ~121KB (77% smaller), enabling binaries to fit in CPU L3 cache for faster repeated execution by AI agents.

See [docs/BINARY_SIZE_OPTIMIZATION.md](docs/BINARY_SIZE_OPTIMIZATION.md) for detailed analysis.

## Usage

### Quick Start Example - Bending Design

Design a rectangular beam section for bending (run from the `AGENT/` folder):

```bash
cd AGENT

# Concise output (default) - optimized for AI agents
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
# Output: As=6.89 As'=0.00 dominio=3

# Human-readable output
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --verbose
# Output: Formatted table with all results

# JSON output (for programmatic use)
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --json
# Output: {"As": 6.89, "As_comp": 0.00, "dominio": 3, ...}

# Extract single field
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --field=As
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

All examples assume you are in the `AGENT/` folder.

### Example 1: Compare Different Section Sizes

```bash
# Using concise output for quick comparison
for h in 40 45 50 55 60; do
  echo "h=$h: $(./flexao --bw=20 --h=$h --d=$((h-4)) --fck=25 --fyk=500 --mk=120)"
done
```

### Example 2: Extract Steel Area for Bar Count Calculation

```bash
# Get required steel area
AS=$(./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --field=As)

# Calculate number of 12.5mm bars (area = 1.23 cm2)
echo "Number of bars needed: $(echo "scale=1; $AS / 1.23" | bc)"
```

### Example 3: JSON Parsing with jq

```bash
# Extract multiple fields from JSON output
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --json | jq '{As, dominio}'
```

## Documentation

**For developers:**
- **[CONTRIBUTING.md](CONTRIBUTING.md)**: Development guide for contributors
- **[CLAUDE.md](CLAUDE.md)**: Instructions for AI agents working on this codebase

**For users (engineers):**
- **[AGENT/AGENTS.md](AGENT/AGENTS.md)**: Quick reference for AI agents using the tools
- **[AGENT/SKILLS.md](AGENT/SKILLS.md)**: Detailed tool documentation and workflows

**Testing with AI agents:**
- **[agent-tests/README.md](agent-tests/README.md)**: Framework for testing tools with AI agents

## Project Structure

```
.
├── README.md              # This file (developer-facing)
├── LICENSE                # MIT License
├── CONTRIBUTING.md        # Development guide
├── CLAUDE.md              # AI agent instructions (for developers)
├── build.sh               # Build script (compiles to AGENT/)
├── src/                   # Pascal source files
│   ├── flexao.pas         # Bending design
│   ├── cisalhamento.pas   # Shear design
│   └── ...
├── agent-tests/           # AI agent testing framework
│   ├── README.md          # Testing methodology
│   ├── tasks/             # Task definitions
│   ├── sessions/          # Test results
│   └── archive/           # Historical sessions
└── AGENT/                 # Production folder (distribute this!)
    ├── AGENTS.md          # AI agent quick reference
    ├── SKILLS.md          # Detailed tool documentation
    ├── flexao             # Compiled binaries
    ├── cisalhamento
    └── ...
```

### For Engineers (End Users)

The `AGENT/` folder is self-contained and can be distributed independently. It contains:
- All compiled binaries (ready to use)
- `AGENTS.md` - Quick reference for AI assistants
- `SKILLS.md` - Detailed documentation

Simply copy the `AGENT/` folder to use the tools. No source code or development files needed.

## Technical Details

### Algorithm Source

The algorithms are based on:
- **Book**: "PROGRAMAS PARA DIMENSIONAMENTO E VERIFICAÇÃO DE CONCRETO ARMADO" (book available for free on Editora Duna's website)
- **Author**: José Milton de Araújo
- **Standard**: NBR 6118:2014 (Brazilian concrete design code)

### Design Philosophy

These tools follow the Unix philosophy: **do one thing well**. They are designed to be:
- Fast (microsecond startup, instant calculation)
- Composable (chain with other Unix tools)
- Predictable (deterministic output, clear exit codes)
- AI-friendly (optimized for repeated calls in agentic workflows)

## AI Agent Testing Framework

Since these tools are designed for AI agents rather than human engineers, we test them by having AI agents use the tools and provide structured feedback.

### Why Test with AI Agents?

Traditional CLI tools are tested by human developers. But these tools have a different target user: AI agents operating in terminal environments (Claude Code, OpenCode, etc.). Testing with actual AI agents reveals friction points that human testers might miss.

### Test-Interview Methodology

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   1. TASK       │     │   2. EXECUTION  │     │   3. INTERVIEW  │
│                 │     │                 │     │                 │
│ AI receives a   │────▶│ AI uses tools   │────▶│ AI answers 30   │
│ structural      │     │ to solve the    │     │ questions about │
│ design problem  │     │ problem         │     │ its experience  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
                                                        │
                                                        ▼
                              ┌─────────────────────────────────────┐
                              │         4. ANALYSIS                 │
                              │                                     │
                              │ Compare written feedback vs actual  │
                              │ terminal logs to detect:            │
                              │ - Unreported friction points        │
                              │ - Documentation-influenced answers  │
                              │ - Universal patterns across agents  │
                              └─────────────────────────────────────┘
```

### Key Insight: Feedback vs Reality

AI agents tend to minimize difficulties in written feedback, even when terminal logs show trial-and-error. By comparing the two, we identify:

- **Path resolution issues** - 100% of tested agents initially failed with `flexao` before discovering `./flexao`
- **Field name confusion** - Agents tried `--field=rho` when the correct name is `--field=taxa`
- **Documentation gaps** - Where agents had to guess instead of finding answers

### Running Your Own Tests

See [agent-tests/README.md](agent-tests/README.md) for:
- Step-by-step instructions to run a test session
- The 30-question interview template
- Example tasks and session naming conventions
- Archived results from previous test sessions

## Contributing

Contributions are welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on adding new tools and development conventions.

## License

MIT License - See LICENSE file for details

## Author

**Felipe Luiz Cardoso** (felcardoso on X)

## Acknowledgments

- Algorithms from José Milton de Araújo's book
- NBR 6118 Brazilian concrete design standard
- Free Pascal Compiler team

---

**Note**: This project is optimized for use with AI agents like Claude Code and Open Code. The concise output format significantly reduces token usage during iterative design workflows while maintaining all necessary engineering information.

**Tip**: For production use, distribute only the `AGENT/` folder. It contains everything an engineer needs without development clutter.
