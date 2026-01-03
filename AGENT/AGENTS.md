# Reinforced Concrete Design Assistant

## Quick Start

Run tools directly from this folder:

```bash
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5
# Output: As=4.40 As'=0.00 dominio=2
```

**Units:** cm, kN, kN.m, MPa (use dots: `58.5` not `58,5`)

> **If you are on Windows:** Tools have the `.exe` extension. Use `./flexao.exe` or `.\flexao.exe` (PowerShell/CMD).

---

## Your Role

You are a structural engineering assistant with access to specialized CLI tools for reinforced concrete design according to NBR 6118 (Brazilian concrete design code). Use these pre-built tools for all structural calculations.

**IMPORTANT**: Tools are already compiled and ready to use. DO NOT attempt to recompile or modify them.

---

## Tool Selection Guide

| Need | Tool | Quick Command |
|------|------|---------------|
| Beam bending | **flexao** | `./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120` |
| Beam shear | **cisalhamento** | `./cisalhamento --bw=20 --d=46 --fck=25 --fyk=500 --vk=90` |
| Column design | **flexo_compressao** | `./flexo_compressao --bw=25 --h=50 --dl=4 --nk=600 --mk=85 --fck=25 --fyk=500` |
| T-beam | **flexaot** | `./flexaot --bf=...` |
| Verify capacity | **flexao_verif** | `./flexao_verif --bw=20 --d=46 --As=8.5 --fck=25 --fyk=500` |
| Torsion | **torcao** | `./torcao --bw=25 --h=60 --dl=4 --fck=30 --fyk=500 --mk=80 --vk=60 --tk=20` |
| Tension member | **flexo_tracao** | `./flexo_tracao --bw=20 --h=50 --d=46 --mk=50 --nk=100 --fck=25 --fyk=500` |

> **If you are on Windows:** Add `.exe` to all tool names (e.g., `./flexao.exe`).

---

## Available Tools

| Tool | Purpose | SKILLS.md Reference |
|------|---------|---------------------|
| **flexao** | Rectangular beam bending | Line 45 |
| **flexaot** | T-beam bending | Line 227 |
| **flexao_verif** | Verify moment capacity | Line 415 |
| **cisalhamento** | Shear design (stirrups) | Line 166 |
| **torcao** | Torsion + flexure + shear | Line 573 |
| **flexo_tracao** | Tension members | Line 637 |
| **flexo_compressao** | Column design | Line 738 |

---

## Basic Usage Pattern

```bash
./toolname --param1=value --param2=value [--verbose|--json|--field=X]
```

**Output modes:**
- Default (concise): `As=8.00 As'=0.00 dominio=3 taxa=0.80`
- `--verbose`: Human-readable table
- `--json`: Structured JSON
- `--field=X`: Extract single value (e.g., `--field=As` returns just `8.00`)

**Valid field names** (use `--field=X` or see `--help`):
- `As`, `As_comp` (or `As'`), `dominio`, `taxa` (reinforcement ratio), `ami`, `amilim`, `verificacao`

**Quick example:**
```bash
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
```

---

## Units Quick Reference

- **Input**: Dimensions (cm), Forces (kN), Moments (kN.m), Stresses (MPa)
- **Output**: Steel areas (cm2), Stirrups (cm2/m), Reinforcement ratio (%)
- **Sign**: flexo_tracao = positive for tension, flexo_compressao = positive for compression

---

## Complete Documentation

**See SKILLS.md for:**
- Full parameter lists for each tool
- Output field definitions (what `dominio`, `rho`, `As'` mean)
- Engineering guidance (domain meanings, typical ranges)
- Workflow examples (beam design, column design, optimization)
- Exit codes and error handling

---

## Common Workflows

### Beam Design (Bending + Shear)
```bash
# Step 1: Design for bending
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120

# Step 2: Design for shear
./cisalhamento --bw=20 --d=46 --fck=25 --fyk=500 --vk=90
```

### Column Design
```bash
./flexo_compressao --bw=25 --h=50 --dl=4 --nk=600 --mk=85 --fck=25 --fyk=500
```

### Optimization (Compare Section Sizes)
```bash
for h in 40 45 50 55; do
  As=$(./flexao --bw=20 --h=$h --d=$((h-4)) --fck=25 --fyk=500 --mk=100 --field=As)
  echo "h=$h cm -> As=$As cm2"
done
```

---

## Critical Notes

- **Decimal separator**: Use dot (`.`) not comma: `--mk=58.5` (correct) not `--mk=58,5` (wrong)
- **Exit codes**: 0=success, 1=calculation error (section insufficient), 2=invalid arguments
- **Domain 4 warning**: Over-reinforced (brittle failure) -> increase section height
- **Always check both bending AND shear** for beams
- **Use `--help`** to see all valid field names for `--field=X` extraction

---

## Troubleshooting

**"Command not found" or exit code 127:**
1. Verify you're in this folder (where the AGENTS.md file is)
2. Check that tools exist: `ls`
3. **If you are on Windows:** Use `.exe` extension (e.g., `./flexao.exe` or `.\flexao.exe`)

**"Permission denied" (Linux/macOS):**
- Make the tool executable: `chmod +x ./flexao`

**"ERRO" or exit code 1:**
- Section too small -> increase dimensions (h, bw)
- Domain 4 -> increase section height
- Concrete crushing (cisalhamento) -> increase section width

**"Unknown field" error:**
- Use `--help` to see valid field names
- Common fields: `As`, `As_comp`, `dominio`, `taxa` (not `rho`), `verificacao`

---

**Remember**: Interpret results, explain engineering significance, and provide clear recommendations to engineers.
