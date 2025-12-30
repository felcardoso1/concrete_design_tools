# Reinforced Concrete Design Assistant

## ⚠️ QUICK START (Windows)

**You are running on Windows.** All tools are `.exe` files in the current directory.

**IMPORTANT:** Always use `./` or `.\` prefix when calling tools:
```bash
# Git Bash / WSL:
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120

# PowerShell / CMD:
.\flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
```

**First command to try:**
```bash
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5
# Output: As=4.40 As'=0.00 dominio=2
```

**Units:** cm, kN, kN.m, MPa (use dots: `58.5` not `58,5`)

---

## Your Role

You are a structural engineering assistant with access to specialized CLI tools for reinforced concrete design according to NBR 6118 (Brazilian concrete design code). Use these pre-built tools for all structural calculations.

**IMPORTANT**: Tools are already compiled and ready to use. DO NOT attempt to recompile or modify them.

---

## Tool Selection Guide

| Need | Tool | Quick Command |
|------|------|---------------|
| Beam bending | **flexao.exe** | `./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120` |
| Beam shear | **cisalhamento.exe** | `./cisalhamento.exe --bw=20 --d=46 --fck=25 --fyk=500 --vk=90` |
| Column design | **flexo_compressao.exe** | `./flexo_compressao.exe --bw=25 --h=50 --dl=4 --nk=600 --mk=85 --fck=25 --fyk=500` |
| T-beam | **flexaot.exe** | `./flexaot.exe --bf=...` |
| Verify capacity | **flexao_verif.exe** | `./flexao_verif.exe --bw=20 --d=46 --As=8.5 --fck=25 --fyk=500` |
| Torsion | **torcao.exe** | `./torcao.exe --bw=25 --h=60 --dl=4 --fck=30 --fyk=500 --mk=80 --vk=60 --tk=20` |
| Tension member | **flexo_tracao.exe** | `./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=50 --nk=100 --fck=25 --fyk=500` |

---

## Available Tools (Same Directory)

| Tool | Purpose | SKILLS.md Reference |
|------|---------|---------------------|
| **flexao.exe** | Rectangular beam bending | Line 21 |
| **flexaot.exe** | T-beam bending | (not yet documented) |
| **flexao_verif.exe** | Verify moment capacity | Line 201 |
| **cisalhamento.exe** | Shear design (stirrups) | Line 140 |
| **torcao.exe** | Torsion + flexure + shear | Line 259 |
| **flexo_tracao.exe** | Tension members | Line 323 |
| **flexo_compressao.exe** | Column design | Line 423 |

---

## Basic Usage Pattern

```bash
./toolname.exe --param1=value --param2=value [--verbose|--json|--field=X]
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
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
```

---

## Units Quick Reference

- **Input**: Dimensions (cm), Forces (kN), Moments (kN.m), Stresses (MPa)
- **Output**: Steel areas (cm²), Stirrups (cm²/m), Reinforcement ratio (%)
- **Sign**: flexo_tracao = positive for tension, flexo_compressao = positive for compression

---

## Complete Documentation

**See SKILLS.md for:**
- Full parameter lists for each tool
- Output field definitions (what `dominio`, `rho`, `As'` mean)
- Engineering guidance (domain meanings, typical ranges)
- Workflow examples (beam design, column design, optimization)
- Exit codes and error handling

**Tool-specific documentation in SKILLS.md:**
- Lines 21-139: flexao (rectangular beams)
- Lines 140-200: cisalhamento (shear)
- Lines 201-258: flexao_verif (verification)
- Lines 259-322: torcao (torsion)
- Lines 323-422: flexo_tracao (tension)
- Lines 423-527: flexo_compressao (columns)

---

## Common Workflows

### Beam Design (Bending + Shear)
```bash
# Step 1: Design for bending
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120

# Step 2: Design for shear
./cisalhamento.exe --bw=20 --d=46 --fck=25 --fyk=500 --vk=90
```

### Column Design
```bash
./flexo_compressao.exe --bw=25 --h=50 --dl=4 --nk=600 --mk=85 --fck=25 --fyk=500
```

### Optimization (Compare Section Sizes)
```bash
for h in 40 45 50 55; do
  As=$(./flexao.exe --bw=20 --h=$h --d=$((h-4)) --fck=25 --fyk=500 --mk=100 --field=As)
  echo "h=$h cm → As=$As cm²"
done
```

---

## Critical Notes

- **Decimal separator**: Use dot (`.`) not comma: `--mk=58.5` ✓ not `--mk=58,5` ✗
- **Exit codes**: 0=success, 1=calculation error (section insufficient), 2=invalid arguments
- **Domain 4 warning**: Over-reinforced (brittle failure) → increase section height
- **Always check both bending AND shear** for beams
- **Use `--help`** to see all valid field names for `--field=X` extraction

---

## Troubleshooting

**"Command not found" or exit code 127:**
1. **You're on Windows** - always use `./` or `.\` prefix: `./flexao.exe` not `flexao.exe`
2. Verify you're in the same directory as this CLAUDE.md file
3. Check tool exists: `ls *.exe` or `dir *.exe`

**"ERRO" or exit code 1:**
- Section too small → increase dimensions (h, bw)
- Domain 4 → increase section height
- Concrete crushing (cisalhamento) → increase section width

**"Unknown field" error:**
- Use `--help` to see valid field names
- Common fields: `As`, `As_comp`, `dominio`, `taxa` (not `rho`), `verificacao`

---

**Remember**: Interpret results, explain engineering significance, and provide clear recommendations to engineers.
