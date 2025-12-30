# Skills: Using CLI Tools in AI Engineering Workflows

This document explains how to use the structural engineering CLI tools during AI-assisted engineering discussions and design sessions.

## ⚠️ QUICK START (Windows)

**You are running on Windows.** All tools are `.exe` files in the current directory.

**Always use `./` or `.\` prefix:**
```bash
# Git Bash / WSL:
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120

# PowerShell / CMD:
.\flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
```

**Output modes:**
- Default: `As=4.40 As'=0.00 dominio=2` (concise, AI-optimized)
- `--field=As`: Extract single value → `4.40`
- `--verbose`: Human-readable table
- `--json`: Structured JSON

**Field names** (for `--field=X`):
- `As`, `As_comp` (or `As'`), `dominio`, `taxa` (reinforcement ratio, not `rho`), `ami`, `amilim`, `verificacao`
- Use `--help` or see error message for complete list

**Units:** cm, kN, kN.m, MPa (use dots: `58.5` not `58,5`)

---

## Overview

These are Unix-style CLI tools for structural engineering calculations. They support piping, chaining, and standard output redirection for integration into automated workflows.

**Platform:** Windows (`.exe` files). Always use `./` prefix when calling tools.

## Available Tools

1. **flexao** - Rectangular section bending design
2. **flexaot** - T-beam bending design
3. **flexao_verif** - Flexural capacity verification
4. **cisalhamento** - Shear design (stirrups)
5. **torcao** - Combined torsion + flexure + shear
6. **flexo_tracao** - Tension members with bending
7. **flexo_compressao** - Column design (compression with bending)

---

### flexao - Reinforced Concrete Bending Design

Calculates required reinforcement for rectangular concrete sections under bending moments according to NBR 6118.

#### Quick Reference

```bash
# Concise output (default) - optimized for AI parsing
./flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30
# Output: As=2.98 As'=0.00 dominio=2

# Get only the steel area needed
./flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --field=As
# Output: 2.98

# Human-readable output
./flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --verbose
# Output: Full formatted table

# Structured data
./flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --json
# Output: {"As": 2.98, "As_comp": 0.00, ...}
```

#### Parameters

**Required:**
- `--bw`: Section width (cm)
- `--h`: Section height (cm)
- `--d`: Effective depth (cm) - typically h minus cover (3-4 cm)
- `--fck`: Concrete characteristic strength (MPa) - common: 20, 25, 30, 40
- `--fyk`: Steel yield strength (MPa) - typically 500 for CA-50
- `--mk`: Characteristic bending moment (kN.m)

**Optional:**
- `--dl`: Distance to compression steel (cm) - default: 0.1*d
- `--es`: Steel elastic modulus (GPa) - default: 200
- `--gamac`: Concrete partial safety factor - default: 1.4
- `--gamas`: Steel partial safety factor - default: 1.15
- `--gamaf`: Load partial safety factor - default: 1.4
- `--bduct`: Ductility coefficient - default: 1.0

**Output Control:**
- `--verbose`: Human-readable formatted output
- `--json`: JSON structured output
- `--field=X`: Extract single value
  - Valid fields: `As`, `As_comp` (or `As'`), `dominio`, `taxa` (reinforcement ratio, **not** `rho`), `ami`, `amilim`, `verificacao`
  - Use `--help` to see complete list for each tool

#### Output Fields

**Default concise format:**
```
As=X.XX As'=X.XX dominio=N [status_if_needed]
```

**Fields:**
- `As`: Required tensile reinforcement area (cm²)
- `As'` (As_comp): Required compression reinforcement area (cm²) - zero for single reinforcement
- `dominio`: Strain domain (2, 3, or 4)
  - Domain 2: Under-reinforced (ductile failure, steel yields first)
  - Domain 3: Balanced to over-reinforced (both steel and concrete at limit)
  - Domain 4: Over-reinforced (brittle failure, concrete crushes first - avoid!)
- `taxa`: Reinforcement ratio (%)
- `ami`: Reduced applied moment (dimensionless)
- `amilim`: Limit moment for single reinforcement (dimensionless)
- `verificacao`: Status (OK, OK_Armadura_minima, OK_Armadura_dupla, ERRO_...)

#### Typical Usage in AI Workflows

**Design iteration:**
```bash
# Try different beam sizes quickly
./flexao.exe --bw=15 --h=40 --d=36 --fck=25 --fyk=500 --mk=50 --field=As
# 5.23

./flexao.exe --bw=15 --h=45 --d=41 --fck=25 --fyk=500 --mk=50 --field=As
# 4.12

./flexao.exe --bw=20 --h=45 --d=41 --fck=25 --fyk=500 --mk=50 --field=As
# 4.12
```

**Check multiple load cases:**
```bash
# Service load (Mk = 30 kN.m)
./flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30
# As=2.98 As'=0.00 dominio=2

# Ultimate load (Mk = 45 kN.m)
./flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=45
# As=4.67 As'=0.00 dominio=2

# Extreme case (Mk = 80 kN.m)
./flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=80
# As=9.32 As'=0.00 dominio=3
```

**Parametric analysis:**
```bash
# Compare concrete grades
for fck in 20 25 30 40; do
  echo -n "fck=$fck: "
  ./flexao.exe --bw=15 --h=40 --d=36 --fck=$fck --fyk=500 --mk=50 --field=As
done
# fck=20: 5.34
# fck=25: 4.94
# fck=30: 4.65
# fck=40: 4.24
```

**Extract for further processing:**
```bash
# Use in calculations
AS=$(./flexao.exe --bw=15 --h=40 --d=36 --fck=25 --fyk=500 --mk=30 --field=As)
echo "Number of 12.5mm bars needed: $(echo "scale=0; $AS / 1.23 + 0.5" | bc)"
# Number of 12.5mm bars needed: 3
```

---

### cisalhamento - Shear Design

Calculates required shear reinforcement (stirrups) for concrete sections according to NBR 6118.

#### Quick Reference

```bash
# Concise output (default)
./cisalhamento.exe --bw=20 --d=45 --fck=25 --fyk=500 --vk=80
# Output: Asw=2.43 twd=1.2444 twu=4.3393

# Get only stirrup area
./cisalhamento.exe --bw=20 --d=45 --fck=25 --fyk=500 --vk=80 --field=Asw
# Output: 2.43

# Verbose output
./cisalhamento.exe --bw=20 --d=45 --fck=25 --fyk=500 --vk=80 --verbose
```

#### Parameters

**Required:**
- `--bw`: Section width (cm)
- `--d`: Effective depth (cm)
- `--fck`: Concrete characteristic strength (MPa)
- `--fyk`: Steel yield strength (MPa)
- `--vk`: Characteristic shear force (kN)

**Optional:**
- `--gamac`: Concrete safety factor, default: 1.4
- `--gamas`: Steel safety factor, default: 1.15
- `--gamaf`: Load safety factor, default: 1.4

**Output Control:**
- `--verbose`, `--json`, `--field=X`

#### Output Fields

- `Asw`: Required stirrup area per meter (cm2/m)
- `twd`: Design shear stress (MPa)
- `twu`: Ultimate shear stress (MPa)
- `tc`: Concrete contribution (MPa)
- `aswmin`: Minimum stirrup area (cm2/m)
- `verificacao`: Status (OK, ERRO_...)

#### Usage Examples

```bash
# Check if section can handle shear
./cisalhamento.exe --bw=15 --d=40 --fck=20 --fyk=500 --vk=150
# If ERRO appears, section is too small

# Compare different beam widths
for bw in 15 20 25; do
  echo -n "bw=$bw: "
  ./cisalhamento.exe --bw=$bw --d=45 --fck=25 --fyk=500 --vk=100 --field=Asw
done
```

---

### flexao_verif - Flexural Capacity Verification

Given existing reinforcement, calculates the moment capacity of the section.

#### Quick Reference

```bash
# Concise output
./flexao_verif.exe --bw=15 --d=36 --As=3 --fck=20 --fyk=500
# Output: Mrd=42.18 x=8.93 dominio=2

# Get only moment capacity
./flexao_verif.exe --bw=15 --d=36 --As=3 --fck=20 --fyk=500 --field=Mrd
# Output: 42.18

# With compression steel
./flexao_verif.exe --bw=15 --d=36 --As=8 --Asl=2 --fck=20 --fyk=500
```

#### Parameters

**Required:**
- `--bw`: Section width (cm)
- `--d`: Effective depth (cm)
- `--As`: Tensile reinforcement area (cm2)
- `--fck`: Concrete characteristic strength (MPa)
- `--fyk`: Steel yield strength (MPa)

**Optional:**
- `--Asl`: Compression reinforcement area (cm2), default: 0
- `--dl`: Distance to compression steel (cm), default: 0.1*d
- `--es`: Steel elastic modulus (GPa), default: 200
- `--gamac`, `--gamas`: Safety factors

#### Output Fields

- `Mrd`: Design moment capacity (kNm)
- `x`: Neutral axis depth (cm)
- `dominio`: Strain domain (2, 3, or 4)
- `verificacao`: Status message

#### Usage Examples

```bash
# Verify if existing reinforcement is adequate
./flexao_verif.exe --bw=20 --d=45 --As=6.5 --fck=25 --fyk=500 --field=Mrd
# Output: 135.4
# Compare with required moment: if Mrd > Mk_required, it's adequate

# Check multiple bar configurations
for As in 4 6 8 10; do
  Mrd=$(./flexao_verif.exe --bw=20 --d=45 --As=$As --fck=25 --fyk=500 --field=Mrd)
  echo "As=$As cm2 → Mrd=$Mrd kNm"
done
```

---

### torcao - Combined Torsion + Flexure + Shear

Designs reinforcement for sections under combined torsion, bending moment, and shear force.

#### Quick Reference

```bash
# Concise output
./torcao.exe --bw=25 --h=60 --dl=4 --fck=30 --fyk=500 --mk=80 --vk=60 --tk=20
# Output: Asw=77.79 Aslt=52.39 smax=30.0

# Get specific values
./torcao.exe --bw=25 --h=60 --dl=4 --fck=30 --fyk=500 --mk=80 --vk=60 --tk=20 --field=Asw
# Output: 77.79

# Detailed breakdown
./torcao.exe --bw=25 --h=60 --dl=4 --fck=30 --fyk=500 --mk=80 --vk=60 --tk=20 --verbose
```

#### Parameters

**Required:**
- `--bw`: Section width (cm)
- `--h`: Section height (cm)
- `--dl`: Cover distance (cm)
- `--fck`: Concrete characteristic strength (MPa)
- `--fyk`: Steel yield strength (MPa)
- `--mk`: Characteristic bending moment (kNm)
- `--vk`: Characteristic shear force (kN)
- `--tk`: Characteristic torsion moment (kNm)

**Optional:**
- `--es`, `--gamac`, `--gamas`, `--gamaf`, `--bduct`

#### Output Fields

- `Asw`: Total stirrup area (cm2/m) - includes shear + torsion
- `Aswv`: Stirrup area for shear only (cm2/m)
- `Aswt`: Stirrup area for torsion only (cm2/m)
- `Aslt`: Longitudinal reinforcement for torsion (cm2)
- `smax`: Maximum stirrup spacing (cm)
- `As`: Flexural tensile reinforcement (cm2)
- `Asl`: Flexural compression reinforcement (cm2)
- `verificacao`: Status

#### Usage Examples

```bash
# Design for combined loading
./torcao.exe --bw=30 --h=70 --dl=4 --fck=30 --fyk=500 --mk=150 --vk=100 --tk=40

# Check if torsion is significant
# Run cisalhamento first (shear only)
./cisalhamento.exe --bw=30 --d=66 --fck=30 --fyk=500 --vk=100 --field=Asw
# Then torcao (shear + torsion)
./torcao.exe --bw=30 --h=70 --dl=4 --fck=30 --fyk=500 --mk=150 --vk=100 --tk=40 --field=Asw
# Compare the two - if significantly different, torsion is important

# Extract components for detailing
./torcao.exe --bw=25 --h=60 --dl=4 --fck=30 --fyk=500 --mk=80 --vk=60 --tk=20 --json | jq '{Asw, Aslt, smax}'
```

---

### flexo_tracao - Tension Members with Bending

Designs reinforcement for concrete members under combined axial tension and bending moment (eccentric tension).

#### Quick Reference

```bash
# Concise output
./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=50 --nk=100 --fck=25 --fyk=500
# Output: As=5.34 Asl=0.00 dominio=2

# Get specific values
./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=50 --nk=100 --fck=25 --fyk=500 --field=As
# Output: 5.34

# Small eccentricity case (reinforcement on both faces)
./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=10 --nk=200 --fck=25 --fyk=500 --verbose
```

#### Parameters

**Required:**
- `--bw`: Section width (cm)
- `--h`: Section height (cm)
- `--d`: Effective depth (cm)
- `--mk`: Characteristic bending moment (kNm)
- `--nk`: Characteristic tension force (kN) - **positive for tension**
- `--fck`: Concrete characteristic strength (MPa)
- `--fyk`: Steel yield strength (MPa)

**Optional:**
- `--dl`: Distance to compression face steel (cm), default: 0.1*d
- `--es`: Steel elastic modulus (GPa), default: 200
- `--gamac`, `--gamas`, `--gamaf`: Safety factors
- `--bduct`: Ductility coefficient, default: 1.0

**Output Control:**
- `--verbose`, `--json`, `--field=X`

#### Output Fields

- `As`: Main reinforcement area (cm2) - tension face
- `Asl`: Secondary reinforcement area (cm2) - compression face
- `dominio`: Domain (1 or 2)
  - Domain 1: Small eccentricity (both faces need reinforcement)
  - Domain 2: Large eccentricity (treated like bending with axial load)
- `verificacao`: Status

#### Usage Examples

```bash
# Large eccentricity (moment dominates)
./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=80 --nk=50 --fck=25 --fyk=500
# Output: As=7.xx Asl=0.00 dominio=2
# Only one face needs reinforcement

# Small eccentricity (axial force dominates)
./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=10 --nk=200 --fck=25 --fyk=500
# Output: As=4.00 Asl=2.44 dominio=1
# Both faces need reinforcement

# Compare eccentricity effect
for nk in 50 100 150 200; do
  echo -n "Nk=$nk kN: "
  ./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=50 --nk=$nk --fck=25 --fyk=500
done
# Shows how increasing axial force affects reinforcement distribution

# Extract for tie design
AS=$(./flexo_tracao.exe --bw=25 --h=25 --d=21 --mk=5 --nk=150 --fck=30 --fyk=500 --field=As)
echo "Total steel required: $AS cm2"
```

#### Engineering Notes

**When to use flexo_tracao:**
- Tie beams under tension
- Members supporting hanging loads
- Wind bracing in tension
- Suspended structures
- Any member with tensile normal force + moment

**Domain interpretation:**
- **Domain 1** (small e): e < h/6 approximately
  - Both faces need steel
  - More uniform stress distribution
  - Typical for heavily loaded ties

- **Domain 2** (large e): e > h/6 approximately
  - One face in tension (needs As)
  - Other face may be in compression (but Asl usually zero)
  - Behaves more like a beam in bending

**Sign convention:**
- `nk` positive = tension (typical case)
- Large Nk with small Mk → Domain 1
- Small Nk with large Mk → Domain 2

---

### flexo_compressao - Column Design (Compression with Bending)

Designs reinforcement for concrete columns under combined axial compression and bending moment.

#### Quick Reference

```bash
# Concise output
./flexo_compressao.exe --bw=20 --h=40 --dl=4 --nk=500 --mk=80 --fck=25 --fyk=500
# Output: As=9.04 Asmin=3.20 dominio=3

# Get steel area to adopt
./flexo_compressao.exe --bw=20 --h=40 --dl=4 --nk=500 --mk=80 --fck=25 --fyk=500 --field=Asadopt
# Output: 9.04

# Detailed output
./flexo_compressao.exe --bw=20 --h=40 --dl=4 --nk=500 --mk=80 --fck=25 --fyk=500 --verbose
```

#### Parameters

**Required:**
- `--bw`: Section width (cm)
- `--h`: Section height (cm)
- `--dl`: Cover distance to reinforcement (cm) - typically 3-4 cm
- `--nk`: Characteristic compression force (kN) - **positive for compression**
- `--mk`: Characteristic bending moment (kNm)
- `--fck`: Concrete characteristic strength (MPa)
- `--fyk`: Steel yield strength (MPa)

**Optional:**
- `--es`: Steel elastic modulus (GPa), default: 200
- `--gamac`, `--gamas`, `--gamaf`: Safety factors

**Output Control:**
- `--verbose`, `--json`, `--field=X`

#### Output Fields

- `As`: Calculated steel area (cm2)
- `Asmin`: Minimum steel area per NBR 6118 for columns (cm2)
- `Asadopt`: Steel area to adopt (max of As and Asmin) (cm2)
- `x`: Neutral axis depth (cm)
- `dominio`: Strain domain (2, 3, or 5)
  - Domain 2: Ductile (less common for columns)
  - Domain 3: Balanced, most common for columns
  - Domain 5: Compression dominated (small eccentricity)
- `verificacao`: Status

#### Usage Examples

```bash
# Typical column design
./flexo_compressao.exe --bw=20 --h=50 --dl=4 --nk=800 --mk=100 --fck=30 --fyk=500
# Output: As=X.XX Asmin=X.XX dominio=3

# Small eccentricity (large axial load)
./flexo_compressao.exe --bw=25 --h=50 --dl=4 --nk=1000 --mk=30 --fck=30 --fyk=500
# May return minimum reinforcement

# Compare different section sizes
for h in 30 40 50 60; do
  echo -n "h=$h cm: "
  ./flexo_compressao.exe --bw=20 --h=$h --dl=4 --nk=600 --mk=80 --fck=25 --fyk=500 --field=Asadopt
done

# Design for biaxial bending (approximate)
# For Mx and My, design separately and combine
./flexo_compressao.exe --bw=40 --h=40 --dl=4 --nk=1000 --mk=120 --fck=30 --fyk=500 --field=As
```

#### Engineering Notes

**When to use flexo_compressao:**
- Building columns
- Piers and supports
- Any compression member with bending
- P-Delta effects consideration

**Column design considerations:**
- **Minimum reinforcement**: NBR 6118 requires ρmin = 0.4% or 0.15·(Nd/Ac)·(fcd/fyd), whichever is greater
- **Maximum reinforcement**: 8% of gross area (for practical detailing)
- **Minimum 4 bars**: Rectangular columns need at least 4 longitudinal bars (one in each corner)
- **Slenderness**: This tool calculates cross-section strength only. Check slenderness (λ) separately for second-order effects

**Domain interpretation for columns:**
- **Domain 3** (most common): Balanced failure, both concrete and steel at or near limits
- **Domain 5**: Compression-dominated, small eccentricity e < (h/2 - cover)
- Avoid **Domain 2** in columns (indicates under-reinforced, more typical of beams)

**Sign convention:**
- `nk` positive = compression (opposite of flexo_tracao)
- Large Nk with small Mk → Small eccentricity → Domain 5 or minimum steel
- Moderate Nk with moderate Mk → Domain 3 (typical)

**Practical tips:**
- Start with dl = 4 cm for typical columns (3 cm cover + bar diameter)
- If As < Asmin, adopt Asmin (tool does this automatically in Asadopt)
- If As > 4-6% of gross area, consider increasing section size
- For biaxial bending, this tool handles uniaxial; use approximate methods or interaction diagrams for precise biaxial design

---

## When to Use These Tools (AI Agent Guidelines)

### DO use these tools when:

1. **User asks for reinforcement design**
   - "How much steel do I need for a 20x50cm beam with Md=120kN.m?"
   - "Design the reinforcement for this section"
   - "What's the required As for..."

2. **Comparing design alternatives**
   - "Should I use a 15x40 or 20x35 beam?"
   - "Which concrete grade is more economical?"
   - Call the tool multiple times with different parameters

3. **Checking if a section is adequate**
   - "Can a 15x30 beam support 60kN.m?"
   - Run tool and check if domain is acceptable (2 or 3, not 4)

4. **During iterative design conversations**
   - User: "The beam is too small"
   - Agent: *runs tool with larger dimensions*
   - Agent: "A 20x45cm beam needs As=4.2cm² instead"

5. **Extracting specific values for calculations**
   - Need just As to calculate number of bars
   - Need domain to check ductility
   - Use `--field=X` for surgical extraction

### DON'T use these tools when:

1. **User is asking conceptual questions**
   - "What is flexural design?" - explain, don't calculate
   - "How does NBR 6118 work?" - describe the code

2. **Insufficient information**
   - User hasn't provided dimensions or loads
   - Ask for parameters first, then use tool

3. **Non-structural questions**
   - Cost estimation, scheduling, construction methods
   - These tools only do structural calculations

## Output Parsing Tips for AI Agents

**Default concise format** is easiest to parse:
```bash
As=2.98 As'=0.00 dominio=2
```

Parse with simple regex or string split:
```
Match: As=(\d+\.\d+)  → captures "2.98"
Match: dominio=(\d+)  → captures "2"
```

**Field extraction** for single values:
```bash
--field=As  → "2.98"  # Just the number, ready to use
```

**JSON format** when you need structured data:
```json
{
  "As": 2.98,
  "As_comp": 0.00,
  "dominio": 2,
  "taxa": 0.4962,
  "ami": 0.177923,
  "amilim": 0.295200,
  "verificacao": "OK"
}
```

## Engineering Context

### Typical Parameter Ranges

**Concrete (fck):**
- 20 MPa: Low-strength, residential
- 25 MPa: Standard residential/commercial
- 30 MPa: Standard commercial
- 40+ MPa: High-strength, special structures

**Steel (fyk):**
- 500 MPa: CA-50 (most common in Brazil)
- 600 MPa: CA-60 (high strength)

**Beam dimensions:**
- Width (bw): 12-30 cm typical
- Height (h): 30-80 cm typical
- Effective depth (d): h - (3 to 4 cm) for beams

**Reinforcement interpretation:**
- As < 0.15% of gross area: Probably minimum reinforcement
- As = 0.5-2%: Normal, economical design
- As > 4%: Section too small, increase dimensions
- As' > 0: Double reinforcement, consider increasing section

**Domain interpretation:**
- Domain 2: Good! Ductile, steel yields before concrete crushes
- Domain 3: OK, but check if close to domain 4
- Domain 4: Avoid! Brittle failure, concrete crushes before steel yields

### Common Workflows

**1. Complete Beam Design (Bending + Shear):**
```bash
# Step 1: Design for bending
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
# Output: As=7.89 As'=0.00 dominio=3

# Step 2: Design for shear
./cisalhamento.exe --bw=20 --d=46 --fck=25 --fyk=500 --vk=90
# Output: Asw=3.21 twd=1.3043 twu=4.3393

# Result: Need As=7.89cm² and Asw=3.21cm²/m
```

**2. Verification of Existing Design:**
```bash
# Given: 20x50cm beam with As=8.5cm²
# Check capacity:
./flexao_verif.exe --bw=20 --d=46 --As=8.5 --fck=25 --fyk=500 --field=Mrd
# Output: 195.3
# If Mrd (195.3) > Mk_required (120), design is adequate ✓
```

**3. Torsion Design:**
```bash
# When torsion is present
./torcao.exe --bw=25 --h=60 --dl=4 --fck=30 --fyk=500 --mk=100 --vk=80 --tk=30
# Output: Asw=95.4 Aslt=65.2 smax=30.0
# Need total stirrups Asw=95.4cm²/m and longitudinal Aslt=65.2cm²
```

**4. Optimization:**
```bash
# Find most economical section
for h in 40 45 50 55; do
  As=$(./flexao.exe --bw=20 --h=$h --d=$((h-4)) --fck=25 --fyk=500 --mk=120 --field=As)
  echo "h=$h → As=$As cm²"
done
# Choose smallest h with acceptable reinforcement ratio
```

**5. Iterative Design:**
```bash
# Start with trial section
./cisalhamento.exe --bw=15 --d=40 --fck=25 --fyk=500 --vk=150
# If ERRO appears, increase width:
./cisalhamento.exe --bw=20 --d=40 --fck=25 --fyk=500 --vk=150
# Repeat until OK
```

**6. Column Design:**
```bash
# Design column for axial load + moment
./flexo_compressao.exe --bw=20 --h=40 --dl=4 --nk=500 --mk=80 --fck=25 --fyk=500
# Output: As=9.04 Asmin=3.20 dominio=3
# Adopt As=9.04cm² distributed around perimeter

# Check if minimum reinforcement governs
./flexo_compressao.exe --bw=25 --h=50 --dl=4 --nk=1000 --mk=30 --fck=30 --fyk=500
# Output: As=0.00 Asmin=5.00 dominio=3 [OK__Armadura_minima]
# Minimum reinforcement governs, adopt Asmin=5.00cm²
```

**7. Tension Member Design:**
```bash
# Design tie beam under tension + moment
./flexo_tracao.exe --bw=20 --h=50 --d=46 --mk=50 --nk=100 --fck=25 --fyk=500
# Output: As=5.34 Asl=0.00 dominio=2
# Large eccentricity: only tension face needs reinforcement
```

## Exit Codes

- `0`: Success, design is OK
- `1`: Calculation error (section insufficient, error in design)
- `2`: Invalid arguments (missing parameters, unknown field)

Use exit codes for error handling:
```bash
if ./flexao.exe --bw=10 --h=20 --d=17 --fck=20 --fyk=500 --mk=200 >/dev/null 2>&1; then
  echo "Design OK"
else
  echo "Section insufficient or error"
fi
```

## Output Mode Comparison

| Mode | Tokens (10 calls) | Use Case |
|------|-------------------|----------|
| `--verbose` | ~4000 | Human-readable reports |
| Default | ~400 | AI parsing, multiple fields |
| `--field=X` | ~100 | Single value extraction |

## Tool Summary

**Implemented Tools:**
- `flexao` - Rectangular bending design
- `flexaot` - T-beam bending design
- `flexao_verif` - Capacity verification
- `cisalhamento` - Shear design
- `torcao` - Combined torsion + flexure + shear
- `flexo_tracao` - Tension members with bending
- `flexo_compressao` - Column design

All tools support: concise default, `--verbose`, `--json`, `--field=X`
