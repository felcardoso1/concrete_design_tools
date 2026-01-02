# Improvement Suggestions for CLI Tools

**Based on:** Analysis of 4 AI agent test sessions (December 30, 2025) + Implementation history  
**Source:** Terminal output analysis, agent feedback (filtered for experience-based insights), and completed improvements

---

## Priority Classification

- **🔴 High Priority:** Issues affecting all agents, blocking or causing significant friction
- **🟡 Medium Priority:** Issues affecting some agents or causing moderate friction
- **🟢 Low Priority:** Nice-to-have improvements, polish items

---

## ✅ Completed Improvements

### 1. Decimal Separator (International Compatibility) ✅

**Status:** ✅ Complete - All 8 tools recompiled and tested

**Problem:**
Tools were compiled with locale-dependent decimal parsing, causing them to fail with standard decimal notation in some system locales.

**Solution:**
Added `FormatSettings.DecimalSeparator := '.'` at the start of each program's main block to force dot as decimal separator regardless of system locale.

**Files Modified:**
- [x] src/flexao.pas
- [x] src/cisalhamento.pas
- [x] src/flexao_verif.pas
- [x] src/torcao.pas
- [x] src/flexo_tracao.pas
- [x] src/flexo_compressao.pas
- [x] src/flexaot.pas

**Testing:**
All tools now work with standard decimal notation:
```bash
./bin/flexao.exe --mk=100.5        # ✅ Works
./bin/cisalhamento.exe --vk=75.3   # ✅ Works
./bin/flexo_compressao.exe --nk=600.5 --mk=85.75  # ✅ Works
```

---

### 2. Reinforcement Ratio in Default Output ✅

**Status:** ✅ Complete

**Change:** Added `rho=X.XX` to flexao.pas default output

**Before:**
```bash
./bin/flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=100
As=8.00 As'=0.00 dominio=3
```

**After:**
```bash
./bin/flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=100
As=8.00 As'=0.00 dominio=3 rho=0.80
```

**Rationale:**
- Reinforcement ratio (ρ) is a critical NBR 6118 check (must be < 4%)
- Already calculated internally, zero cost to output
- Saves AI agents from manual calculation
- Minimal token bloat (8 characters)

**File modified:** src/flexao.pas

---

### 3. Valid Fields Documentation in Help Text ✅

**Status:** ✅ Complete

**Change:** All tools now list valid field names in `--help` output

**Before:**
```
--field=X  Output only specific field (As, As_comp, dominio, taxa, ami, amilim)
```

**After:**
```
--field=X  Output only specific field
           Valid fields: As, As_comp, dominio, taxa, ami, amilim, verificacao
```

**Impact:**
- Improved discoverability - no trial-and-error needed
- Two-line format improves readability
- Fixed missing fields in documentation (e.g., aswmin in cisalhamento, x in flexo_compressao)

**Files modified:**
- src/flexao.pas
- src/cisalhamento.pas (added missing `aswmin`)
- src/flexao_verif.pas
- src/flexaot.pas (added missing `verificacao`)
- src/torcao.pas (added missing `verificacao`)
- src/flexo_tracao.pas
- src/flexo_compressao.pas (added missing `x`)

---

### 4. Units Reference in SKILLS.md ✅

**Status:** ✅ Complete

**Change:** Added comprehensive Units Reference section near top of SKILLS.md

**Content:**
- Input units: cm, kN, kNm, MPa, GPa
- Output units: cm², cm²/m, kNm, cm, %, dimensionless
- Sign conventions: tension vs compression in flexo_tracao/compressao

**Rationale:**
- Eliminates unit confusion without bloating output
- AI agents can reference once instead of guessing
- Maintains Unix philosophy (concise output, document separately)

**File modified:** SKILLS.md

---


## 🔴 High Priority Improvements (Pending)


### 2. Field Name Discoverability

**Status:** ⚠️ Partially Complete - Help text done, aliases pending

**Issue:** Agent tried `--field=rho` but correct field name is `taxa`. Error message was helpful (listed valid fields), but confusion occurred.

**Evidence:**
- One agent encountered this error
- Error message correctly listed valid fields, enabling recovery
- Suggests field names aren't intuitive

**Completed:**
- ✅ Valid fields listed in --help output

**Remaining Work:**

#### A. Field Name Aliases
Support multiple names for same field:
- `taxa` and `rho` → same field
- `As'` and `As_comp` → same field

#### B. --field=list Option
```bash
./flexao.exe --field=list
# Output:
# Available fields: As, As_comp, dominio, taxa, ami, amilim, verificacao
# Aliases: rho=taxa, As'=As_comp
```

**Recommendation:** Implement field name aliases and --field=list option.

---

### 3. Error Messages with Actionable Guidance

**Status:** ❌ Not Started

**Issue:** Current error messages are brief (e.g., "ERRO: Secao insuficiente"). Agents suggested more actionable error messages.

**Evidence:**
- Multiple agents suggested this improvement
- Terminal shows errors are brief and don't guide recovery

**Suggested Solutions:**

#### Enhanced Error Messages
```bash
# Instead of:
ERRO: Secao insuficiente

# Provide:
ERRO: Section insufficient (Domain 4 - brittle failure)
CURRENT: bw=20cm, h=40cm, Mk=200kNm, fck=25MPa
SUGGESTION: Increase height to h >= 45cm OR increase width to bw >= 25cm
MINIMUM REQUIRED: h >= 45cm for this loading
```

#### Specific Exit Codes
- `0`: Success
- `1`: Calculation error (insufficient section)
- `2`: Invalid arguments
- `3`: Domain 4 (brittle failure - section too small)
- `4`: Invalid parameter range

**Recommendation:** Implement enhanced error messages with suggestions. This would help agents automatically retry with corrected parameters.

---

## 🟡 Medium Priority Improvements

### 4. Tool Discovery Mechanism

**Status:** ⚠️ Partially Complete - Documentation added, tool list command pending

**Issue:** Agents had to use `ls *.exe` or `ls -la` to discover available tools.

**Evidence:**
- 3 of 4 agents listed directory contents
- No built-in way to list available tools

**Completed:**
- ✅ Tool Selection Guide table added to test_sandbox/CLAUDE.md

**Remaining Work:**

#### A. Master Tool List Command
```bash
./tools --list
# or
./list-tools.sh
# Output:
# Available tools:
#   flexao              - Bending design
#   cisalhamento        - Shear design
#   flexo_compressao    - Column design
#   ...
```

#### B. --help-all Flag
```bash
./flexao.exe --help-all
# Shows all available tools with brief descriptions
```

**Recommendation:** Apply Tool Selection Guide to main CLAUDE.md immediately, consider Option A for future.

---

### 5. Parameter Validation and Range Checking

**Status:** ❌ Not Started

**Issue:** Agents suggested more explicit validation for invalid inputs (e.g., negative dimensions).

**Evidence:**
- Multiple agents suggested this
- No terminal evidence of invalid inputs being tested
- Suggests proactive concern rather than actual issue

**Suggested Solutions:**

#### Input Validation
```bash
./flexao.exe --bw=-20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5
# Error:
# ERRO: Invalid parameter value
# --bw must be positive (received: -20)
# Typical range: 10-100 cm
```

#### Range Warnings
```bash
./flexao.exe --bw=5 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5
# Warning: bw=5cm is unusually small (typical: 15-30cm for beams)
# Calculation proceeds, but result may be impractical
```

**Recommendation:** Implement basic validation (positive values, reasonable ranges) with clear error messages.

---

### 6. Batch Processing Mode

**Status:** ❌ Not Started

**Issue:** Multiple agents suggested batch processing for parametric studies.

**Evidence:**
- 3 of 4 agents suggested this
- Terminal shows agents used shell loops for comparisons
- Would enable more efficient parametric studies

**Suggested Solutions:**

#### A. Stdin Batch Mode
```bash
cat << EOF | ./flexao.exe --batch --field=As
20 50 46 25 500 58.5
20 55 51 25 500 58.5
20 60 56 25 500 58.5
EOF
# Output:
# 4.40
# 3.91
# 3.51
```

#### B. JSON Array Input
```bash
./flexao.exe --batch --json << EOF
[
  {"bw": 20, "h": 50, "d": 46, "fck": 25, "fyk": 500, "mk": 58.5},
  {"bw": 20, "h": 55, "d": 51, "fck": 25, "fyk": 500, "mk": 58.5}
]
EOF
# Output: JSON array of results
```

**Recommendation:** Implement Option A (stdin batch) as it's simpler and more Unix-like. Option B could be added later.

---

---

## 🟢 Low Priority Improvements

### 9. Version and Help Information

**Status:** ❌ Not Started

**Issue:** Agents suggested `--version` flag and enhanced `--help`.

**Evidence:**
- Mentioned by multiple agents
- Would help with tool discovery and debugging

**Suggested Solutions:**

#### Version Flag
```bash
./flexao.exe --version
# Output:
# flexao v1.0.0
# NBR 6118:2014 compliant
# Compiled: 2025-12-29
```

#### Enhanced Help
```bash
./flexao.exe --help
# Shows:
# - All parameters with units and defaults
# - Output fields with descriptions
# - Example commands
# - Exit codes
```

**Recommendation:** Implement both - low effort, high value for debugging and discovery.

---

