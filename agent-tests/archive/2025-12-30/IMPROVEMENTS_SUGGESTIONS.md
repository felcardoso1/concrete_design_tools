# Improvement Suggestions for CLI Tools

**Based on:** Analysis of 4 AI agent test sessions (December 30, 2025)  
**Source:** Terminal output analysis and agent feedback (filtered for experience-based insights)

---

## Priority Classification

- **🔴 High Priority:** Issues affecting all agents, blocking or causing significant friction
- **🟡 Medium Priority:** Issues affecting some agents or causing moderate friction
- **🟢 Low Priority:** Nice-to-have improvements, polish items

---

## High Priority Improvements

### 1. Path Resolution and Tool Discovery

**Issue:** All 4 agents failed on first attempt with `flexao.exe` (command not found), requiring trial-and-error to discover `./flexao.exe` syntax.

**Evidence:**
- 100% of agents encountered this issue
- All required multiple attempts before success
- Wasted tool calls and added friction

**Suggested Solutions:**

#### Option A: Executable Wrapper Scripts (Recommended)
Create wrapper scripts that handle path resolution:
```bash
# Create flexao.bat (Windows) or flexao.sh (Linux)
@echo off
cd /d "%~dp0"
.\bin\flexao.exe %*
```

#### Option B: PATH Configuration Documentation
Add prominent "Quick Start" section at top of CLAUDE.md:
```markdown
## ⚠️ IMPORTANT: Running Tools

On Windows (Git Bash/WSL):
```bash
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
```

On Windows (CMD/PowerShell):
```cmd
.\flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
```

On Linux/Mac:
```bash
./flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
```
```

#### Option C: Installation Script
Create `install.sh` / `install.bat` that:
- Adds tools to PATH
- Creates symlinks/aliases
- Verifies installation

**Recommendation:** Implement Option A (wrapper scripts) + Option B (documentation) for immediate impact.

---

### 2. Field Name Discoverability

**Issue:** Agent tried `--field=rho` but correct field name is `taxa`. Error message was helpful (listed valid fields), but confusion occurred.

**Evidence:**
- One agent encountered this error
- Error message correctly listed valid fields, enabling recovery
- Suggests field names aren't intuitive

**Suggested Solutions:**

#### A. Enhanced --help Output
```bash
./flexao.exe --help
# Should include:
# 
# Field Extraction (--field=X):
#   As          : Required tensile reinforcement (cm²)
#   As_comp     : Compression reinforcement (cm²) [also: As']
#   dominio     : Strain domain (2, 3, or 4)
#   taxa        : Reinforcement ratio (%) [also: rho]
#   ami         : Reduced applied moment
#   amilim      : Limit moment for single reinforcement
#   verificacao : Status message
```

#### B. Field Name Aliases
Support multiple names for same field:
- `taxa` and `rho` → same field
- `As'` and `As_comp` → same field

#### C. --field=list Option
```bash
./flexao.exe --field=list
# Output:
# Available fields: As, As_comp, dominio, taxa, ami, amilim, verificacao
# Aliases: rho=taxa, As'=As_comp
```

**Recommendation:** Implement all three - enhanced help, aliases, and --field=list.

---

### 3. Error Messages with Actionable Guidance

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

## Medium Priority Improvements

### 4. Tool Discovery Mechanism

**Issue:** Agents had to use `ls *.exe` or `ls -la` to discover available tools.

**Evidence:**
- 3 of 4 agents listed directory contents
- No built-in way to list available tools

**Suggested Solutions:**

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

#### C. README with Tool Matrix
Add to CLAUDE.md:
```markdown
## Tool Selection Guide

| Need | Tool | Command |
|------|------|---------|
| Beam bending | flexao | `./flexao.exe --bw=...` |
| Beam shear | cisalhamento | `./cisalhamento.exe --bw=...` |
| Column design | flexo_compressao | `./flexo_compressao.exe --bw=...` |
| T-beam | flexaot | `./flexaot.exe --bf=...` |
| Verify capacity | flexao_verif | `./flexao_verif.exe --As=...` |
```

**Recommendation:** Implement Option C (documentation) immediately, consider Option A for future.

---

### 5. Parameter Validation and Range Checking

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

### 7. Units in Output

**Issue:** Some agents suggested including units in output for clarity.

**Evidence:**
- Mentioned by 2 agents
- Not a blocking issue, but would improve clarity

**Suggested Solutions:**

#### Units in Concise Output (Optional)
```bash
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5 --units
# Output: As=4.40cm² As'=0.00cm² dominio=2 taxa=0.44%
```

#### Units in JSON Output
```json
{
  "As": {"value": 4.40, "unit": "cm²"},
  "As_comp": {"value": 0.00, "unit": "cm²"},
  "dominio": {"value": 2, "unit": null}
}
```

**Recommendation:** Add `--units` flag for verbose output mode. Keep default concise output unit-free for token efficiency.

---

## Low Priority Improvements

### 8. Documentation Restructuring

**Issue:** Agents suggested documentation could be better organized (progressive disclosure, quick start).

**Evidence:**
- Multiple agents suggested this
- Some feedback appears influenced by documentation structure analysis
- However, all agents successfully used tools after reading docs

**Suggested Solutions:**

#### A. Quick Start Section (Top of CLAUDE.md)
```markdown
# QUICK START (Read this first!)

1. Tools are in current directory - use `./toolname.exe`
2. Basic beam design:
   ./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
3. Units: cm, kN, kN.m, MPa (use dots: 58.5 not 58,5)
4. See full documentation below...
```

#### B. Three-Tier Documentation Structure
- **Quick Reference** (1-2 pages): Essential commands, common workflows
- **Reference Manual** (current SKILLS.md): Complete parameter lists
- **Examples Repository** (separate file): Complete design scenarios

**Recommendation:** Implement Option A immediately. Option B is nice-to-have but current documentation is functional.

---

### 9. Version and Help Information

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

### 10. Configuration Files

**Issue:** One agent suggested support for configuration files for repeated parameter sets.

**Evidence:**
- Mentioned by 1 agent
- Would be useful for repeated calculations

**Suggested Solution:**
```bash
# config.json
{
  "fck": 25,
  "fyk": 500,
  "gamac": 1.4,
  "gamas": 1.15
}

./flexao.exe --config=config.json --bw=20 --h=50 --d=46 --mk=120
# Uses fck=25, fyk=500 from config, overrides with command-line params
```

**Recommendation:** Low priority - shell scripts or aliases can achieve similar results.

---

## Implementation Roadmap

### Phase 1: Critical Fixes (Immediate)
1. ✅ Add "Quick Start" section to CLAUDE.md with path resolution
2. ✅ Enhance error messages with actionable guidance
3. ✅ Add field name aliases (rho=taxa, As'=As_comp)
4. ✅ Enhance --help output with field descriptions

### Phase 2: User Experience (Short-term)
5. ✅ Create wrapper scripts for path resolution
6. ✅ Add --field=list option
7. ✅ Implement input validation with clear errors
8. ✅ Add --version flag
9. ✅ Add tool selection guide to CLAUDE.md

### Phase 3: Advanced Features (Medium-term)
10. ⭐ Implement batch processing mode (stdin)
11. ⭐ Add units flag for output
12. ⭐ Create example workflow scripts

### Phase 4: Polish (Long-term)
13. ⭐ Documentation restructuring (if needed)
14. ⭐ Configuration file support
15. ⭐ Interactive help/wizard mode

---

## Notes on Agent Feedback Interpretation

### What to Take Seriously
- ✅ Issues encountered by multiple agents (path resolution, field names)
- ✅ Terminal-verified problems (failed first attempts, error recovery)
- ✅ Suggestions that would clearly improve workflow (batch processing)

### What to Question
- ⚠️ Documentation structure suggestions (may be influenced by reading docs rather than usage)
- ⚠️ Theoretical improvements without terminal evidence
- ⚠️ Overly detailed suggestions that seem derived from documentation analysis

### Validation Strategy
- Always check terminal output for evidence of claimed issues
- Prioritize improvements that address actual friction points
- Test improvements with agents before full implementation

---

## Conclusion

The most critical improvements are:
1. **Path resolution** (affects 100% of agents)
2. **Field name discoverability** (caused actual errors)
3. **Error message enhancement** (would enable automatic recovery)

These three improvements would address the primary friction points identified in the test sessions. Other improvements are valuable but less critical for initial agent adoption.

