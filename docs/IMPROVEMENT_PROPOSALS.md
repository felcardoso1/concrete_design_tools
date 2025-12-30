# Improvement Proposals from AI Agent Testing

This document tracks suggestions from AI agents that have tested the tools in real-world scenarios.

## Document Purpose
- Record feedback from AI users of the agentic tools
- Assess each proposal against Unix philosophy (do ONE thing well)
- Distinguish between useful enhancements vs. feature bloat
- Maintain focus on deterministic calculation tools, not design automation

---

## Test Session: Claude Code (Sonnet 4.5) - 2025-12-30

**Source:** `Test sessions/Claude Code Claude Sonnet 4.5 session.txt`

**Task:** Design reinforced concrete beam and column for a small building
**Result:** ✅ Successful - all calculations completed correctly
**Token Efficiency:** ~100 tokens for technical output vs potential 1000+ with verbose mode

### Proposals from This Session

#### P1.1: Units in JSON Output
**Status:** 🟡 Future Enhancement
**Priority:** Medium
**Rationale:** Nice to have for verification/debugging, but not critical

**Current:**
```json
{"As": 6.36, "dominio": 2, "rho": 0.64}
```

**Proposed:**
```json
{
  "As": {"value": 6.36, "unit": "cm²"},
  "dominio": 2,
  "rho": {"value": 0.64, "unit": "%"}
}
```

**Assessment:**
- **Pros:** Eliminates unit ambiguity, helpful for international users
- **Cons:** Adds complexity to JSON parsing, increases token count
- **Decision:** Add to backlog - implement if users report unit confusion errors
- **Keep:** Concise format must remain unchanged (no units)

---

#### P1.2: Structured Error Output (--json-error flag)
**Status:** 🟡 Future Enhancement
**Priority:** Medium
**Rationale:** Could help AI agents diagnose failures programmatically

**Current:**
```
Error: Compressed concrete strut insufficient
(exit code 1)
```

**Proposed:**
```json
{
  "error": "BIELA_INSUFICIENTE",
  "message": "Compressed concrete strut insufficient",
  "suggestion": "Increase bw or fck",
  "values": {"twd": 5.2, "twu": 4.3}
}
```

**Assessment:**
- **Pros:** Enables programmatic error handling and auto-correction attempts
- **Cons:** Significant code addition, unclear if AI agents need this level of detail
- **Decision:** Add to backlog - wait for evidence that agents struggle with text errors
- **Note:** Current stderr text errors have worked fine in testing

---

#### P1.3: Input Validation Warnings
**Status:** ✅ Approved for Implementation
**Priority:** High
**Rationale:** Catches common typos and input errors without blocking execution

**Example:**
```bash
$ flexao --bw=20 --h=50 --d=46 --fck=250 --fyk=500 --mk=81.9
Warning: Unusual concrete grade fck=250.0 MPa (typical range: 20-50 MPa)
```

**Validation Rules:**
- `fck`: Warn if < 15 or > 90 MPa
- `fyk`: Warn if not in {250, 500, 600} MPa
- `bw`: Warn if > 100 cm (beams) or > 150 cm (columns)
- `d > h`: **Error** and halt (geometric impossibility)
- `cover`: Warn if < 1.5 cm or > 5 cm

**Assessment:**
- **Pros:** Prevents catastrophic typo errors (fck=250 instead of fck=25)
- **Cons:** Minimal - warnings go to stderr, don't affect stdout parsing
- **Decision:** IMPLEMENT - high value, low cost
- **Implementation:** Add `ValidateInputs()` procedure to each tool

---

#### P1.4: Case-Insensitive Field Names
**Status:** ✅ Approved for Implementation
**Priority:** High
**Rationale:** Improves usability without breaking existing behavior

**Current:**
```bash
$ flexao ... --field=as          # Works? Unknown
$ flexao ... --field=As          # Works? Unknown
$ flexao ... --field=AS          # Works? Unknown
```

**Proposed:**
```pascal
normalizedField := LowerCase(fieldOutput);
if (normalizedField = 'as') then ...
```

**Also accept aliases:**
- `As` ← `as`, `AS`
- `As_comp` ← `as_comp`, `as'`, `As'`
- `dominio` ← `domain`
- `rho` ← `taxa`

**Assessment:**
- **Pros:** More forgiving, reduces frustration, enables natural language use
- **Cons:** None - aliases are free
- **Decision:** IMPLEMENT - zero downside
- **Bonus:** Enhanced error messages listing valid field names

---

#### P2.1: Reinforcement Bar Arrangement Selector Tool
**Status:** ❌ REJECTED - Bloat
**Priority:** N/A
**Rationale:** Violates Unix philosophy and tool scope

**What was proposed:**
```bash
$ bar_selector --As=6.36 --bw=20
4φ16mm(8.04) | 3φ16mm+2φ12.5mm(6.49) | 5φ12.5mm(6.14)
```

**Why it's bloat:**
1. **Requires engineering judgment** - bar spacing depends on:
   - Aggregate size (varies by project)
   - Construction practices (varies by region/contractor)
   - Congestion from other reinforcement (stirrups, secondary bars)
   - Splice locations (stagger requirements)
   - Formwork details (affects cover)

2. **Violates Unix philosophy** - tools should do CALCULATION, not DESIGN
   - `flexao` calculates required As ✅
   - Suggesting "4φ16mm" is detailing work ❌

3. **False sense of automation** - AI agent should maintain engineering oversight
   - Detailing requires context the tool doesn't have
   - Engineer (or AI engineer) must consider constructability

4. **Code complexity** - would need:
   - Spacing rules database (varies by code year)
   - Mixed bar combination logic (exponential complexity)
   - Lap splice considerations
   - Layer arrangement optimization

**Decision:** REJECT
**Alternative:** AI agents can use simple heuristics:
- `As = 6.36 cm²` → suggest `4×(6.36/4) = 4×1.59 cm²` → lookup 1.59 cm² → ~φ14mm (use φ16mm)
- This is trivial logic the AI can do without a tool

**Lesson:** Don't automate engineering judgment - keep tools focused on deterministic math

---

#### P2.2: Combined Beam Design Tool (viga)
**Status:** ❌ REJECTED - Bloat
**Priority:** N/A
**Rationale:** Violates separation of concerns

**What was proposed:**
```bash
$ viga --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9 --vk=54.6
As=6.36 Asw=2.05 dominio=2 rho=0.64
```

**Why it's bloat:**
1. **Loses modularity** - can't check bending without shear, or vice versa
2. **Less flexible** - what if I only want to update shear design?
3. **Parameter explosion** - need all params for both calculations upfront
4. **Violates Unix philosophy** - each tool should do ONE thing

**Current approach is superior:**
```bash
$ flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9
$ cisalhamento --bw=20 --d=46 --fck=25 --fyk=500 --vk=54.6
```

**Decision:** REJECT
**Rationale:** If AI wants combined output, it can call both tools in parallel (which Claude Code did successfully)

---

#### P2.3: Intermediate Calculation Values in JSON
**Status:** 🟡 Future Enhancement
**Priority:** Low
**Rationale:** Useful for education/debugging, but niche use case

**Proposed:**
```json
{
  "inputs": {"bw": 20, "h": 50, "d": 46, "fck": 25, "fyk": 500, "mk": 81.9},
  "intermediate": {
    "fcd": 17.86,
    "fyd": 434.78,
    "kmd": 0.123,
    "x": 8.5
  },
  "outputs": {"As": 6.36, "As_comp": 0, "dominio": 2, "rho": 0.64}
}
```

**Assessment:**
- **Pros:** Helps verify calculations, educational value, debugging
- **Cons:** Exposes internal implementation, increases maintenance burden
- **Decision:** Add to backlog - consider for `--debug` flag instead of default JSON
- **Alternative:** Create separate verification tools (e.g., `flexao_verif`) that show work

---

#### P2.4: Batch Processing Mode
**Status:** 🟡 Future Enhancement
**Priority:** Low
**Rationale:** Nice for parametric studies, but shell loops work fine

**Proposed:**
```bash
$ flexao --batch sections.csv --output results.csv
```

**Current workaround:**
```bash
# Works perfectly fine with existing tools
$ for size in 40 45 50 55; do
    echo -n "$size,"
    flexao --bw=20 --h=$size --d=$((size-4)) --fck=25 --fyk=500 --mk=81.9 --field=As
  done
```

**Assessment:**
- **Pros:** Slightly more convenient for large parametric studies
- **Cons:** Adds CSV parsing, file I/O, increases complexity significantly
- **Decision:** REJECT - shell loops + field extraction already solve this
- **Rationale:** Unix philosophy says combine simple tools, don't build mega-tools

---

#### P2.5: Domain Explanation Flag (--explain)
**Status:** ❌ REJECTED - Bloat
**Priority:** N/A
**Rationale:** Documentation issue, not tool issue

**Proposed:**
```bash
$ flexao ... --explain
As=6.36 As'=0.00 dominio=2 rho=0.64
Note: Domain 2 = ductile failure (excellent for beams)
```

**Why it's bloat:**
1. **Educational content doesn't belong in output** - that's what docs are for
2. **AI agents don't need explanations** - they learn from SKILLS.md
3. **Bloats concise output** - defeats the purpose
4. **Redundant with --verbose** - which already explains everything

**Decision:** REJECT
**Alternative:** Ensure SKILLS.md clearly explains domain meanings:
```markdown
## Interpreting Results: Domain

- Domain 2: Ductile failure (steel yields before concrete crushes) ✅ IDEAL for beams
- Domain 3: Balanced failure (steel and concrete fail together) ✅ OK for beams/columns
- Domain 4: Brittle failure (concrete crushes before steel yields) ⚠️ Avoid in seismic zones
```

---

## Summary of Decisions

### ✅ Approved for Implementation (4)
1. **Input validation warnings** - High value, prevents typos
2. **Case-insensitive field names** - Zero downside, better UX
3. **Field name aliases** - Free usability improvement
4. **Enhanced field error messages** - Better developer experience

### 🟡 Future Enhancements (3)
1. **Units in JSON output** - Nice to have, not urgent
2. **Structured error output** - Wait for demonstrated need
3. **Intermediate values in JSON** - Consider `--debug` flag instead

### ❌ Rejected as Bloat (4)
1. **Bar arrangement selector** - Violates scope, requires engineering judgment
2. **Combined design tools** - Violates Unix philosophy
3. **Batch processing mode** - Shell loops already work
4. **Domain explanation flag** - Belongs in documentation

---

## Design Principles (Reinforced by This Analysis)

### What Makes a Good Improvement?
1. **Enhances core function** - Better calculation accuracy/validation
2. **Low complexity cost** - Simple to implement and maintain
3. **No scope creep** - Stays within "deterministic calculation" boundary
4. **Preserves Unix philosophy** - One tool, one job, composable

### What Makes a Bad Improvement?
1. **Automates judgment** - Engineering decisions belong with the engineer/AI
2. **Combines tools** - Reduces modularity and flexibility
3. **Duplicates existing capability** - Shell can already do it
4. **Bloats output** - Defeats token efficiency goal

### The Bar Selector Anti-Pattern
This proposal perfectly illustrates what to avoid:
- **Looks helpful** - "Save the AI from manual bar selection!"
- **Actually harmful** - Removes engineer oversight of critical detailing
- **High complexity** - Spacing rules, mixed combos, regional variations
- **Scope violation** - This is DESIGN work, not CALCULATION work

**Remember:** The goal is to give AI agents **tools to DO engineering**, not tools to **REPLACE engineering**.

---

## Next Steps

### Immediate Actions
1. Implement input validation warnings in all tools
2. Implement case-insensitive field names with aliases
3. Update help text to list valid field names

### Documentation Updates
1. Update SKILLS.md with domain interpretation guide
2. Add section on "Why we don't suggest bar arrangements"
3. Document shell-based batch processing patterns

### Testing Needed
1. Test input validation with common typo scenarios
2. Test field extraction with various case combinations
3. Verify that warnings don't break JSON parsing

---

## Lessons Learned

### From Claude Code Session
1. ✅ **Concise output works brilliantly** - 10x token reduction achieved
2. ✅ **Parallel tool execution** - AI called 3 tools efficiently
3. ✅ **Simple is powerful** - No complex features needed for real work
4. ⚠️ **Temptation to over-automate** - Must resist scope creep

### Quotes from Claude Code
> "The concise output format is brilliant and should be the model for other AI-agent-facing tools."

> "The tools are already highly usable as-is. The improvements I suggested are mostly 'nice to haves'."

> "Keep the core philosophy (concise output, fast execution, Unix philosophy)."

**Key Insight:** When an AI agent says "this would be nice to have," ask: "Does this help you DO the engineering, or does it try to DO the engineering FOR you?"

The answer determines whether it's an enhancement or bloat.

---

## Test Session: Grok Code Fast 1 - 2025-12-30

**Source:** `Test sessions/open code grok fast session.txt`

**Task:** Design reinforced concrete beam B1 and column C1, optimize beam section
**Result:** ✅ Successful - completed in 13.9 seconds
**Bug Discovered:** `--field=rho` failed despite `rho` appearing in default output
**Post-Session Feedback:** Interview and impressions documents provided detailed usability feedback

### Proposals from This Session

---

#### P3.1: Fix Field Extraction Bug - rho/taxa Inconsistency
**Status:** 🔴 CRITICAL BUG - Must Fix
**Priority:** Critical
**Rationale:** Breaks field extraction for commonly needed value

**Issue:**
```bash
$ flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9
As=6.36 As'=0.00 dominio=2 rho=0.64    # ✅ Shows "rho=0.64"

$ flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9 --field=rho
Error: Unknown field "rho"              # ❌ Claims "rho" doesn't exist
Valid fields: As, As_comp, dominio, taxa, ami, amilim, verificacao
```

**Root Cause:** Internal field name is `taxa` but default output displays as `rho`

**Proposed Fix:**
Support both `--field=taxa` and `--field=rho` as aliases for the same value
```pascal
// In field extraction code
normalizedField := LowerCase(fieldOutput);
if (normalizedField = 'rho') or (normalizedField = 'taxa') then
  WriteLn(taxa:0:2);
```

**Additional Aliases to Add:**
- `rho` ← `taxa` (reinforcement ratio)
- `As'` ← `As_comp` (compression steel)
- `domain` ← `dominio` (English variant)

**Assessment:**
- **Pros:** Fixes broken functionality, improves usability, no backward compat issues
- **Cons:** None
- **Decision:** IMPLEMENT IMMEDIATELY - critical bug affecting AI agent workflows
- **Impact:** High - Grok Code Fast 1 had to work around this issue during test session

---

#### P3.2: Enhanced --help Output with Units, Ranges, and Examples
**Status:** ✅ Approved for Implementation
**Priority:** High
**Rationale:** Reduces doc lookups, prevents common input errors

**Current Help (flexao):**
```
Usage: flexao --bw=VALUE --h=VALUE --d=VALUE --fck=VALUE --fyk=VALUE --mk=VALUE
```

**Proposed Enhanced Help:**
```
Usage: flexao --bw=VALUE --h=VALUE --d=VALUE --fck=VALUE --fyk=VALUE --mk=VALUE [OPTIONS]

Required Parameters:
  --bw     Section width (cm, typical: 12-100)
  --h      Total section height (cm, typical: 20-150)
  --d      Effective depth (cm, typical: 0.85-0.95 × h)
  --fck    Concrete strength (MPa, typical: 20, 25, 30, 35, 40)
  --fyk    Steel yield strength (MPa, typical: 500=CA-50, 600=CA-60)
  --mk     Characteristic moment (kN.m, positive values)

Output Formats:
  (default)  Concise: As=X.XX As'=X.XX dominio=X rho=X.XX
  --verbose  Human-readable table with explanations
  --json     JSON structured output
  --field=X  Extract single value (As, As'/As_comp, dominio/domain, rho/taxa, ami, amilim)

Example:
  flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9
  Output: As=6.36 As'=0.00 dominio=2 rho=0.64

Exit Codes:
  0 = Success
  1 = Calculation error (section insufficient, etc.)
  2 = Invalid arguments
```

**Assessment:**
- **Pros:** Self-documenting, reduces errors, improves onboarding for new AI agents
- **Cons:** Slightly longer help text (but AI agents requested this)
- **Decision:** IMPLEMENT - AI feedback was unanimous on this
- **Apply to:** All tools (flexao, cisalhamento, flexo_compressao, torcao, flexo_tracao)

---

#### P3.3: Input Validation with Descriptive Error Messages
**Status:** ✅ Approved for Implementation
**Priority:** High
**Rationale:** Catches typos (e.g., fck=250 instead of fck=25), provides actionable guidance

**Current Behavior:**
```bash
$ flexao --bw=20 --h=50 --d=46 --fck=250 --fyk=500 --mk=81.9
# May produce nonsensical results or crash
```

**Proposed Behavior:**
```bash
$ flexao --bw=20 --h=50 --d=46 --fck=250 --fyk=500 --mk=81.9
Warning: Unusual concrete grade fck=250.0 MPa (typical range: 15-90 MPa)
Warning: Did you mean fck=25 MPa?
# ... continues with calculation using fck=250
```

**Validation Rules:**

**Warnings (stderr, don't halt):**
- `fck < 15 or > 90 MPa` - Warn about unusual concrete grade
- `fyk not in {250, 500, 600}` - Warn about unusual steel grade
- `bw > 100 cm (beams) or > 150 cm (columns)` - Warn about large width
- `cover < 1.5 or > 5 cm` - Warn about unusual cover
- `rho > 4%` - Warn about high reinforcement ratio (result)

**Errors (stderr, halt with exit code 2):**
- `d >= h` - Geometric impossibility
- `bw <= 0 or h <= 0` - Invalid dimensions
- `fck <= 0 or fyk <= 0` - Invalid material properties
- `mk < 0` - Negative moment (use absolute value or flip sign)

**Implementation Pattern:**
```pascal
procedure ValidateInputs;
begin
  // Hard errors - halt execution
  if bw <= 0 then
  begin
    WriteLn(StdErr, 'Error: bw must be positive (got ', bw:0:2, ' cm)');
    Halt(2);
  end;

  if d >= h then
  begin
    WriteLn(StdErr, 'Error: d (', d:0:1, ') must be less than h (', h:0:1, ')');
    WriteLn(StdErr, 'Effective depth should be 0.8-0.95 of total height');
    Halt(2);
  end;

  // Soft warnings - continue execution
  if (fck < 15) or (fck > 90) then
  begin
    WriteLn(StdErr, 'Warning: Unusual fck=', fck:0:1, ' MPa (typical: 15-90 MPa)');
    if (fck > 100) and (fck < 1000) then
      WriteLn(StdErr, 'Did you mean fck=', (fck/10):0:1, ' MPa?');
  end;

  if not (fyk in [250, 500, 600]) then
    WriteLn(StdErr, 'Warning: Unusual fyk=', fyk:0:0, ' MPa (typical: 500 or 600)');
end;
```

**Assessment:**
- **Pros:** Prevents catastrophic typos, improves UX, helps debugging
- **Cons:** Minimal code addition, warnings to stderr don't affect parsing
- **Decision:** IMPLEMENT - high value, low cost, AI agents strongly requested this
- **Apply to:** All tools

---

#### P3.4: Standardize Status Output Format
**Status:** ✅ Approved for Implementation
**Priority:** Medium
**Rationale:** Current format `[OK__Armadura_minima]` is harder to parse than `status=ARMADURA_MINIMA`

**Current Output (cisalhamento):**
```bash
Asw=2.05 twd=0.8309 twu=4.3393 [OK__Armadura_minima]
```

**Proposed Output:**
```bash
Asw=2.05 twd=0.83 twu=4.34 status=ARMADURA_MINIMA
```

**Changes:**
1. Remove `[OK__` prefix, use clean `status=VALUE` field
2. Reduce decimal precision (0.8309 → 0.83) for conciseness
3. Make `status` extractable via `--field=status`
4. Only show status when NOT "OK" (like flexao currently does)

**Status Values (standardized):**
- `OK` - Design is adequate (omit from default output)
- `ARMADURA_MINIMA` - Using minimum reinforcement
- `BIELA_INSUFICIENTE` - Compressed strut insufficient
- `SECAO_INSUFICIENTE` - Section insufficient (increase dimensions)

**Assessment:**
- **Pros:** Easier parsing, consistent with flexao, field-extractable
- **Cons:** Minor breaking change (but tools are early stage)
- **Decision:** IMPLEMENT - improves consistency across tools
- **Apply to:** cisalhamento, flexo_compressao, all future tools

---

#### P3.5: Version Information and NBR 6118 References
**Status:** ✅ Approved for Implementation
**Priority:** Medium
**Rationale:** Helps with debugging, reproducibility, and understanding code sections

**Proposed `--version` Flag:**
```bash
$ flexao --version
flexao v1.0.0
NBR 6118:2014 - Section 17.2 (Simple Bending)
Algorithm: Araújo (2014) - Chapter 2
Compiled: 2025-12-30 with FPC 3.2.2
```

**Add to --verbose Output:**
```
============================================
  FLEXAO v1.0.0 - NBR 6118:2014 (17.2)
============================================
Inputs:
  bw = 20.0 cm
  ...
```

**Implementation:**
```pascal
const
  TOOL_VERSION = '1.0.0';
  NBR_SECTION = '17.2';
  NBR_YEAR = '2014';
  ALGORITHM_REF = 'Araújo (2014) - Chapter 2';

procedure ShowVersion;
begin
  WriteLn(ProgramName, ' v', TOOL_VERSION);
  WriteLn('NBR 6118:', NBR_YEAR, ' - Section ', NBR_SECTION, ' (Simple Bending)');
  WriteLn('Algorithm: ', ALGORITHM_REF);
  WriteLn('Compiled: ', {$I %DATE%}, ' with FPC ', {$I %FPCVERSION%});
  Halt(0);
end;
```

**Assessment:**
- **Pros:** Debugging aid, reproducibility, educational reference
- **Cons:** Minimal - adds ~10 lines of code per tool
- **Decision:** IMPLEMENT - useful for long-term maintenance
- **Apply to:** All tools

---

#### P3.6: Parameter Documentation Improvements (dl ambiguity)
**Status:** ✅ Approved for Implementation
**Priority:** Medium
**Rationale:** AI agent noted `dl` (in flexo_compressao) is unclear - is it cover, effective cover, or centroid distance?

**Current Help (flexo_compressao):**
```
--dl     Distance to reinforcement layer (cm)
```

**Proposed Enhanced Help:**
```
--dl     Distance from face to reinforcement centroid (cm)
         (Typically: cover + stirrup diameter + half bar diameter)
         Example: cover=3cm, stirrup=0.5cm, bar=1.6cm → dl=4.3cm
         For symmetric reinforcement, use dl=d'=same value
```

**Alternative Approach:**
Add `--help-param=X` flag for detailed parameter explanations:
```bash
$ flexo_compressao --help-param=dl
Parameter: dl (Distance to reinforcement layer)

Definition: Distance from concrete face to the centroid of the reinforcement layer

Calculation: dl = cover + stirrup_diameter + (bar_diameter / 2)

Example:
  - Concrete cover: 3.0 cm
  - Stirrup diameter: 0.5 cm (φ5mm)
  - Main bar diameter: 1.6 cm (φ16mm)
  - Result: dl = 3.0 + 0.5 + (1.6/2) = 4.3 cm

Typical values: 3.5-5.0 cm for columns
```

**Assessment:**
- **Pros:** Clarifies confusing parameter, improves usability
- **Cons:** Longer help text (but only shown when requested)
- **Decision:** IMPLEMENT - Option 1 (enhanced inline help), Option 2 as future enhancement
- **Apply to:** All parameters with potential ambiguity

---

#### P3.7: Pre-Calculation Validation Flag (--validate)
**Status:** 🟡 Future Enhancement
**Priority:** Medium
**Rationale:** Useful for parameter sanity checks before running calculations

**Proposed Usage:**
```bash
$ flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9 --validate

Input Validation: PASSED
- Geometric ratios: OK (d/h = 0.92)
- Material strengths: OK (fck=25 MPa, fyk=500 MPa)
- Moment magnitude: OK (Mk=81.9 kN.m)
- Estimated reinforcement ratio: ~0.5-1.0% (within typical range)
- Estimated domain: 2 or 3 (ductile design)

No errors detected. Ready for calculation.
```

**Use Cases:**
- Quick sanity check before batch processing
- Debugging input generation scripts
- Teaching tool (shows expected ranges)

**Assessment:**
- **Pros:** Helps catch errors early, educational value
- **Cons:** Duplicates some work from P3.3 (input validation), moderate complexity
- **Decision:** Add to backlog - useful but not urgent (P3.3 covers most of this)
- **Implementation:** Could be a simple wrapper around existing validation logic

---

#### P3.8: Batch Processing Mode with JSON Input
**Status:** 🟡 Future Enhancement
**Priority:** Medium
**Rationale:** Convenient for parametric studies, but shell loops already work

**Proposed Usage:**
```bash
# Create JSON input file
$ cat batch_input.json
{
  "cases": [
    {"bw": 20, "h": 50, "d": 46, "fck": 25, "fyk": 500, "mk": 81.9},
    {"bw": 20, "h": 55, "d": 51, "fck": 25, "fyk": 500, "mk": 81.9},
    {"bw": 20, "h": 60, "d": 56, "fck": 25, "fyk": 500, "mk": 81.9}
  ]
}

# Run batch calculation
$ flexao --batch=batch_input.json --json

# Output
{
  "results": [
    {"As": 6.36, "As_comp": 0.00, "dominio": 2, "rho": 0.64},
    {"As": 5.61, "As_comp": 0.00, "dominio": 2, "rho": 0.55},
    {"As": 5.01, "As_comp": 0.00, "dominio": 2, "rho": 0.48}
  ]
}
```

**Current Workaround (shell loop):**
```bash
# Works perfectly fine with existing tools
for h in 50 55 60; do
  d=$((h - 4))
  echo -n "$h,"
  flexao --bw=20 --h=$h --d=$d --fck=25 --fyk=500 --mk=81.9 --field=As
done
```

**Assessment:**
- **Pros:** Slightly cleaner for large parametric studies, single process startup
- **Cons:** Adds JSON parsing, file I/O, significantly increases complexity
- **Decision:** Add to backlog - shell loops work fine, violates Unix philosophy
- **Rationale:** "Do one thing well" means one calculation per invocation

---

#### P3.9: Cross-Platform Path Handling and Distribution
**Status:** 🟡 Future Enhancement
**Priority:** Low
**Rationale:** Current tools work on Windows; Linux/macOS support desirable but not urgent

**Current Issues:**
- Windows-specific path separators (`\`) in examples
- No pre-compiled binaries for Linux/macOS
- Path handling feels "less natural" on Windows (per AI feedback)

**Proposed Improvements:**

1. **Accept both `/` and `\` in paths** (if path args added in future)
2. **Provide pre-compiled binaries:**
   - Windows x64 (current)
   - Linux x64
   - macOS ARM64 (M1/M2)
3. **Add GitHub Actions for multi-platform builds:**
   ```yaml
   - name: Build for all platforms
     run: |
       fpc -O2 -Xs -Twin64 src/flexao.pas -o bin/flexao-win64.exe
       fpc -O2 -Xs -Tlinux src/flexao.pas -o bin/flexao-linux-x64
       fpc -O2 -Xs -Tdarwin src/flexao.pas -o bin/flexao-macos-arm64
   ```

**Docker Consideration (REJECTED):**
- **Pro:** Consistent environment across platforms
- **Con:** Violates "no dependencies" principle
- **Decision:** Keep native binaries approach

**Assessment:**
- **Pros:** Broader adoption, better Linux/Mac experience
- **Cons:** Maintenance overhead (3x platforms to test)
- **Decision:** Add to backlog - Windows works fine, expand when demand exists
- **Note:** Tools currently don't take file paths as input, so path handling is not critical

---

#### P3.10: Documentation Streamlining for AI Agent Efficiency
**Status:** ✅ Approved for Implementation
**Priority:** Low
**Rationale:** AI agent feedback noted documentation is "slightly verbose" with "redundant examples"

**Key Feedback:**
- SKILLS.md is "effective but slightly verbose with redundant examples"
- Philosophical content (Unix philosophy, Why Pascal) is "less essential for usage"
- "Focus docs on parameters, outputs, and examples; trim philosophical fluff to avoid token waste"

**Proposed Changes:**

**For test_sandbox/CLAUDE.md (AI agents USING the tools):**
1. ✅ Keep: "Unix-like tool" description (sets expectations for CLI behavior)
2. ❌ Remove: "Why Pascal?" section (irrelevant to tool users)
3. ❌ Remove: Deep philosophy explanations about design choices
4. ✅ Add: "Quick Start for AI Agents" section at top
5. ✅ Keep: All parameter tables, output examples, usage patterns

**For CLAUDE.md (root - DEVELOPING the tools):**
1. ✅ Keep: Everything including "Why Pascal?" (relevant for developers)
2. ✅ Keep: Design philosophy and implementation details
3. No changes needed

**For SKILLS.md:**
1. ✅ Condense: Reduce repetitive output mode examples (show each mode once, not 5 times)
2. ✅ Add: "Common Workflows" appendix with bash scripting patterns
3. ✅ Keep: Units, tool references, engineering context (AI found these valuable)
4. ✅ Restructure: Move verbose examples to collapsible sections or appendix

**Example Restructuring (SKILLS.md):**
```markdown
## Quick Reference

### flexao - Bending Design
**Purpose:** Calculate required reinforcement for rectangular sections under bending
**Units:** All dimensions in cm, forces in kN, moments in kN.m
**Output:** As=6.36 As'=0.00 dominio=2 rho=0.64

**Concise example:**
flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9

<details>
<summary>See all output modes (verbose, json, field extraction)</summary>

[... detailed examples here ...]

</details>
```

**Assessment:**
- **Pros:** Faster AI onboarding, reduced token usage, clearer structure
- **Cons:** Requires careful editing to preserve essential information
- **Decision:** IMPLEMENT - AI feedback was clear about this
- **Priority:** Low (current docs work fine, but improvements valuable long-term)

**Important Note:**
- There are TWO separate CLAUDE.md files:
  - `test_sandbox/CLAUDE.md` - For AI agents using the tools (trim philosophy)
  - `CLAUDE.md` (root) - For developers building tools (keep philosophy)

---

### P3.11: Additional Tools - Remove from Roadmap
**Status:** ❌ REJECTED - Bloat
**Priority:** N/A
**Rationale:** User requested removal of these from roadmap if present

**AI Agent Suggested:**
- Slab design tools (laje_detalhada)
- Foundation design
- Crack width verification (fissuracao) - **Already in roadmap, keep this one**
- Anchorage length (ancoragem) - **Already in roadmap, keep this one**

**Decision:**
- ❌ Remove: Slab design, foundation design (out of scope for initial release)
- ✅ Keep: fissuracao, ancoragem (in existing Chapter 9, reasonable scope)
- Focus on completing existing roadmap tools first

**Existing Roadmap (from CLAUDE.md):**
1. ✅ flexao (Chapter 2) - COMPLETE
2. cisalhamento (Chapter 5)
3. flexao_t (Chapter 3)
4. flexao_verif (Chapter 4)
5. flexo_compressao (Chapter 6)
6. torcao (Chapter 8)
7. flexo_tracao (Chapter 9)

**Keep this focused scope.** Don't add slabs, foundations, or other complex tools.

---

## Summary of Grok Code Fast 1 Decisions

### 🔴 Critical Bug Fix (1)
1. **Fix rho/taxa field extraction** - Breaks current workflows

### ✅ Approved for Implementation (6)
1. **Enhanced --help output** - Units, ranges, examples
2. **Input validation warnings** - Prevent typos like fck=250
3. **Standardize status output** - Clean `status=VALUE` format
4. **Version information** - --version flag and NBR references
5. **Parameter documentation** - Clarify ambiguous params like `dl`
6. **Documentation streamlining** - Remove philosophy from user-facing docs

### 🟡 Future Enhancements (3)
1. **Validation flag (--validate)** - Pre-calc sanity checks
2. **Batch processing mode** - JSON input arrays (low priority, shell loops work)
3. **Cross-platform binaries** - Linux/macOS support when demand exists

### ❌ Rejected (1)
1. **Expanded tool roadmap** - Keep focused on existing 7 tools from reference book

---

## Updated Summary Across All Test Sessions

### ✅ Approved for Implementation (10 total)
**From Claude Code Session:**
1. Input validation warnings
2. Case-insensitive field names
3. Field name aliases
4. Enhanced field error messages

**From Grok Code Fast 1 Session:**
5. Fix rho/taxa field extraction bug (CRITICAL)
6. Enhanced --help output
7. Standardize status output format
8. Version information flag
9. Parameter documentation improvements
10. Documentation streamlining for AI efficiency

### 🟡 Future Enhancements (6 total)
**From Claude Code Session:**
1. Units in JSON output
2. Structured error output
3. Intermediate values in JSON

**From Grok Code Fast 1 Session:**
4. Validation flag (--validate)
5. Batch processing mode
6. Cross-platform binary distribution

### ❌ Rejected as Bloat (5 total)
**From Claude Code Session:**
1. Bar arrangement selector
2. Combined design tools
3. Batch processing via CSV (different from JSON proposal)
4. Domain explanation flag

**From Grok Code Fast 1 Session:**
5. Expanded tool roadmap (slabs, foundations)

---

## Lessons Learned

### From Grok Code Fast 1 Session
1. ✅ **Bug discovered in real use** - Field extraction aliases critical
2. ✅ **Documentation feedback valuable** - AI agents want concise, practical docs
3. ✅ **Help text matters** - Inline units/examples reduce friction
4. ⚠️ **Scope discipline required** - Resist adding tools beyond reference book chapters

### Quotes from Grok Code Fast 1
> "The tools integrate seamlessly into my workflows, allowing quick structural calculations without manual math."

> "Yes, decisively [better than LLM calculations]. LLM-based calculations rely on trained knowledge, which may not perfectly match NBR 6118 specifics."

> "Details like being 'made in Pascal' or deep philosophy explanations are less essential for usage... agents prioritize practicality."

**Key Insight:** AI agents value **practical usability** (help text, validation, clear errors) over **philosophical explanations** (why Pascal, Unix philosophy deep dives). Keep docs focused on "how to use" not "why it exists."

---

## Test Session: Big Pickle (OpenCode) - 2025-12-30

**Source:** `Test sessions/open code big pickle session.txt`

**Task:** Design reinforced concrete beam B1 and column C1 for a building
**Result:** ✅ Successful - all calculations completed correctly
**Post-Session Feedback:** Comprehensive AI agent experience document provided

### Proposals from This Session

---

#### P4.1: Enhanced Error Messages for Calculation Failures
**Status:** ✅ Approved for Implementation
**Priority:** High
**Rationale:** Generic "ERRO" messages don't provide actionable guidance

**Current Behavior:**
```bash
$ flexao --bw=5 --h=10 --fck=25 --fyk=500 --mk=100
ERRO
(exit code 1)
```

**Proposed Behavior:**
```bash
$ flexao --bw=5 --h=10 --fck=25 --fyk=500 --mk=100
ERRO: Section insufficient (domain 4 - over-reinforced)

Suggestions:
  - Increase height from 10cm to >25cm, OR
  - Increase width from 5cm to >15cm, OR
  - Use higher concrete grade (current: fck=25 MPa)
  - Add compression reinforcement (double reinforced section)

(exit code 1)
```

**Design Guidance by Error Type:**

1. **Domain 4 (Over-reinforced):**
   ```
   ERRO: Section insufficient - domain 4 (over-reinforced, brittle failure)
   The concrete will crush before steel yields - avoid in seismic zones.

   Solutions (in order of preference):
   1. Increase section height: h >XX cm (most effective)
   2. Increase section width: bw >XX cm
   3. Use double reinforcement (As + As')
   4. Increase concrete grade: fck >XX MPa
   ```

2. **Compressed Strut Insufficient (Shear):**
   ```
   ERRO: Compressed concrete strut insufficient
   Maximum shear capacity exceeded (Vd > Vrd2).

   Solutions:
   1. Increase section width: bw >XX cm (recommended)
   2. Increase concrete grade: fck >XX MPa
   3. Reduce applied shear load
   ```

3. **Geometric Errors:**
   ```
   ERRO: Effective depth d (XX cm) must be less than total height h (XX cm)
   Typical relationship: d = 0.85-0.95 × h
   Example: For h=50cm, use d=46-48cm
   ```

**Output Modes:**
- **Default/Concise:** Show error code only: `ERRO: DOMAIN_4`
- **Verbose:** Show full explanation with suggestions (as above)
- **JSON:**
  ```json
  {
    "error": "DOMAIN_4",
    "message": "Section insufficient - over-reinforced",
    "suggestions": ["Increase h to >25cm", "Increase bw to >15cm", "Use fck >25 MPa"],
    "current_values": {"bw": 5, "h": 10, "fck": 25}
  }
  ```

**Implementation Pattern:**
```pascal
procedure ReportError(errorType: string; details: string);
begin
  // Concise mode
  if not verboseOutput then
  begin
    WriteLn(StdErr, 'ERRO: ', errorType);
    Halt(1);
  end;

  // Verbose mode
  WriteLn(StdErr, 'ERRO: ', errorType);
  WriteLn(StdErr, '');
  WriteLn(StdErr, details);
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Run with --help for parameter guidance');
  Halt(1);
end;
```

**Assessment:**
- **Pros:** Dramatically improves UX, reduces trial-and-error, educational value
- **Cons:** Requires calculation of suggested values (moderate complexity)
- **Decision:** IMPLEMENT - High value improvement requested by multiple AI agents
- **Apply to:** All tools (flexao, cisalhamento, flexo_compressao, etc.)

---

#### P4.2: Decision Tree Workflows in Documentation
**Status:** 🟡 Future Enhancement (Very Long Term)
**Priority:** Low
**Rationale:** Helpful for complex multi-tool workflows, but not urgent

**Proposed Addition to Documentation:**

**For SKILLS.md (AI Agent Usage Guide):**
```markdown
## Common Structural Design Workflows

### Workflow 1: Simple Beam Design (Bending + Shear)

1. **Calculate design loads** (manual or load tool)
   - Dead load + Live load → Factored loads
   - Md = design bending moment (kN.m)
   - Vd = design shear force (kN)

2. **Design for bending:**
   ```bash
   flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9
   ```
   - ✅ If `dominio=2 or 3`: Continue to step 3
   - ❌ If `dominio=4`: STOP → Increase section height or width

3. **Design for shear:**
   ```bash
   cisalhamento --bw=20 --d=46 --fck=25 --fyk=500 --vk=54.6
   ```
   - ✅ If `OK` or `ARMADURA_MINIMA`: Continue to step 4
   - ❌ If `BIELA_INSUFICIENTE`: STOP → Increase bw or fck

4. **Check deflection** (manual calculation or future tool)

5. **Detail reinforcement** (bar selection, spacing, drawing)

---

### Workflow 2: Column Design (Axial + Bending)

1. **Calculate design loads**
   - Nd = axial compression (kN)
   - Md = bending moment (kN.m)

2. **Design for flexo-compression:**
   ```bash
   flexo_compressao --bw=25 --h=50 --dl=3.5 --nk=600 --mk=85 --fck=25 --fyk=500
   ```
   - Check if `dominio=3` (balanced) or `dominio=4` (compression-controlled)
   - Verify `As >= Asmin` (typically 0.4% for columns)

3. **Check slenderness** (manual - is column short or slender?)
   - If slender: Consider second-order effects

4. **Detail reinforcement** (minimum 4 bars for rectangular columns)

---

### Workflow 3: Iterative Section Optimization

Goal: Find the most economical section for a given moment.

```bash
# Compare multiple section heights
for h in 40 45 50 55 60; do
  d=$((h - 4))
  echo -n "h=$h: As="
  flexao --bw=20 --h=$h --d=$d --fck=25 --fyk=500 --mk=81.9 --field=As
done

# Output:
# h=40: As=8.45
# h=45: As=7.12
# h=50: As=6.36
# h=55: As=5.61
# h=60: As=5.01
```

Select section based on:
- Reinforcement area (lower is more economical)
- Architectural constraints (height restrictions)
- Constructability (formwork standardization)
```

**Workflow Decision Tree Diagram (ASCII):**
```
BEAM DESIGN
│
├─ Calculate Md, Vd
│
├─ Run: flexao --bw=X --h=Y --mk=Md
│  │
│  ├─ dominio=2/3? ──YES──> Continue
│  │
│  └─ dominio=4? ──YES──> Increase h or bw, retry
│
├─ Run: cisalhamento --bw=X --d=Y --vk=Vd
│  │
│  ├─ OK/ARMADURA_MINIMA? ──YES──> Continue
│  │
│  └─ BIELA_INSUFICIENTE? ──YES──> Increase bw, retry
│
├─ Check deflection (manual)
│
└─ Detail reinforcement (bar selection)
```

**Assessment:**
- **Pros:** Guides new users, demonstrates tool composition, reduces learning curve
- **Cons:** Adds documentation length, needs maintenance as tools evolve
- **Decision:** Add to backlog - Implement when we have 5+ tools (critical mass for workflows)
- **Priority:** Very Low - Current tools are self-explanatory, this is "nice to have"
- **Note:** Wait until we have flexao, cisalhamento, flexo_compressao, torcao all implemented

**Implementation Timeline:**
- **Now:** Skip - only 1 tool fully implemented (flexao)
- **After 3-4 tools:** Consider adding basic workflow examples
- **After all 7 tools:** Add comprehensive decision trees and multi-tool workflows

---

#### P4.3: CLAUDE.md Reorganization for AI Agent Efficiency
**Status:** ✅ Approved for Implementation
**Priority:** Medium
**Rationale:** test_sandbox/CLAUDE.md is verbose with philosophy that belongs in root CLAUDE.md

**Current Situation:**
- Root [CLAUDE.md](../CLAUDE.md) - For developers building tools (305 lines) ✅ Keep all content
- [test_sandbox/CLAUDE.md](../test_sandbox/CLAUDE.md) - For AI agents using tools ⚠️ Too verbose, has development content

**Issue from Big Pickle Feedback:**
> "CLAUDE.md is quite verbose (305 lines), has redundancy in examples, and includes installation/compilation details not relevant for end users. Documentation mixes usage with development concerns."

**Root Cause:**
AI agent saw test_sandbox/CLAUDE.md but thought it was also the developer guide because it contained development-oriented content (compilation instructions, algorithm references, etc.). This made it feel bloated for a usage guide.

**Proposed Reorganization:**

**For [test_sandbox/CLAUDE.md](../test_sandbox/CLAUDE.md) (AI Agent Usage Guide):**

```markdown
# Reinforced Concrete Design Tools - AI Agent Guide

## Quick Start (Lines 1-50)

### Available Tools
| Tool | Purpose | Example |
|------|---------|---------|
| flexao | Beam bending design | `flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9` |
| cisalhamento | Shear design | `cisalhamento --bw=20 --d=46 --fck=25 --fyk=500 --vk=54.6` |
| flexo_compressao | Column design | `flexo_compressao --bw=25 --h=50 --dl=3.5 --nk=600 --mk=85 --fck=25 --fyk=500` |

### Units Reference (Essential!)
- Dimensions: cm
- Forces: kN
- Moments: kN.m
- Stresses: MPa

### Output Modes
- Default (concise): `As=6.36 As'=0.00 dominio=2 rho=0.64`
- `--verbose`: Human-readable table
- `--json`: Structured output
- `--field=X`: Single value extraction

### Exit Codes
- 0 = Success
- 1 = Calculation error (section insufficient, etc.)
- 2 = Invalid arguments

---

## Tool Reference Cards (Lines 51-150)

### flexao - Rectangular Beam Bending
**Purpose:** Calculate required reinforcement for rectangular sections under bending
**Required Parameters:** bw h d fck fyk mk
**Output:** As As_comp dominio rho
**Common Issues:**
- `dominio=4` → Over-reinforced, increase h
- `dominio=2` → Ductile design ✅ (ideal)

**Example:**
```bash
flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=81.9
# Output: As=6.36 As'=0.00 dominio=2 rho=0.64
```

[Repeat for each tool]

---

## Workflows & Patterns (Lines 151-200)

[Shell scripting patterns, common workflows]

---

## Troubleshooting (Lines 201-250)

[Common errors and solutions]

---

## Detailed Reference (Lines 251+)

[Full parameter descriptions, engineering context, etc.]
```

**What to REMOVE from test_sandbox/CLAUDE.md:**
- ❌ "Why Pascal?" section (belongs in root CLAUDE.md)
- ❌ Compilation instructions (belongs in root CLAUDE.md)
- ❌ FPC flags and optimization details (belongs in root CLAUDE.md)
- ❌ Algorithm conversion strategy (belongs in root CLAUDE.md)
- ❌ Deep Unix philosophy explanations (brief mention OK, deep dive not needed)

**What to KEEP in test_sandbox/CLAUDE.md:**
- ✅ "Unix-like tool" description (sets CLI expectations)
- ✅ All parameter tables
- ✅ Output examples and usage patterns
- ✅ Units reference
- ✅ Troubleshooting guide
- ✅ Engineering context (domains, reinforcement ratios, etc.)

**Reorganization Strategy:**
1. Create "Quick Start" section (first 50 lines) with essentials
2. Add "Tool Reference Cards" (one-page cheat sheet per tool)
3. Move verbose examples to appendix or collapsible sections
4. Remove development-oriented content → root CLAUDE.md
5. Target: Reduce from ~300 lines to ~150-200 lines of focused usage content

**Assessment:**
- **Pros:** Faster AI onboarding, reduced token usage, clearer separation of concerns
- **Cons:** Requires careful editing to preserve essential engineering information
- **Decision:** IMPLEMENT - Aligns with P3.10, clarifies the dual-documentation structure
- **Priority:** Medium - Current docs work, but improvement is valuable

**Important Note:**
This is NOT about creating separate docs (they already exist), but about **reorganizing test_sandbox/CLAUDE.md** to remove development bloat and improve AI agent efficiency.

---

## Summary of Big Pickle Session Decisions

### ✅ Approved for Implementation (2)
1. **Enhanced error messages** - Actionable guidance for calculation failures
2. **CLAUDE.md reorganization** - Trim test_sandbox version, keep root version intact

### 🟡 Future Enhancements (1)
1. **Decision tree workflows** - Add after 5+ tools implemented (very long term)

### ❌ Rejected as Bloat (4)
1. **Bar arrangement selector** - Violates scope, requires engineering judgment
2. **Load combination calculator** - Should be separate tool or manual calculation
3. **Optimization routines** - Field extraction already enables this
4. **Design checks (crack width, deflection)** - Future separate tools, not additions

---

## Updated Summary Across All Test Sessions

### ✅ Approved for Implementation (12 total)
**From Claude Code Session:**
1. Input validation warnings
2. Case-insensitive field names
3. Field name aliases
4. Enhanced field error messages

**From Grok Code Fast 1 Session:**
5. Fix rho/taxa field extraction bug (CRITICAL)
6. Enhanced --help output
7. Standardize status output format
8. Version information flag
9. Parameter documentation improvements
10. Documentation streamlining for AI efficiency

**From Big Pickle Session:**
11. Enhanced error messages for calculation failures
12. CLAUDE.md reorganization (test_sandbox version)

### 🟡 Future Enhancements (7 total)
**From Claude Code Session:**
1. Units in JSON output
2. Structured error output (JSON)
3. Intermediate values in JSON

**From Grok Code Fast 1 Session:**
4. Validation flag (--validate)
5. Batch processing mode
6. Cross-platform binary distribution

**From Big Pickle Session:**
7. Decision tree workflows (very long term - after 5+ tools)

### ❌ Rejected as Bloat (8 total)
**From Claude Code Session:**
1. Bar arrangement selector
2. Combined design tools
3. Batch processing via CSV
4. Domain explanation flag

**From Grok Code Fast 1 Session:**
5. Expanded tool roadmap (slabs, foundations)

**From Big Pickle Session:**
6. Load combination calculator
7. Optimization routines
8. Design checks as tool additions (consider as separate tools)

---

## Lessons Learned

### From Big Pickle Session
1. ✅ **Error messages are critical** - "ERRO" alone is frustrating; guidance needed
2. ✅ **Documentation clarity matters** - AI agent confused developer guide with usage guide
3. ✅ **Separation of concerns** - Keep root CLAUDE.md for devs, test_sandbox for users
4. ⚠️ **Workflow documentation timing** - Wait until tool suite is complete before adding complex workflows

### Quotes from Big Pickle
> "These tools are **significantly better** than performing calculations within the LLM for structural engineering applications."

> "The combination of accuracy, consistency, and AI-optimized output makes them ideal for agentic workflows."

> "Primary Recommendation: Implement better error handling and documentation restructuring."

> "Success Metric: If these tools can reduce engineering calculation errors by 90%+ while maintaining AI agent efficiency, they will have achieved their design goals."

**Key Insight:** AI agents overwhelmingly prefer these deterministic tools over LLM calculations, but usability polish (error messages, documentation clarity) is critical for adoption. The core concept is validated; now refine the UX.
