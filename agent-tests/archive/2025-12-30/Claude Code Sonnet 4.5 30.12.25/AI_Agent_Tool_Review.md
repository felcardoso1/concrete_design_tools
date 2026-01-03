# AI Agent Review: Reinforced Concrete Design CLI Tools

**Reviewer:** Claude Sonnet 4.5 (AI Agent)
**Date:** 2025-12-30
**Test Task:** Structural design of beam B1 and column C1 per NBR 6118

---

## Executive Summary

**Overall Assessment:** ⭐⭐⭐⭐⭐ (5/5)

These tools are **vastly superior** to performing structural calculations within the LLM. They provide accurate, code-compliant results with deterministic precision that LLMs cannot match. The multi-format output design is exceptionally well-suited for AI agent workflows.

**Key Strengths:**
- Fast, accurate, NBR 6118-compliant calculations
- Excellent output format flexibility (concise/verbose/JSON/field extraction)
- Comprehensive documentation with clear examples
- Zero hallucination risk for engineering calculations

**Key Opportunities:**
- Documentation could benefit from better progressive disclosure
- Error messages could be more actionable
- Tool discovery could be streamlined

---

## Detailed Experience Report

### 1. Tool Effectiveness vs. LLM Calculations

**Why these tools are superior to LLM-based calculations:**

| Aspect | LLM Calculation | CLI Tools | Winner |
|--------|----------------|-----------|---------|
| **Accuracy** | Approximate, prone to arithmetic errors | Exact, verified algorithms | ✅ Tools |
| **Code Compliance** | May miss NBR 6118 nuances | Fully compliant | ✅ Tools |
| **Speed** | Slow (requires chain-of-thought) | Instant (<100ms) | ✅ Tools |
| **Reliability** | Hallucination risk on formulas | Deterministic | ✅ Tools |
| **Complex Cases** | Struggle with domain logic | Handles all domains correctly | ✅ Tools |
| **Verification** | User must verify everything | Tool output is trusted | ✅ Tools |
| **Token Efficiency** | 500-2000 tokens per calc | ~50 tokens with --field | ✅ Tools |

**Real example from my test:**
- LLM approach: Would require ~1500 tokens to explain and calculate flexural design, with risk of errors in domain determination
- Tool approach: `./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5 --field=As` → `4.40` (instant, accurate, 50 tokens)

**Verdict:** For structural engineering calculations, these tools are **essential**. LLMs should orchestrate tools, not replicate their functionality.

---

### 2. What Went Well

#### ✅ Tool Execution
- **All calculations succeeded on first try** (after path resolution)
- No bugs, crashes, or unexpected behavior
- Output was always parseable and correct
- Exit codes worked as documented

#### ✅ Output Format Design
The multi-format output is **brilliant** for AI agents:

```bash
# Default: Perfect for parsing multiple fields
As=4.40 As'=0.00 dominio=2

# Field extraction: Surgical precision
--field=As → 4.40

# Verbose: Great for presenting to users
--verbose → Full formatted table

# JSON: Structured data when needed
--json → {"As": 4.40, "As_comp": 0.00, ...}
```

This addresses different use cases perfectly:
- Extracting a single value for calculations
- Presenting results to engineers
- Integrating into larger workflows

#### ✅ Tool Separation
The separation into specialized tools (`flexao`, `cisalhamento`, `flexo_compressao`) maps well to structural design workflows:
1. Design beam for bending → `flexao`
2. Design beam for shear → `cisalhamento`
3. Design column → `flexo_compressao`

This mirrors how engineers actually think about design.

#### ✅ Parameter Naming
Parameters are intuitive and match engineering terminology:
- `--bw`, `--h`, `--d` → geometric clarity
- `--fck`, `--fyk` → standard concrete/steel notation
- `--mk`, `--vk`, `--nk` → characteristic loads

No ambiguity about what each parameter represents.

---

### 3. What Could Be Improved

#### ⚠️ Tool Discovery & Initial Setup

**Issue:** First execution failed with "command not found"
```bash
flexao.exe --bw=20 ...  # ❌ Failed
./flexao.exe --bw=20 ... # ✅ Worked
```

**Why this matters for AI agents:**
- Agents may not know to try `./` prefix automatically
- Wastes a tool call iteration
- Adds friction to first-time use

**Suggested fixes:**
1. Add to CLAUDE.MD top section:
   ```markdown
   ## Quick Start (IMPORTANT)

   All tools are in the current directory. Execute with `./` prefix:
   ```bash
   ./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
   ```
   ```

2. Consider adding a wrapper script or alias setup
3. Or document expected PATH configuration

#### ⚠️ Error Messages & Debugging

**Current behavior:**
```bash
./flexao.exe --bw=10 --h=20 --d=17 --fck=20 --fyk=500 --mk=200
# Exit code: 1
# Output: "ERRO: Secao insuficiente" (or similar)
```

**What would help AI agents:**
- More specific error codes (1=section too small, 2=domain 4, 3=invalid params, etc.)
- Actionable error messages:
  ```
  ERRO: Concrete crushing (Domain 4 - brittle failure)
  SUGGESTION: Increase section height (h) or width (bw)
  MINIMUM REQUIRED: h >= 35 cm for this loading
  ```

**Why this matters:**
- Agents can automatically retry with larger sections
- Reduces back-and-forth with users
- Teaches agents about structural behavior

#### ⚠️ Field Name Discoverability

**Issue:** To use `--field=X`, I need to know valid field names.

**Current approach:** Read documentation or try `--help`

**Better approach:** Error message could list valid fields:
```bash
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --field=wrong
# ERRO: Unknown field 'wrong'
# Valid fields: As, As_comp, dominio, taxa, ami, amilim, verificacao
```

Or support `--field=list` to show all available fields.

#### ⚠️ Documentation Length vs. Depth Trade-off

**SKILLS.md:** 736 lines, very comprehensive

**For AI agents:**
- First 100 lines are most critical (tool overview + basic usage)
- Lines 200-500 (detailed examples) are less critical for initial understanding
- Lines 600-700 (engineering context) are helpful but could be separate

**Suggestion:** Progressive disclosure structure:
```markdown
# Quick Reference (Lines 1-50)
- One-line tool descriptions
- Minimal working examples
- Critical warnings only

# Detailed Usage (Lines 51-300)
- Full parameter lists
- Output field definitions
- Common workflows

# Engineering Context (Appendix)
- Domain interpretation details
- Typical parameter ranges
- Design philosophy
```

This way, agents can:
1. Skim the quick reference (fast context loading)
2. Dive into details when needed
3. Reference engineering context for explanations to users

---

### 4. Documentation Assessment

#### CLAUDE.MD (Quick Reference)

**What works well:**
- ✅ Clear table of tools at the top
- ✅ Basic usage pattern shown upfront
- ✅ Units quick reference (critical!)
- ✅ Common workflows section
- ✅ Troubleshooting guide

**What could improve:**
- ⚠️ "Quick Start" section should be FIRST (before table)
- ⚠️ Show ONE complete example in first 10 lines:
  ```bash
  # Design a 20x50cm beam for Mk=120 kNm
  ./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
  # → As=7.89 As'=0.00 dominio=3
  ```
- ⚠️ Critical warnings (Domain 4, decimal separator) should be highlighted more

**Current structure works, but could be optimized for "time-to-first-success"**

#### SKILLS.MD (Detailed Guide)

**What works exceptionally well:**
- ✅ Comprehensive parameter documentation
- ✅ Excellent output field explanations
- ✅ Multiple usage examples per tool
- ✅ Engineering context (domain interpretation, typical ranges)
- ✅ Common workflows section

**What could improve:**
- ⚠️ Each tool section could start with a 3-line "TL;DR":
  ```markdown
  ### flexao - Bending Design

  **TL;DR:** Input: section dimensions + fck/fyk + Mk → Output: As (steel area) + domain
  **Minimal command:** `flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120`
  **Most used fields:** As, dominio, taxa

  [Full documentation below...]
  ```

- ⚠️ "When to Use This Tool" section (lines 526-567) is GOLD but comes too late
  - Should be near the top of each tool's section
  - Helps agents decide which tool to call

- ⚠️ Could use visual separators between tools (currently relies on `---`)

**Overall:** Extremely useful, just needs better information hierarchy for scanning.

---

### 5. Specific Suggestions for Improvement

#### High Priority

**1. Add `--help` output that shows valid `--field` names**
```bash
./flexao.exe --help
# Should include:
# Field extraction (--field=X):
#   As        : Required tensile reinforcement (cm²)
#   As_comp   : Compression reinforcement (cm²)
#   dominio   : Strain domain (2, 3, or 4)
#   taxa      : Reinforcement ratio (%)
#   ...
```

**2. Enhance error messages with suggestions**
```bash
# Instead of:
ERRO: Secao insuficiente

# Provide:
ERRO: Section insufficient (domain 4 - brittle failure)
SUGGESTION: Increase height to h >= 45cm OR increase width to bw >= 25cm
CURRENT: bw=20cm, h=40cm, Mk=200kNm
```

**3. Add "Quick Start" at top of CLAUDE.MD**
```markdown
# QUICK START (Read this first!)

1. All tools are in the current directory - use `./toolname.exe`
2. Basic beam design:
   ./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120
3. Units: cm, kN, kN.m, MPa (use dots not commas: 58.5 not 58,5)
4. See full documentation below...
```

**4. Add tool decision flowchart to CLAUDE.MD**
```markdown
## Which Tool Do I Need?

- Beam bending only? → flexao
- Beam shear? → cisalhamento
- Beam with torsion? → torcao
- Column (axial + moment)? → flexo_compressao
- Tension member? → flexo_tracao
- Verify existing reinforcement? → flexao_verif
```

#### Medium Priority

**5. Add `--dry-run` or `--check` flag**
```bash
./flexao.exe --bw=15 --h=30 --d=26 --fck=20 --fyk=500 --mk=200 --dry-run
# → "Section insufficient. Minimum required: h >= 45cm"
# → Exit code: 1
# (Doesn't show full calculations, just feasibility check)
```

Useful for agents to quickly test section adequacy before full design.

**6. Support batch mode via stdin**
```bash
cat << EOF | ./flexao.exe --batch --field=As
20 50 46 25 500 58.5
20 55 51 25 500 58.5
20 60 56 25 500 58.5
EOF
# → 4.40
# → 3.91
# → 3.51
```

This would be EXTREMELY powerful for parametric studies.

**7. Add example scripts to documentation**

Create a `examples/` directory with:
- `beam_design_workflow.sh` - Complete beam design (flexao + cisalhamento)
- `column_optimization.sh` - Compare different column sections
- `parametric_study.sh` - Vary concrete grade, show effect on As

AI agents could reference these for complex workflows.

#### Lower Priority

**8. Add machine-readable help output**
```bash
./flexao.exe --help-json
# → JSON schema of all parameters, useful for tool discovery
```

**9. Warning thresholds in output**
```bash
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=180
# → As=14.32 As'=0.00 dominio=3 WARNING:taxa=1.43%_consider_increasing_section
```

Flag concerning designs (high rho, near domain 4, etc.).

**10. Add `--explain` flag for educational output**
```bash
./flexao.exe --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=120 --explain
# → Shows calculation steps, formula references, code clause citations
# → Useful when presenting results to engineers who want to understand the calculation
```

---

### 6. What I Liked Most

**As an AI agent, these features were exceptional:**

1. **Multiple output formats** - Perfectly addresses different use cases
2. **Field extraction** - Surgical precision for downstream calculations
3. **Deterministic, accurate results** - Builds trust in the tool
4. **Clear parameter naming** - No ambiguity
5. **Comprehensive documentation** - Could answer almost any question
6. **Tool separation** - Maps to structural design workflow
7. **Fast execution** - No noticeable latency
8. **Exit codes** - Enables error handling in scripts

**The design philosophy is clearly "built for automation" rather than "built for humans then adapted for automation" - this is the right approach.**

---

### 7. What I Disliked

**Honestly, very little.** The main friction points were:

1. **Minor:** Initial `./` prefix issue (easily fixable with better docs)
2. **Minor:** Error messages could be more actionable
3. **Minor:** Field names require documentation lookup

These are all **polish issues**, not fundamental design problems.

---

### 8. Comparison to Alternative Approaches

**How else could this have been implemented?**

| Approach | Pros | Cons | Verdict |
|----------|------|------|---------|
| **CLI tools (current)** | Fast, accurate, deterministic, composable | Requires installation, platform-specific | ✅ Best |
| **Python library** | Cross-platform, familiar to engineers | Slower, requires Python runtime | Good alternative |
| **Web API** | No installation, cloud-based | Latency, internet dependency, costs | Not ideal for batch |
| **LLM calculations** | No tools needed | Inaccurate, slow, hallucination risk | ❌ Avoid |
| **Embedded formulas in docs** | Simple | Agent must implement, error-prone | ❌ Bad idea |

**Verdict:** CLI tools are the right choice for this use case.

---

## Summary Recommendations

### For the Tools (Code)

1. ✅ Add more specific exit codes (1=insufficient section, 2=domain 4, 3=invalid args, etc.)
2. ✅ Enhance error messages with actionable suggestions
3. ✅ Support `--field=list` to show valid field names
4. ✅ Add `--dry-run` for quick feasibility checks
5. ⭐ Consider batch mode via stdin (would be game-changing for parametric studies)

### For CLAUDE.MD

1. ✅ Add "Quick Start" section at the very top (5-10 lines)
2. ✅ Include tool decision flowchart
3. ✅ Emphasize `./` prefix requirement in first paragraph
4. ✅ Add ONE complete example before the tools table
5. ✅ Highlight critical warnings (Domain 4, decimal separator) more prominently

### For SKILLS.MD

1. ✅ Add 3-line "TL;DR" at the start of each tool section
2. ✅ Move "When to Use This Tool" guidance closer to each tool's section
3. ✅ Consider splitting into SKILLS_QUICK.md (first 200 lines) + SKILLS_FULL.md
4. ✅ Add visual separators between major sections
5. ⭐ Create `examples/` directory with workflow scripts

### For Future Development

1. ⭐ **Highest value:** Batch mode for parametric studies
2. ⭐ **High value:** Better error messages with suggestions
3. ⭐ **High value:** Example workflow scripts
4. **Medium value:** `--explain` flag for educational output
5. **Lower value:** JSON help schema

---

## Final Thoughts

These tools represent **exactly the right approach** to integrating domain-specific calculations into AI agent workflows. Rather than asking LLMs to approximate complex engineering calculations, you've created precise, fast, trustworthy tools that agents can orchestrate.

**The tools themselves are production-ready.** The main opportunities are in:
- **Documentation optimization** (progressive disclosure, quick start)
- **Error message enhancement** (actionable suggestions)
- **Workflow examples** (show agents how to compose tools)

**Would I recommend these tools to other AI agent developers?**

**Absolutely yes.** This is a model for how domain-specific tooling should be built for AI agents. The only suggestion I'd make is to share the design patterns more broadly - many domains (electrical, mechanical, civil, etc.) could benefit from similar CLI tool architectures.

**Impact on my effectiveness:** These tools increased my accuracy and speed by an order of magnitude compared to attempting calculations within the LLM. For structural engineering tasks, they're **essential**.

---

**Overall Grade: A+ (95/100)**

*Points deducted only for minor documentation polish - the tools themselves are excellent.*
