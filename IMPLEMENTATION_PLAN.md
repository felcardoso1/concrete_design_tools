# CLI Tools Improvements - Implementation Plan

**Status:** Ready for Implementation  
**Date:** 2025-12-30  
**Compilation:** `fpc -O2 -Xs` (optimization level 2, strip symbols)

---

## Architecture Principles (Maintained)

- **No additional files**: All improvements embedded directly in each tool's `.pas` file
- **Independent tools**: Each tool remains a single, statically-linked executable
- **Non-interactive**: All tools run and exit immediately, never wait for user input
- **Unix philosophy**: Input via arguments, output via stdout/stderr, exit codes for status

---

## Phase 1: Core Infrastructure Patterns

### 1.1 Exit Code Constants
Add to each tool (embedded, not shared):
```pascal
const
  EXIT_SUCCESS = 0;
  EXIT_CALC_ERROR = 1;      // Calculation error (insufficient section, etc.)
  EXIT_INVALID_ARGS = 2;     // Invalid arguments/parameters
  EXIT_DOMAIN_4 = 3;         // Domain 4 (brittle failure)
  EXIT_INVALID_RANGE = 4;    // Parameter out of valid range
```

### 1.2 Field Alias Resolution Function
Add to each tool (tool-specific aliases):
```pascal
function ResolveFieldName(const fieldName: string): string;
// Maps aliases to canonical names (e.g., 'rho' -> 'taxa', 'As'' -> 'As_comp')
```

### 1.3 Warning Accumulation Variable
Add to each tool:
```pascal
var
  warnings: string;  // Accumulated warnings (only shown in verbose/JSON)
```

---

## Phase 2: High Priority Improvements

### 2.1 Field Name Aliases and Discovery

**Implementation:**
- Add `ResolveFieldName()` function to each tool
- Support `--field=list` option to show available fields and aliases
- Update field matching in `OutputResults` to check aliases

**Field Aliases per Tool:**
- `flexao`: `rho` → `taxa`, `As'` → `As_comp`
- `cisalhamento`: (check existing)
- Other tools: tool-specific aliases

**Example:**
```bash
./flexao.exe --field=list
# Output:
# Available fields: As, As_comp, dominio, taxa, ami, amilim, verificacao
# Aliases: rho=taxa, As'=As_comp
```

### 2.2 Enhanced Error Messages

**Implementation:**
- Replace brief error messages with detailed, actionable ones
- Include current parameter values
- Provide specific suggestions (e.g., "Increase h to >= 45cm")
- Use appropriate exit codes (3 for Domain 4, 4 for invalid ranges)

**Error Message Format:**
```
ERRO: [Error type] - [Description]
CURRENT: [key parameters]
SUGGESTION: [actionable guidance]
```

**Important:** Errors are still non-interactive - just better text output, then exit immediately.

---

## Phase 3: Medium Priority Improvements

### 3.1 Parameter Validation and Range Checking

**Validation Rules:**
- **fck**: Minimum 10 MPa (error if below), Maximum 90 MPa (warning if above, but still run)
- **bw**: Minimum 9 cm (error if below)
- All dimensions: Must be positive (error if ≤ 0)
- Material strengths: Within NBR 6118 typical ranges

**Warning Display Strategy:**
- **Warnings appear ONLY in:**
  - Verbose mode (human-readable output)
  - JSON mode (as `"warnings"` field)
- **Warnings do NOT appear in:**
  - Default concise mode (keeps output parseable)
  - Field-specific mode (expects only numbers)

**Implementation Pattern:**
```pascal
procedure ValidateParameters;
begin
  warnings := '';  // Initialize empty
  
  // Hard errors (exit immediately)
  if fck < 10 then
  begin
    WriteLn(StdErr, 'ERRO: fck must be >= 10 MPa (received: ', fck:0:1, ' MPa)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  if bw < 9 then
  begin
    WriteLn(StdErr, 'ERRO: bw must be >= 9 cm (received: ', bw:0:1, ' cm)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  // Soft warnings (accumulate, show only in verbose/JSON)
  if fck > 90 then
    warnings := warnings + 'WARNING: fck=' + FloatToStr(fck) + 
               ' MPa exceeds 90 MPa. Formulas valid up to 90 MPa.' + LineEnding;
end;
```

### 3.2 Batch Processing Mode

**Implementation:**
- Add `--batch` flag detection
- Read from stdin when `--batch` is set
- Parse space-separated input (one calculation per line)
- Output one result per input line
- Maintain `--field` compatibility for batch mode

**Input Format:**
```
bw h d fck fyk mk [optional params]
```

**Output Format:**
- One result per input line
- Same format as single calculation (default/verbose/json/field)

### 3.3 Tool Discovery Mechanism

**Implementation:**
- Create `list-tools.sh` / `list-tools.bat` script
- Add `--help-all` flag to each tool (optional, shows all tools)

---

## Phase 4: Low Priority Improvements

### 4.1 Version and Help Information

**Implementation:**
- Add `--version` flag showing version, NBR 6118 compliance, compile date
- Enhance `ShowUsage` with:
  - Example commands
  - Exit codes documentation
  - Field descriptions
  - More detailed parameter descriptions

---

## Implementation Order

1. **Phase 2.1**: Field name aliases and `--field=list` (all 8 tools)
2. **Phase 2.2**: Enhanced error messages (all 8 tools)
3. **Phase 3.1**: Parameter validation (all 8 tools)
4. **Phase 3.2**: Batch processing (all 8 tools)
5. **Phase 3.3**: Tool discovery (new script)
6. **Phase 4.1**: Version and enhanced help (all 8 tools)
7. **Compile and Test**: All tools with `fpc -O2 -Xs`
8. **Update Documentation**: SKILLS.md with new features

---

## Compilation Command

```bash
fpc -O2 -Xs src/flexao.pas -o../bin/flexao.exe
```

Apply to all 8 tools.

---

## Testing Checklist

For each tool:
- [ ] Field aliases work (e.g., `--field=rho` → `taxa`)
- [ ] `--field=list` shows fields and aliases
- [ ] Enhanced error messages appear with context
- [ ] Parameter validation rejects invalid inputs (exit code 4)
- [ ] Warnings appear only in verbose/JSON mode
- [ ] Concise mode has no warnings (clean output)
- [ ] Batch mode processes multiple inputs
- [ ] `--version` flag works
- [ ] Enhanced `--help` shows examples and exit codes
- [ ] Tool compiles with `-O2 -Xs` successfully

