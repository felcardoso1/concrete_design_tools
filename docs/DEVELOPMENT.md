# Pascal CLI Tools for Structural Engineering

## Project Overview

We are building a suite of lightweight CLI tools for reinforced concrete design according to NBR 6118 (Brazilian concrete design code). These tools are designed to be invoked by AI agents like Claude Code, enabling fast, deterministic calculations during agentic workflows.

### Why Pascal?
- **Microsecond startup**: No runtime initialization, no interpreter, no GC warmup
- **Single static binary**: Just compile and run, no dependencies
- **Simple syntax**: Easy to write, easy for LLMs to read and modify
- **Perfect for formulas**: Structural engineering is mostly deterministic math

### Design Philosophy
These tools follow the Unix philosophy: do one thing well. An AI agent will call these tools repeatedly during a session, so speed and predictability are paramount.

---

## Compilation

### Compiler
Free Pascal Compiler (fpc) is installed and available in PATH.

### Basic Compilation
```bash
fpc filename.pas
```
This produces an executable with the same name (filename.exe on Windows, filename on Linux).

### Recommended Flags
```bash
fpc -O2 -Xs filename.pas
```
- `-O2`: Optimization level 2
- `-Xs`: Strip debug symbols (smaller binary)

### Check for Errors
The compiler outputs warnings and errors to stdout. A successful compilation ends with lines showing linked units and the output filename.

---

## CLI Tool Conventions

Every tool in this project MUST follow these conventions:

### 1. Input via Command Line Arguments
```
toolname --param1=value1 --param2=value2
```
Or positional arguments for simple tools:
```
toolname value1 value2 value3
```

### 2. Output Format
**IMPORTANT**: Default output must be CONCISE for AI agent efficiency (10x-40x token reduction vs verbose)

- **Default**: Concise key=value format optimized for AI agents
  - Example: `As=2.98 As'=0.00 dominio=2`
  - Easy to parse with regex
  - Minimal tokens for repeated calls during design iterations
- **`--verbose`**: Human-readable formatted output with tables and explanations
- **`--json`**: Structured JSON output for programmatic parsing
- **`--field=X`**: Extract single field value (surgical precision)

### 3. Exit Codes
- `0`: Success
- `1`: Calculation error (e.g., section is insufficient)
- `2`: Invalid arguments or missing parameters

### 4. No Interactive Prompts
Never use `ReadLn` waiting for user input. All parameters must come from command line. If parameters are missing, print usage and exit with code 2.

### 5. Error Output
- Errors and warnings go to stderr
- Results go to stdout
- This allows agents to parse output cleanly

### 6. Standard Flags
Every tool must support:
- `--help`: Display usage information
- `--verbose`: Human-readable formatted output
- `--json`: JSON structured output
- `--field=X`: Extract specific field (list valid fields in help)

---

## Project Structure

```
/pascal-cli-tools
├── CLAUDE.md              (this file - implementation guide)
├── SKILLS.md              (AI agent usage guide - how Claude uses these tools)
├── src/
│   ├── flexao.pas         (bending design) ✓ COMPLETE
│   ├── cisalhamento.pas   (shear design)
│   ├── flexao_t.pas       (T-beam bending)
│   ├── flexao_verif.pas   (bending verification)
│   ├── flexo_compressao.pas (column design)
│   ├── torcao.pas         (torsion design)
│   └── flexo_tracao.pas   (tension members)
├── bin/                   (compiled binaries)
│   └── flexao.exe         ✓ COMPLETE
├── tests/                 (test cases with known results)
├── docs/                  (additional documentation)
└── reference_book/        (Fortran source algorithms)
```

---

## Reference Material

### Algorithm Source Files
The algorithms are extracted from the book "PROGRAMAS PARA DIMENSIONAMENTO E VERIFICAÇÃO DE CONCRETO ARMADO" by José Milton de Araújo, available as text files in this folder:

| File | Description | Tool to Create |
|------|-------------|----------------|
| `fortran explanation.txt` | Explanation of Fortran syntax for reference | - |
| `chapter 2_flexão normal simples.txt` | Flexural design of rectangular sections | `flexao` |
| `chapter 3_flexão normal simples seções T.txt` | Flexural design of T sections | `flexao_t` |
| `chapter 4_flexão normal simples verificação da capacidade resistente.txt` | Verification of flexural strength | `flexao_verif` |
| `chapter 5_esforço cortante.txt` | Shear design | `cisalhamento` |
| `chapter 6_flexo compressão normal dimensionamento de seções retangulares.txt` | Design for flexo-compression (columns) | `flexo_compressao` |
| `chapter 7_flexo compressão normal verificação de seções retangulares.txt` | Verification of flexo-compression | `flexo_compressao_verif` |
| `chapter 8_torção.txt` | Torsion design | `torcao` |
| `chapter 9_flexo tração normal.txt` | Flexo-tension design | `flexo_tracao` |

### How to Read the Source Files
- The files contain Fortran 90 code which is syntactically similar to Pascal
- Read `fortran explanation.txt` first if unfamiliar with Fortran syntax
- Fortran uses `!` for comments, Pascal uses `//` or `{ }`
- Fortran arrays are 1-indexed like Pascal
- `REAL` in Fortran → `Double` in Pascal
- `INTEGER` in Fortran → `Integer` in Pascal
- `IF...THEN...ELSE...END IF` structure is nearly identical

### Conversion Strategy
1. Read the relevant chapter txt file
2. Understand the inputs, outputs, and calculation logic
3. Convert Fortran to Pascal, adapting to our CLI conventions
4. Add JSON output option
5. Test with reasonable values and verify results make engineering sense

### NBR 6118 Reference
The algorithms follow NBR 6118:2014 (or later). Key parameters:
- γc = 1.4 (concrete safety factor)
- γs = 1.15 (steel safety factor)
- fck in MPa
- fyk in MPa (usually 500 MPa for CA-50)

---

## Tool Specifications

### 1. flexao (Bending Design) ✓ IMPLEMENTED
Calculates required reinforcement for rectangular sections under simple bending.

**Inputs:**
- `bw`: Section width (cm)
- `h`: Section height (cm)
- `d`: Effective depth (cm)
- `fck`: Characteristic concrete strength (MPa)
- `fyk`: Characteristic steel yield strength (MPa)
- `mk`: Characteristic bending moment (kN.m)

**Outputs:**
- `As`: Required tensile steel area (cm²)
- `As'` (As_comp): Required compression steel area (cm²)
- `dominio`: Strain domain (2, 3, or 4)
- `taxa`: Reinforcement ratio (%)
- `ami`: Reduced applied moment
- `amilim`: Limit moment for single reinforcement
- `verificacao`: Status message

**Examples:**
```bash
# Default: Concise output for AI agents
flexao --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30
# Output: As=2.98 As'=0.00 dominio=2

# Extract only As value
flexao --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --field=As
# Output: 2.98

# Human-readable format
flexao --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --verbose
# Output: Full formatted table

# JSON for programmatic parsing
flexao --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --json
# Output: {"As": 2.98, "As_comp": 0.00, "dominio": 2, ...}
```

**See [SKILLS.md](SKILLS.md) for detailed usage in AI workflows.**

### 2. cisalhamento (Shear Design)
Calculates required shear reinforcement (stirrups).

**Inputs:**
- `bw`: Section width (cm)
- `d`: Effective depth (cm)
- `fck`: Characteristic concrete strength (MPa)
- `fyk`: Characteristic steel yield strength (MPa)
- `vsd`: Design shear force (kN)

**Outputs:**
- `vrd2`: Maximum shear resistance (kN)
- `vc`: Concrete contribution (kN)
- `asw_s`: Required stirrup area per unit length (cm²/m)
- `verificacao`: "OK", "ARMADURA MINIMA", or "BIELA COMPRIMIDA INSUFICIENTE"

### 3. flexao_t (T-Beam Bending Design)
Calculates required reinforcement for T-sections under simple bending.

### 4. flexao_verif (Bending Verification)
Verifies the capacity of a section given the reinforcement (inverse of flexao).

### 5. flexo_compressao (Column Design)
Calculates required reinforcement for rectangular sections under combined compression and bending.

### 6. torcao (Torsion Design)
Calculates required reinforcement for torsional moments.

### 7. flexo_tracao (Tension Member Design)
Calculates required reinforcement for sections under combined tension and bending.

---

## Code Template

Use this as a starting point for each tool. **IMPORTANT**: This template includes the required output modes for AI agent efficiency.

```pascal
program ToolName;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

var
  // Input variables
  param1, param2: Double;
  jsonOutput, verboseOutput: Boolean;
  fieldOutput: string;

  // Output variables
  result1, result2: Double;
  status: string;

procedure ShowUsage;
begin
  WriteLn(StdErr, 'Usage: toolname --param1=VALUE --param2=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --param1   Description of param1 (unit)');
  WriteLn(StdErr, '  --param2   Description of param2 (unit)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Output Formats:');
  WriteLn(StdErr, '  (default)  Concise output for AI agents: result1=X.XX result2=X.XX');
  WriteLn(StdErr, '  --verbose  Human-readable formatted output');
  WriteLn(StdErr, '  --json     JSON format for structured parsing');
  WriteLn(StdErr, '  --field=X  Output only specific field (result1, result2, status)');
  Halt(2);
end;

function GetParamValue(const name: string): string;
var
  i: Integer;
  prefix: string;
begin
  prefix := '--' + name + '=';
  Result := '';
  for i := 1 to ParamCount do
  begin
    if Pos(prefix, ParamStr(i)) = 1 then
    begin
      Result := Copy(ParamStr(i), Length(prefix) + 1, MaxInt);
      Exit;
    end;
  end;
end;

function HasFlag(const name: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to ParamCount do
    if ParamStr(i) = '--' + name then
      Exit(True);
end;

procedure ParseArguments;
var
  s: string;
begin
  if HasFlag('help') or (ParamCount = 0) then
    ShowUsage;

  jsonOutput := HasFlag('json');
  verboseOutput := HasFlag('verbose');
  fieldOutput := GetParamValue('field');

  s := GetParamValue('param1');
  if s = '' then ShowUsage;
  param1 := StrToFloat(s);

  s := GetParamValue('param2');
  if s = '' then ShowUsage;
  param2 := StrToFloat(s);
end;

procedure Calculate;
begin
  // Implement calculation logic here
  result1 := param1 * param2;
  result2 := param1 / param2;
  status := 'OK';
end;

procedure OutputResults;
begin
  // Field-specific output (highest priority)
  if fieldOutput <> '' then
  begin
    if (fieldOutput = 'result1') then
      WriteLn(result1:0:2)
    else if (fieldOutput = 'result2') then
      WriteLn(result2:0:2)
    else if (fieldOutput = 'status') then
      WriteLn(status)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: result1, result2, status');
      Halt(2);
    end;
    Exit;
  end;

  // JSON output
  if jsonOutput then
  begin
    WriteLn('{');
    WriteLn('  "result1": ', result1:0:2, ',');
    WriteLn('  "result2": ', result2:0:2, ',');
    WriteLn('  "status": "', status, '"');
    WriteLn('}');
    Exit;
  end;

  // Verbose output (human-readable)
  if verboseOutput then
  begin
    WriteLn('============================================');
    WriteLn('  TOOL NAME - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('Result 1: ', result1:8:2);
    WriteLn('Result 2: ', result2:8:2);
    WriteLn('Status:   ', status);
    WriteLn('============================================');
    Exit;
  end;

  // Default: Concise output for AI agents
  Write('result1=', result1:0:2);
  Write(' result2=', result2:0:2);
  if status <> 'OK' then
    Write(' [', status, ']');
  WriteLn;
end;

begin
  try
    ParseArguments;
    Calculate;
    OutputResults;

    // Exit code based on status
    if Pos('ERRO', status) > 0 then
      Halt(1)
    else
      Halt(0);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Error: ', E.Message);
      Halt(1);
    end;
  end;
end.
```

**Key Points:**
1. **Default output is CONCISE** (`result1=X result2=X`) - optimized for AI agents
2. **Three output modes**: default (concise), `--verbose`, `--json`, `--field=X`
3. **Field extraction** allows surgical precision (just one value)
4. **Status/verification** messages only shown if not "OK"
5. **Exit codes** reflect success/error state

---

## Workflow for Claude Code

When asked to implement a new tool:

1. **Read the algorithm**: Open the corresponding chapter txt file from `reference_book/`
2. **Understand the Fortran**: Check `fortran explanation.txt` if syntax is unclear
3. **Understand the math**: Identify inputs, outputs, and intermediate calculations
4. **Create the Pascal file**: Use the template above, implement the calculation
   - **CRITICAL**: Implement all four output modes (default concise, --verbose, --json, --field=X)
   - Default output MUST be concise for AI agent efficiency
5. **Compile**: Run `cd src && fpc -O2 -Xs toolname.pas && mv toolname.exe ../bin/`
6. **Test all output modes**:
   - Test default concise output
   - Test `--verbose` for human readability
   - Test `--json` for structured parsing
   - Test `--field=X` for field extraction
   - Test with edge cases (minimum reinforcement, insufficient section, etc.)
7. **Verify results**: Compare against expected engineering ranges
8. **Update SKILLS.md**: Add usage examples and engineering context for the new tool

### Suggested Implementation Order

1. ✅ `flexao` — Chapter 2, fundamental calculation, validates the workflow **COMPLETE**
   - Use [src/flexao.pas](src/flexao.pas) as reference for output patterns
2. `cisalhamento` — Chapter 5, commonly needed alongside flexao
3. `flexao_t` — Chapter 3, extends flexao to T-beams
4. `flexao_verif` — Chapter 4, inverse of flexao (given As, find capacity)
5. `flexo_compressao` — Chapter 6, column design
6. `torcao` — Chapter 8, specialized but straightforward
7. `flexo_tracao` — Chapter 9, tension members

**Pattern to follow:** Look at [src/flexao.pas](src/flexao.pas) for the complete implementation pattern, especially the `OutputResults` procedure which handles all output modes correctly.

---

## Testing

Since we don't have the worked examples from the book, testing will be done through:

1. **Sanity checks**: Run with typical values and verify output is reasonable
   - Reinforcement ratios typically 0.15% to 4%
   - Steel areas should be positive
   - Domain should be 2, 3, or 4 for valid designs
   
2. **Boundary conditions**: Test edge cases
   - Very small moments (should give minimum reinforcement)
   - Large moments (should indicate insufficient section)
   
3. **Manual verification**: Compare against hand calculations or existing software

Example test (Windows command prompt):
```batch
bin\flexao --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --md=120
```

Typical beam (20x50cm, fck=25MPa, Md=120kN.m) should yield:
- As somewhere between 5-10 cm² (reasonable for this moment)
- Domain 2 or 3
- Reinforcement ratio around 0.5-1%

---

## Design Philosophy for AI Agents

### Why Concise Output Matters

These tools will be called **repeatedly** during engineering design sessions:
- Comparing alternative section sizes
- Iterating on reinforcement layouts
- Parametric studies of concrete grades
- Load case combinations

**Token efficiency examples:**
```bash
# Comparing 5 beam sizes with verbose output: ~2000 tokens
# Same comparison with concise output: ~200 tokens
# 10x reduction = 10x more calculations in same context window
```

### Output Mode Selection Guide

**Default (Concise)**: AI agent discussions, quick iterations
```bash
./bin/flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30
As=2.98 As'=0.00 dominio=2
```

**--field=X**: Extracting single value for calculations
```bash
AS=$(./bin/flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --field=As)
echo "Number of bars: $(echo "$AS / 1.23" | bc)"
```

**--json**: Programmatic parsing of multiple fields
```bash
./bin/flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --json | jq '.As'
```

**--verbose**: Final reports, human review, documentation
```bash
./bin/flexao.exe --bw=15 --h=40 --d=36 --fck=20 --fyk=500 --mk=30 --verbose > report.txt
```

### Consistency Across Tools

Every tool must follow the same pattern:
- Same argument parsing (GetParamValue, HasFlag)
- Same output flags (--verbose, --json, --field=X)
- Same error handling (stderr for errors, stdout for results)
- Same exit codes (0=success, 1=calc error, 2=invalid args)

This allows AI agents to build mental models: "All tools work the same way."

---

## Notes

- All dimensions in cm unless otherwise specified
- All forces in kN
- All moments in kN.m
- All stresses in MPa
- Use Double precision for all floating-point calculations
- The reference book algorithms are the authoritative source
- See [SKILLS.md](SKILLS.md) for detailed AI agent usage patterns
