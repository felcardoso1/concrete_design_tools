# Binary Size Optimization for Concrete Design Tools

This document details the exploration, testing, and conclusions regarding binary size optimization for the Pascal-based CLI tools in this project.

## Motivation

The concrete design tools are intended to be called repeatedly by AI agents, potentially thousands of times per session. Smaller binaries offer:

1. **L3 Cache Residency**: Binaries under ~256KB can remain hot in CPU L3 cache between invocations
2. **Faster Loading**: Less disk I/O and memory mapping overhead
3. **Lower Memory Footprint**: Important when multiple tools run concurrently
4. **Constraint**: Dynamic linking is not acceptable (tools must be fully static and self-contained)

---

## Exploration Summary

### Baseline Measurement

Original binaries compiled with `-O2 -Xs`:

| Binary | Size |
|--------|------|
| flexao | 536 KB |
| flexaot | 537 KB |
| cisalhamento | 531 KB |
| flexao_verif | 531 KB |
| flexo_compressao | 535 KB |
| flexo_tracao | 535 KB |
| torcao | 536 KB |
| **Total** | **~3.7 MB** |

### Optimization Flags Tested

| Flags | Result | Notes |
|-------|--------|-------|
| `-O2 -Xs` (baseline) | 536 KB | Current build configuration |
| `-Os -Xs` | 536 KB | Size optimization alone doesn't help |
| `-O2 -Xs -XX -CX` | **121 KB** | Smart linking - major improvement |
| `-Os -Xs -XX -CX` | 125 KB | Slightly larger than -O2 |
| `-O1 -Xs -XX -CX` | 121 KB | Same as -O2 |
| `-O4 -Xs -XX -CX` | 121 KB | Same as -O2 |
| External `strip --strip-all` | 121 KB | No additional benefit (-Xs already strips) |
| `eu-strip --remove-comment` | 121 KB | No additional benefit |
| `objcopy --remove-section` | 121 KB | No additional benefit |

### Winning Configuration

```bash
fpc -O2 -Xs -XX -CX -o"output" "source.pas"
```

| Flag | Purpose |
|------|---------|
| `-O2` | Optimize for speed (produces compact code) |
| `-Xs` | Strip debug symbols from executable |
| `-XX` | Enable smart linking (link only used code) |
| `-CX` | Create smart-linkable units (enables dead code elimination) |

### Results

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Per-binary size | 536 KB | 121 KB | **77% smaller** |
| Total (7 tools) | 3.7 MB | ~850 KB | **2.9 MB saved** |

---

## How Smart Linking Works

### Without Smart Linking

When you write `uses SysUtils, Math;`, the linker includes the **entire compiled unit** in your binary, even if you only use a few functions.

```
Your Code (27 KB) + Entire SysUtils (~250 KB) + Entire Math (~250 KB) = 536 KB
```

### With Smart Linking (-XX -CX)

The compiler analyzes which functions are actually called and includes only those:

```
Your Code (27 KB) + Used SysUtils functions (~47 KB) + Used Math functions (~47 KB) = 121 KB
```

### Technical Details

From FPC documentation:

> When you use smartlinking, the compiler creates a series of code blocks that are as small as possible, i.e. a code block will contain only the code for one procedure or function.

The compiler:
1. Creates separate object files for each procedure/function
2. Packages them into a static library (`libunit.a`)
3. The linker extracts only referenced symbols

---

## Unit Size Breakdown

To understand where size comes from, minimal test programs were compiled:

| Program | Size | Delta |
|---------|------|-------|
| Minimal (`WriteLn` only) | 31 KB | Baseline RTL |
| With `Math` unit | 94 KB | +63 KB |
| With `SysUtils` unit | 94 KB | +63 KB |
| With both + application code | 121 KB | +27 KB |

The ~90 KB overhead from Math/SysUtils is unavoidable given the functions used:

**From SysUtils:**
- `StrToFloat()` / `FloatToStr()` - string-to-number conversion
- `StringReplace()` - string manipulation
- `FormatSettings` - locale handling
- Exception infrastructure (`try`/`except`)
- AnsiString memory management (`{$H+}`)

**From Math:**
- `Power()` - exponentiation
- `Ln()` - natural logarithm

---

## Alternative Explored: Pure Pascal (No SysUtils)

### Concept

Replace standard library functions with hand-written equivalents:

| Current | Pure Pascal Alternative |
|---------|------------------------|
| `StrToFloat(s)` | Custom parser using `Val()` procedure |
| `FloatToStr(x)` | Custom formatter using `Str()` procedure |
| `Power(x, n)` | `Exp(n * Ln(x))` or iterative multiplication |
| `StringReplace()` | Manual character-by-character replacement |
| Exception handling | Return codes and boolean flags |
| AnsiStrings (`{$H+}`) | ShortStrings or null-terminated strings |

### Theoretical Size

With pure Pascal, binaries could potentially reach 50-60 KB.

### Why This Is Not Worth It

1. **Diminishing Returns**: 121 KB → 60 KB saves only 61 KB per binary. The 77% reduction from smart linking already achieves the goal of L3 cache residency.

2. **Code Quality Degradation**:
   - Loss of exception safety (silent failures instead of clear errors)
   - Manual string parsing is error-prone
   - `Val()` has awkward error handling semantics
   - ShortStrings limited to 255 characters

3. **Maintenance Burden**:
   - Custom implementations must be tested and maintained
   - Standard library functions are battle-tested
   - Future developers expect standard idioms

4. **Functional Risk**:
   - `StrToFloat` handles edge cases (scientific notation, locale)
   - Custom parsers often miss edge cases
   - Debugging becomes harder without exception traces

5. **Actual Performance**:
   - 121 KB fits comfortably in L3 cache (modern CPUs have 8-64 MB)
   - Difference between 60 KB and 121 KB is negligible for cache behavior
   - Runtime performance is identical

### Verdict

**Do not pursue pure Pascal rewrites.** Smart linking achieves the performance goal with zero code changes and zero risk.

---

## Risk Assessment for Smart Linking

### When Smart Linking Is Safe

Smart linking is safe when code:

- Uses direct procedure/function calls only
- Has no RTTI (Run-Time Type Information) usage
- Has no `published` class properties
- Does not store procedures in variables for dynamic dispatch
- Does not use `MethodAddress` or similar reflection
- Uses only well-tested standard units (System, SysUtils, Math, Classes)

**The concrete design tools meet all these criteria.**

### When Smart Linking Can Cause Problems

#### 1. RTTI and Dynamic Method Calls

```pascal
// RISK: Method might be stripped as "unused"
var methodPtr: Pointer;
methodPtr := obj.MethodAddress('Calculate');
```

The linker cannot trace string-based method lookups.

#### 2. Published Properties

```pascal
type
  TConfig = class(TPersistent)
  published
    property Timeout: Integer read FTimeout write FTimeout;  // May be stripped
  end;
```

Published properties exist for RTTI streaming. If accessed only via RTTI, they appear unused.

#### 3. Procedure Variables with Indirect Assignment

```pascal
type THandler = procedure(Sender: TObject);
var OnComplete: THandler;

procedure MyHandler(Sender: TObject);
begin
  // This entire procedure might be stripped
end;

initialization
  OnComplete := @MyHandler;  // Linker might not trace this
```

#### 4. Plugin/Extension Systems

```pascal
// Plugin expected to export this, but linker sees no internal calls
procedure PluginInit; export;
begin
  RegisterPlugin(Self);
end;
```

External callers are invisible to the linker.

#### 5. Initialization Sections with Side Effects

```pascal
unit MyRegistry;

var GlobalRegistry: TStringList;

initialization
  GlobalRegistry := TStringList.Create;
  GlobalRegistry.Add('default');  // Might be stripped if GlobalRegistry unused
```

If `GlobalRegistry` is never referenced, the entire init section may be eliminated.

### Mitigation Strategies

If you must use smart linking with risky patterns:

1. **Explicit References**: Add dummy references to ensure inclusion
   ```pascal
   procedure ForceLink;
   begin
     if False then MyHandler(nil);  // Forces MyHandler to be linked
   end;
   ```

2. **Disable Per-Unit**: Use `{$SMARTLINK OFF}` in specific units

3. **Test Thoroughly**: Run full test suite after enabling smart linking

---

## Guidance for Future Tools

### Recommended Build Configuration

```bash
fpc -O2 -Xs -XX -CX -o"$OUTPUT" "$SOURCE"
```

Always use smart linking for CLI tools unless you have a specific reason not to.

### Checklist Before Enabling Smart Linking

- [ ] No RTTI usage (`TypeInfo`, `GetTypeData`, `MethodAddress`)
- [ ] No `published` properties accessed via streaming
- [ ] No procedure variables with indirect assignment
- [ ] No plugin/export interfaces
- [ ] No reliance on initialization section side effects
- [ ] Full test suite passes after compilation

### When to Disable Smart Linking

Disable smart linking (`-XX-` or omit flags) if building:

- Shared libraries (`.so`, `.dll`) - mutually exclusive with smart linking
- Plugin hosts that load external code
- Applications using heavy RTTI (serialization frameworks, ORMs)
- Components designed for visual designers (published properties)

### Size Expectations

| Tool Complexity | Expected Size (with smart linking) |
|-----------------|-----------------------------------|
| Minimal (no units) | 30-40 KB |
| With Math only | 90-100 KB |
| With SysUtils only | 90-100 KB |
| With both + typical code | 110-130 KB |
| With Classes unit | 150-200 KB |
| With heavy RTL usage | 200-300 KB |

---

## Verification Procedure

After building with smart linking, verify each tool:

```bash
# Test normal operation
./tool --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5

# Test all output modes
./tool --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5 --json
./tool --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5 --verbose
./tool --bw=20 --h=50 --d=46 --fck=25 --fyk=500 --mk=58.5 --field=As

# Test error handling
./tool --help
./tool --invalid=param

# Test edge cases
./tool --bw=20 --h=50 --d=46 --fck=90 --fyk=500 --mk=500  # High values
./tool --bw=9 --h=20 --d=16 --fck=20 --fyk=500 --mk=10    # Low values
```

If all tests pass, smart linking is correctly configured.

---

## References

- [FPC Documentation: Smart Linking](https://www.freepascal.org/docs-html/current/prog/progse30.html)
- [FPC Command Line Reference](https://www.freepascal.org/docs-html/user/usersu17.html)

---

## Conclusion

Smart linking with `-XX -CX` reduces binary sizes by 77% (536 KB → 121 KB) with:

- Zero source code changes
- Zero functional differences
- Zero runtime performance impact
- Low risk for procedural CLI tools

This optimization should be applied to all tools in this project and any future additions following the same architectural pattern.
