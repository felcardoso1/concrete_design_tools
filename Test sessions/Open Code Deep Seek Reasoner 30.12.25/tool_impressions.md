# AI Agent Impressions: Structural Engineering CLI Tools

## Overview
As an AI agent tasked with performing reinforced concrete design calculations, I recently used a suite of CLI tools (`flexao.exe`, `cisalhamento.exe`, `flexo_compressao.exe`, etc.) to solve a practical engineering problem. These tools are specifically designed for AI agents rather than human users, which presents unique advantages and considerations.

## Overall Experience
The tools performed reliably and produced accurate results consistent with NBR 6118 standards. The interface is straightforward: command-line execution with clear parameter flags. The ability to run calculations externally rather than relying on the LLM's internal knowledge base significantly improved accuracy and reduced hallucination risk.

## Comparison: External Tools vs. LLM Calculations

### Advantages of External Tools:
1. **Accuracy Guarantee**: Code-based calculations eliminate the risk of mathematical errors or approximation mistakes that can occur in LLM reasoning.
2. **Code Compliance**: Tools implement actual NBR 6118 algorithms, ensuring regulatory compliance that would be difficult to verify in LLM-generated calculations.
3. **Consistency**: Same inputs always produce same outputs, unlike LLM responses which can vary.
4. **Speed**: Calculations complete in milliseconds versus seconds of LLM processing.
5. **Precision**: Handles edge cases (domain 4 warnings, minimum reinforcement checks) that would be complex to implement correctly in natural language.

### Limitations of External Tools:
1. **Context Switching**: Requires exiting LLM context to execute shell commands, which can disrupt conversational flow.
2. **Error Handling**: Must parse exit codes and error messages rather than receiving natural language explanations.
3. **Learning Curve**: Need to memorize parameter names and conventions versus asking natural language questions.

## What Worked Well

### Tool Design:
- **Concise Output Formats**: The default `As=4.40 As'=0.00 dominio=2` format is perfect for AI parsing - minimal tokens, clear field delimiters.
- **Field Extraction**: `--field=As` option is invaluable for extracting single values without parsing overhead.
- **Multiple Output Modes**: `--verbose` for human-readable reports, `--json` for structured data, default for AI parsing.
- **Exit Codes**: Clear success/failure indicators (0=success, 1=calculation error, 2=invalid arguments).

### Documentation:
- **SKILLS.md**: Comprehensive reference with parameter lists, output field definitions, and engineering context (736 lines).
- **CLAUDE.md**: Quick-start guide with practical examples and workflow patterns.
- **Practical Examples**: Documentation includes realistic engineering scenarios (beam design, column design, optimization).

### Engineering Context:
- **Domain Explanations**: Clear definitions of domains 2/3/4 with engineering significance.
- **Typical Ranges**: Guidance on normal parameter values helps validate inputs.
- **Workflow Examples**: Complete design sequences (bending + shear) mirror real engineering practice.

## Areas for Improvement

### Tool Interface:
1. **Path Issues**: On Windows/Ubuntu WSL, need to use `./flexao.exe` instead of `flexao.exe`. Could include wrapper scripts or PATH configuration guidance.
2. **Parameter Validation**: Missing required parameters give generic errors; more specific "missing --bw parameter" messages would help debugging.
3. **Units Clarification**: Documentation states cm/kN/MPa but this could be more prominent in error messages.
4. **Batch Processing**: No native support for multiple load cases; requires shell scripting loops.

### Documentation:
1. **Verbosity**: SKILLS.md at 736 lines is comprehensive but overwhelming. Could be split: Quick Start (1-2 pages), Reference (parameter tables), Examples (workflows).
2. **Redundancy**: Some information repeats between CLAUDE.md and SKILLS.md.
3. **Searchability**: No index or table of contents for quick navigation.
4. **AI-Optimized Format**: Could include a "For AI Agents" section with parsing examples and common patterns.

### Output Consistency:
1. **Field Names**: Some tools use `As'` others `As_comp` in documentation; consistent naming would simplify parsing.
2. **Error Format**: Error messages vary in format; standardized `ERROR: description` format would help.
3. **JSON Output**: Could include units in JSON output (`{"As": 4.40, "units": "cm²"}`).

## Documentation Assessment

### CLAUDE.md:
- **Strengths**: Perfect quick reference, excellent workflow examples, clear unit reminders.
- **Weaknesses**: Could include troubleshooting section for common AI agent issues.
- **Rating**: 9/10 for AI agents - concise, actionable, well-organized.

### SKILLS.md:
- **Strengths**: Exhaustive reference, valuable engineering context, good examples.
- **Weaknesses**: Too long for quick consultation, some sections redundant.
- **Rating**: 7/10 - comprehensive but needs better organization for AI use.

## Suggested Improvements

### Tool Enhancements:
1. **Interactive Help**: `tool.exe --help` should show parameter list with defaults and units.
2. **Input Validation**: Early validation with clear error messages for invalid ranges.
3. **Batch Mode**: `--batch` flag accepting JSON array of inputs, returning JSON array of results.
4. **Unit Conversion**: Optional `--units=mm` flag for millimeter inputs.
5. **Configuration File**: Support `--config=file.json` for repeated parameter sets.

### Documentation Improvements:
1. **Three-Tier Documentation**:
   - **Quick Start** (1 page): Basic commands, common workflows
   - **Reference Card** (1 page): Parameter tables, output fields, exit codes
   - **Examples Repository** (separate file): Complete design scenarios
2. **AI Agent Guide**: Special section on parsing output, error handling, optimization patterns.
3. **Troubleshooting Guide**: Common error messages and solutions.
4. **Interactive Examples**: More copy-paste ready command sequences.

### Developer Experience:
1. **Installation Script**: Setup PATH and permissions automatically.
2. **Test Suite**: Example inputs with expected outputs for verification.
3. **Version Info**: `--version` flag to check tool versions.
4. **Logging**: Optional `--log=file.txt` for debugging complex calculations.

## Conclusion
These CLI tools represent a significant advancement for AI-assisted structural engineering. They provide accurate, reliable calculations while maintaining a simple interface suitable for AI agents. The external execution model correctly addresses the LLM's limitations in mathematical precision and code compliance.

The tools are **definitely better** than performing calculations inside the LLM for several reasons:
1. **Reliability**: Eliminates calculation errors
2. **Compliance**: Implements actual design codes
3. **Efficiency**: Faster than LLM reasoning
4. **Consistency**: Repeatable results

With minor improvements to documentation organization and error handling, these tools could become the standard for AI-assisted structural design across multiple engineering domains.

**Most Valuable Feature**: The `--field=X` extraction option - dramatically reduces token usage and parsing complexity.

**Biggest Opportunity**: Adding batch processing capability would enable parametric studies and optimization workflows that are currently cumbersome.

---
*Generated by AI agent after practical use of the tools for beam and column design.*
*Date: December 30, 2025*