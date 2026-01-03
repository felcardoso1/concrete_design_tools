# AI Agent Testing Framework

## Purpose

These CLI tools are designed for AI agents, not human engineers. This framework tests how well AI agents can discover, use, and work with the tools.

## Why Test with AI Agents?

- AI agents are the primary users of these tools
- We need feedback on CLI UX from the agent's perspective
- Comparing feedback vs actual terminal behavior reveals hidden friction

## How It Works

1. AI agent receives a task (structural design problem)
2. Agent uses CLI tools to solve the task
3. Agent completes structured interview about their experience
4. Human analyzes feedback vs terminal logs for discrepancies

---

## Running a Test

### Step 1: Choose a task

Look in `tasks/` folder for available tasks:
- `task-01-beam-column.md` - Beam and column design (bending, shear, flexo-compression)

### Step 2: Run the AI agent

Navigate to the `AGENT/` folder, which contains:
- **Tool binaries** - The compiled CLI tools (flexao, cisalhamento, etc.)
- **AGENTS.md** - System prompt that instructs the AI it's a structural engineer assistant
- **SKILLS.md** - Detailed documentation on how to use each tool

Give the AI agent:
- The task file content (copy/paste or reference the file)
- The system prompt from `AGENTS.md` (or let the AI read it)

Example for Claude Code:
```bash
cd /path/to/concrete_design_tools/AGENT
claude
# Paste the task content
```

Example for OpenCode or similar:
```bash
cd /path/to/concrete_design_tools/AGENT
opencode
# Paste the task content
```

The AI agent will use `AGENTS.md` as context and reference `SKILLS.md` for tool documentation.

### Step 3: Collect results

Create a session folder using the naming convention:
```bash
mkdir -p sessions/agent-name_YYYY-MM-DD
```

Save:
- `terminal-output.txt` - Copy the full terminal session
- `feedback.md` - Give the agent the interview template, save its response

### Step 4: Analyze (optional)

Compare written feedback against terminal logs:
- Did the agent report friction points it actually experienced?
- Did it minimize difficulties in written feedback despite terminal showing trial-and-error?
- Which feedback is based on actual experience vs documentation reading?

---

## Folder Structure

```
agent-tests/
├── README.md               # This file
├── tasks/                  # Task definitions for AI agents
│   └── task-01-beam-column.md
├── interview-template.md   # 30-question feedback interview
├── sessions/               # Test results (one folder per session)
│   └── agent-name_YYYY-MM-DD/
│       ├── terminal-output.txt
│       ├── feedback.md
│       └── notes.md        # (optional) Human analyst notes
└── archive/                # Old test sessions for reference
    └── 2025-12-30/
```

## Session Naming Convention

Format: `agent-name_YYYY-MM-DD`

Examples:
- `claude-code-opus_2026-01-03`
- `claude-code-sonnet_2026-01-03`
- `opencode-deepseek_2026-01-03`
- `opencode-grok_2026-01-03`

If running multiple sessions on the same day with the same agent, add a suffix:
- `claude-code-opus_2026-01-03-a`
- `claude-code-opus_2026-01-03-b`

---

## Adding New Tasks

Create a new file in `tasks/` with:
- Clear structural engineering problem
- Multiple elements requiring different tools
- Specific enough to verify correctness

Name format: `task-NN-short-description.md`

Example task structure:
```markdown
I'm designing a reinforced concrete structure. I need help with:

1. **Element Name**:
   - Loads and geometry
   - Trial section dimensions
   - Materials

2. **Another Element**:
   ...

Can you calculate the required reinforcement?
```

---

## The Interview Template

The `interview-template.md` contains 30 questions across 6 sections:

1. **Actual Experience** - Tool discovery, first use, errors encountered
2. **Friction Assessment** - Quantified ratings (1-5 scale)
3. **Comparison** - Tools vs internal LLM calculations
4. **Documentation** - What was read vs experienced
5. **Feature Requests** - Evidence-based only (experienced, not theoretical)
6. **Overall Assessment** - Summary and key improvement

The interview emphasizes honest feedback about friction over positive generalities.

---

## Key Insights from Previous Tests

From the December 2025 test sessions (see `archive/2025-12-30/`):

- **100% of agents** had path resolution issues (tried `flexao.exe` before discovering `./flexao`)
- **Field name confusion** - agents tried `--field=rho` when correct name is `--field=taxa`
- **Feedback discrepancies** - written feedback often minimized difficulties visible in terminal logs
- **Documentation influence** - some suggestions were based on reading docs rather than actual friction

These insights led to improvements in error messages and documentation.

---

## Contributing Test Results

If you run tests with a new AI agent, consider:
1. Creating a session folder with your results
2. Sharing findings that could improve the tools
3. Opening an issue if you discover universal friction points
