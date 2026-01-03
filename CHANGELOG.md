# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-01-03

### Added

- **flexao**: Bending design for rectangular sections
- **flexaot**: T-beam bending design
- **flexao_verif**: Bending capacity verification
- **cisalhamento**: Shear design (stirrup calculation)
- **torcao**: Torsion design with combined bending and shear
- **flexo_tracao**: Tension member design
- **flexo_compressao**: Column design (compression + bending)

### Features

- Multiple output formats: concise (default), verbose, JSON, single-field extraction
- All tools follow NBR 6118 (Brazilian concrete design code)
- Optimized binary size (~121KB) using smart linking
- Cross-platform support (Linux, macOS, Windows)
- Comprehensive test suite with 100+ test cases

### Documentation

- README.md with installation and usage instructions
- CONTRIBUTING.md for developer guidelines
- CLAUDE.md for AI agent integration
- SKILLS.md with detailed tool documentation and engineering workflows
- AGENTS.md quick reference for AI assistants
