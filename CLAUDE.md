https://github.com/mchav/granite

## Project Goal: Port Granite (Haskell terminal plotting library) to Lean 4

### Granite Overview
Granite is a Haskell library for creating terminal-based data visualizations with no dependencies beyond base.
It supports: scatter plots, histograms, bar charts, stacked bars, pie charts, box plots, line graphs, and heatmaps.

### Implementation Plan
1. Port all core data types and functions from Granite.hs to Lean 4
2. Maintain the same API and functionality 
3. Use only Lean 4's base/standard library (no external dependencies)
4. Implement Unicode braille character rendering for plots
5. Support ANSI color codes for terminal output
- you can use lean-lsp-mcp to search for necessary function signatures