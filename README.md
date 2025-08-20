# Limestone

A Lean 4 port of [Granite](https://github.com/mchav/granite), a terminal-based data visualization library with zero dependencies.

## Features

Limestone provides beautiful terminal-based charts using Unicode braille characters and ANSI colors:

- **Scatter plots** - Visualize relationships between variables
- **Histograms** - Display frequency distributions
- **Bar charts** - Compare categorical data
- **Stacked bars** - Show composition of categories
- **Line graphs** - Track trends over time
- **Pie charts** - Display proportions
- **Box plots** - Summarize distributions with quartiles
- **Heatmaps** - Visualize 2D data intensity

## Installation

Add Limestone to your Lean 4 project by including it in your `lakefile.toml`:

```toml
[[require]]
name = "limestone"
git = "https://github.com/yourusername/limestone"
rev = "main"
```

Then run:
```bash
lake update
lake build
```

## Usage

```lean
import Limestone
open Limestone.Granite

def main : IO Unit := do
  -- Create a simple scatter plot
  let data1 := series "Dataset 1" [
    (1.0, 2.0), (2.0, 4.0), (3.0, 3.0), (4.0, 5.0)
  ]
  let data2 := series "Dataset 2" [
    (1.0, 3.0), (2.0, 2.5), (3.0, 4.0), (4.0, 3.5)
  ]
  
  let plot ← scatterList "My Scatter Plot" [data1, data2] defPlot
  IO.println plot
```

## Type Safety

Limestone improves upon the original Granite by adding compile-time guarantees:

- **NonEmptyArray** - Prevents empty data errors
- **ValidPlot** - Ensures plot dimensions are within reasonable bounds
- **ValidBins** - Guarantees histogram bins are properly configured

## Examples

Run the example visualizations:

```bash
lake exe limestone
```

This will display various chart types demonstrating the library's capabilities.

## Configuration

Customize your plots with the `Plot` structure:

```lean
def myPlot : Plot :=
  { widthChars   := 80    -- Width in terminal characters
  , heightChars  := 24    -- Height in terminal characters  
  , leftMargin   := 8     -- Space for Y-axis labels
  , bottomMargin := 3     -- Space for X-axis labels
  , titleMargin  := 2     -- Space above the plot
  , legendPos    := .legendRight  -- or .legendBottom
  }
```

## Color Palette

Limestone supports 16 terminal colors including bright variants:
- Standard: black, red, green, yellow, blue, magenta, cyan, white
- Bright: brightBlack, brightRed, brightGreen, brightYellow, brightBlue, brightMagenta, brightCyan, brightWhite

## Patterns

For distinguishing data series, five fill patterns are available:
- solid, checker, diagA, diagB, sparse

## Requirements

- Lean 4.7.0 or later
- Terminal with Unicode and ANSI color support

## License

MIT License - See [LICENSE](LICENSE) file for details.

## Acknowledgments

This is a Lean 4 port of [Granite](https://github.com/mchav/granite) by Michael Chavinda. The original Haskell implementation provided the inspiration and algorithms for this library.

### Original Granite Library
- **Repository**: https://github.com/mchav/granite
- **Author**: Michael Chavinda (@mchav)
- **Language**: Haskell
- **License**: BSD-3-Clause

Limestone aims to bring the simplicity and elegance of Granite's terminal plotting to the Lean 4 ecosystem while adding compile-time safety guarantees through Lean's type system.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Status

This library is in active development. Core functionality is working, but some advanced features are still being implemented.