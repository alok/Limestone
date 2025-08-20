# Limestone

Terminal plotting for Lean 4. Port of [Granite](https://github.com/mchav/granite) (Haskell). Zero dependencies.

## What it does

Makes charts in your terminal using Unicode braille dots and colors:

- Scatter plots
- Histograms  
- Bar charts
- Stacked bars
- Line graphs
- Pie charts
- Box plots
- Heatmaps

## Example Outputs

### Scatter Plot
```
Temperature vs Time
26.3°│                                          ⢀
     │                                           
     │                                           
     │                 ⢀                         
     │                                           
23.0°│                                           
     │    ⡀                                      
     │                                           
     │                                           
19.7°│⢀                                          
     └────────────────────────────────────────────
      0                    2.0                   4.0
```

### Bar Chart
```
Sales by Quarter
61.3│                              ████████████
    │                              ████████████
    │     ███████     ██████       ████████████
    │     ███████     ██████       ████████████
30.7│█████████████████████████████████████████
    │█████████████████████████████████████████
    │█████████████████████████████████████████
 0.0│█████████████████████████████████████████
    └─────────────────────────────────────────
     Q1        Q2        Q3        Q4
```

### Histogram
```
Distribution
4.0│        ██ ██ ██ ██ ██ ██        
   │        ██ ██ ██ ██ ██ ██        
2.0│██ ██ ██ ██ ██ ██ ██ ██ ██ ██ ██
   │██ ██ ██ ██ ██ ██ ██ ██ ██ ██ ██
0.0│██ ██ ██ ██ ██ ██ ██ ██ ██ ██ ██
   └─────────────────────────────────
    0.87         2.85         4.83
```

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

## Quick start

```lean
import Limestone
open Limestone.Granite

-- One-liner
#eval plot "My Data" #[(1, 2), (2, 4), (3, 3)]

-- Or with multiple series
def main : IO Unit := do
  let data1 := series "A" #[(1, 2), (2, 4), (3, 3)]
  let data2 := series "B" #[(1, 3), (2, 2), (3, 5)]
  let chart ← scatterArray "Comparison" #[data1, data2] defPlot
  IO.println chart
```

## Try it

```bash
lake exe examples
```

## API

```lean
-- Simple functions for quick plots
plot : String → Array (Float × Float) → IO String
hist : String → Array Float → Nat → IO String
barChart : String → Array String → Array Float → IO String

-- Customize size
let cfg := plotConfig 80 24  -- width, height
scatter "Big Chart" data cfg
```

## Requirements

- Lean 4.7.0 or later
- Terminal with Unicode and ANSI color support

## License

MIT License - See [LICENSE](LICENSE) file for details.

## Credits

Port of [Granite](https://github.com/mchav/granite) by @mchav (BSD-3-Clause)