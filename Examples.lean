import Limestone
open Limestone.Granite

/-- Interactive examples showing the ergonomic API -/
def examples : IO Unit := do
  -- Simple plot from Y values
  IO.println "=== Quick Plot from Y Values ==="
  let chart1 ← plotY "Simple Growth" [1, 4, 2, 8, 5, 9, 7]
  IO.println chart1
  
  -- Quick scatter plot
  IO.println "\n=== Quick Scatter Plot ==="
  let chart2 ← plot "Temperature vs Time" [(0, 20), (1, 22), (2, 25), (3, 23), (4, 26)]
  IO.println chart2
  
  -- Multiple series with simple API
  IO.println "\n=== Multiple Series ==="
  let chart3 ← plots "Comparison" [
    data "Series A" [(1, 2), (2, 4), (3, 3)],
    data "Series B" [(1, 3), (2, 2), (3, 5)]
  ]
  IO.println chart3
  
  -- Quick histogram
  IO.println "\n=== Quick Histogram ==="
  let samples := [1.2, 1.5, 2.1, 2.3, 2.5, 2.8, 3.1, 3.2, 3.5, 3.9, 4.1, 4.5]
  let chart4 ← hist "Distribution" samples 5
  IO.println chart4
  
  -- Bar chart with simple API
  IO.println "\n=== Bar Chart ==="
  let chart5 ← barChart "Sales" ["Q1", "Q2", "Q3", "Q4"] [45.2, 52.1, 48.7, 61.3]
  IO.println chart5
  
  -- Pie chart
  IO.println "\n=== Pie Chart ==="
  let chart6 ← pieChart "Market Share" ["Product A", "Product B", "Product C"] [45, 30, 25]
  IO.println chart6
  
  -- Line graph
  IO.println "\n=== Line Graph ==="
  let chart7 ← line "Trend" (fromY [10, 15, 13, 17, 20, 18, 22])
  IO.println chart7
  
  -- Custom plot size
  IO.println "\n=== Custom Size Plot ==="
  let customCfg := plotConfig 40 15
  let chart8 ← scatterList "Compact" [("Data", [(1, 2), (2, 3), (3, 4)])] customCfg.toRaw
  IO.println chart8

/-- Main entry point for examples -/
def main : IO Unit := examples