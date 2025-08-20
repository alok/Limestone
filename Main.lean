import Limestone

open Limestone.Granite

def main : IO Unit := do
  -- Test scatter plot
  let data1 := series "Dataset 1" [
    (1.0, 2.0), (2.0, 4.0), (3.0, 3.0), (4.0, 5.0), (5.0, 4.5)
  ]
  let data2 := series "Dataset 2" [
    (1.0, 3.0), (2.0, 2.5), (3.0, 4.0), (4.0, 3.5), (5.0, 5.0)
  ]
  
  let plot ← scatter "Sample Scatter Plot" [data1, data2] defPlot
  IO.println plot
  
  IO.println "\n--- Histogram ---\n"
  
  -- Test histogram
  let histData := [1.2, 1.5, 1.8, 2.1, 2.3, 2.5, 2.8, 3.0, 3.2, 3.5, 3.8, 4.0, 4.2, 4.5, 4.8, 5.0]
  let hist ← histogram "Sample Histogram" (bins 5 1.0 5.5) histData defPlot
  IO.println hist
  
  IO.println "\n--- Bar Chart ---\n"
  
  -- Test bar chart
  let barData := [("Product A", 45.2), ("Product B", 38.7), ("Product C", 52.1), ("Product D", 29.8)]
  let barChart ← bars "Sales by Product" barData defPlot
  IO.println barChart
  
  IO.println "\n--- Line Graph ---\n"
  
  -- Test line graph
  let lineData1 := series "Trend 1" [(1.0, 2.0), (2.0, 3.5), (3.0, 3.2), (4.0, 4.8), (5.0, 4.5)]
  let lineData2 := series "Trend 2" [(1.0, 1.5), (2.0, 2.8), (3.0, 3.9), (4.0, 3.5), (5.0, 5.2)]
  let lineChart ← lineGraph "Trends Over Time" [lineData1, lineData2] defPlot
  IO.println lineChart
