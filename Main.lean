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
  
  IO.println "\n--- Pie Chart ---\n"
  
  -- Test pie chart
  let pieData := [("Category A", 30.0), ("Category B", 25.0), ("Category C", 20.0), ("Category D", 15.0), ("Category E", 10.0)]
  let pieChart ← pie "Market Share" pieData defPlot
  IO.println pieChart
  
  IO.println "\n--- Box Plot ---\n"
  
  -- Test box plot
  let boxData := [
    ("Q1", [1.2, 1.5, 2.1, 2.8, 3.2, 3.5, 3.8, 4.0, 4.2]),
    ("Q2", [2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0]),
    ("Q3", [1.8, 2.2, 2.8, 3.2, 3.8, 4.2, 4.5, 4.8, 5.2])
  ]
  let boxChart ← boxPlot "Quarterly Performance" boxData defPlot
  IO.println boxChart
  
  IO.println "\n--- Heatmap ---\n"
  
  -- Test heatmap
  let heatmapData := [
    [1.0, 2.0, 3.0, 4.0, 5.0],
    [2.0, 4.0, 6.0, 8.0, 10.0],
    [3.0, 6.0, 9.0, 12.0, 15.0],
    [4.0, 8.0, 12.0, 16.0, 20.0]
  ]
  let heatmapChart ← heatmap "Data Intensity" heatmapData defPlot
  IO.println heatmapChart
  
  IO.println "\n--- Stacked Bars ---\n"
  
  -- Test stacked bars
  let stackedData := [
    ("Product A", [("Q1", 10.0), ("Q2", 12.0), ("Q3", 15.0), ("Q4", 18.0)]),
    ("Product B", [("Q1", 8.0), ("Q2", 10.0), ("Q3", 11.0), ("Q4", 14.0)]),
    ("Product C", [("Q1", 12.0), ("Q2", 14.0), ("Q3", 16.0), ("Q4", 20.0)])
  ]
  let stackedChart ← stackedBars "Quarterly Sales by Product" stackedData defPlot
  IO.println stackedChart
