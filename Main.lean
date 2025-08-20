import Limestone

open Limestone.Granite

/-- Main entry point demonstrating various chart types from the Limestone library -/
def main : IO Unit := do
  -- Test scatter plot using Array interface
  let data1 := series "Dataset 1" #[
    (1.0, 2.0), (2.0, 4.0), (3.0, 3.0), (4.0, 5.0), (5.0, 4.5)
  ]
  let data2 := series "Dataset 2" #[
    (1.0, 3.0), (2.0, 2.5), (3.0, 4.0), (4.0, 3.5), (5.0, 5.0)
  ]
  
  let plot ← scatterArray "Sample Scatter Plot" #[data1, data2] defPlot
  IO.println plot
  
  IO.println "\n--- Histogram ---\n"
  
  -- Test histogram with Bins
  let histData := #[1.2, 1.5, 1.8, 2.1, 2.3, 2.5, 2.8, 3.0, 3.2, 3.5, 3.8, 4.0, 4.2, 4.5, 4.8, 5.0]
  let histBins := mkBins 5 1.0 5.5
  let histArray : NonEmptyArray Float := { data := histData, h := by decide }
  let hist ← histogram "Sample Histogram" histBins histArray defPlot
  IO.println hist
  
  IO.println "\n--- Bar Chart ---\n"
  
  -- Test bar chart
  let barData := NonEmptyArray.ofList ("Product A", 45.2) 
    [("Product B", 38.7), ("Product C", 52.1), ("Product D", 29.8)]
  let barChart ← bars "Sales by Product" barData defPlot
  IO.println barChart
  
  IO.println "\n--- Line Graph ---\n"
  
  -- Test line graph
  let lineData1pts := NonEmptyArray.ofList (1.0, 2.0) [(2.0, 3.5), (3.0, 3.2), (4.0, 4.8), (5.0, 4.5)]
  let lineData2pts := NonEmptyArray.ofList (1.0, 1.5) [(2.0, 2.8), (3.0, 3.9), (4.0, 3.5), (5.0, 5.2)]
  let lineData := NonEmptyArray.ofList ("Trend 1", lineData1pts) [("Trend 2", lineData2pts)]
  let lineChart ← lineGraph "Trends Over Time" lineData defPlot
  IO.println lineChart
  
  IO.println "\n--- Pie Chart ---\n"
  
  -- Test pie chart
  let pieData := NonEmptyArray.ofList ("Category A", 30.0) 
    [("Category B", 25.0), ("Category C", 20.0), ("Category D", 15.0), ("Category E", 10.0)]
  let pieChart ← pie "Market Share" pieData defPlot
  IO.println pieChart
  
  IO.println "\n--- Box Plot ---\n"
  
  -- Test box plot
  let boxData1 := NonEmptyArray.ofList 1.2 [1.5, 2.1, 2.8, 3.2, 3.5, 3.8, 4.0, 4.2]
  let boxData2 := NonEmptyArray.ofList 2.0 [2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0]
  let boxData3 := NonEmptyArray.ofList 1.8 [2.2, 2.8, 3.2, 3.8, 4.2, 4.5, 4.8, 5.2]
  let boxData := NonEmptyArray.ofList ("Q1", boxData1) [("Q2", boxData2), ("Q3", boxData3)]
  let boxChart ← boxPlot "Quarterly Performance" boxData defPlot
  IO.println boxChart
  
  IO.println "\n--- Heatmap ---\n"
  
  -- Test heatmap
  let row1 := NonEmptyArray.ofList 1.0 [2.0, 3.0, 4.0, 5.0]
  let row2 := NonEmptyArray.ofList 2.0 [4.0, 6.0, 8.0, 10.0]
  let row3 := NonEmptyArray.ofList 3.0 [6.0, 9.0, 12.0, 15.0]
  let row4 := NonEmptyArray.ofList 4.0 [8.0, 12.0, 16.0, 20.0]
  let heatmapData := NonEmptyArray.ofList row1 [row2, row3, row4]
  let heatmapChart ← heatmap "Data Intensity" heatmapData defPlot
  IO.println heatmapChart
  
  IO.println "\n--- Stacked Bars ---\n"
  
  -- Test stacked bars
  let prodA := NonEmptyArray.ofList ("Q1", 10.0) [("Q2", 12.0), ("Q3", 15.0), ("Q4", 18.0)]
  let prodB := NonEmptyArray.ofList ("Q1", 8.0) [("Q2", 10.0), ("Q3", 11.0), ("Q4", 14.0)]
  let prodC := NonEmptyArray.ofList ("Q1", 12.0) [("Q2", 14.0), ("Q3", 16.0), ("Q4", 20.0)]
  let stackedData := NonEmptyArray.ofList ("Product A", prodA) [("Product B", prodB), ("Product C", prodC)]
  let stackedChart ← stackedBars "Quarterly Sales by Product" stackedData defPlot
  IO.println stackedChart