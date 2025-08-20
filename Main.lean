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
  
  IO.println "\n---\n"
  
  -- Simple test of canvas rendering
  let canvas := Canvas.new 10 5
  let canvas1 := Canvas.setDot canvas 5 5 (some Color.brightRed)
  let canvas2 := Canvas.setDot canvas1 10 10 (some Color.brightBlue)
  let canvas' := Canvas.setDot canvas2 15 15 (some Color.brightGreen)
  IO.println "Canvas test:"
  IO.println (Canvas.render canvas')
