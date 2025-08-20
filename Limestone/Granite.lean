/-
  Limestone: A Lean 4 port of the Granite terminal plotting library
  Original: https://github.com/mchav/granite
-/

namespace Limestone.Granite

/-- Position of legend in plots -/
inductive LegendPos where
  | legendRight
  | legendBottom
  deriving Repr, BEq

/-- Plot configuration -/
structure Plot where
  widthChars   : Nat
  heightChars  : Nat
  leftMargin   : Nat
  bottomMargin : Nat
  titleMargin  : Nat
  legendPos    : LegendPos
  deriving Repr

/-- Default plot configuration -/
def defPlot : Plot :=
  { widthChars   := 60
  , heightChars  := 20
  , leftMargin   := 6
  , bottomMargin := 2
  , titleMargin  := 1
  , legendPos    := .legendRight
  }

/-- Terminal colors -/
inductive Color where
  | default | black | red | green | yellow | blue | magenta | cyan | white
  | brightBlack | brightRed | brightGreen | brightYellow | brightBlue
  | brightMagenta | brightCyan | brightWhite
  deriving Repr, BEq

/-- Convert color to ANSI code -/
def Color.ansiCode : Color → Nat
  | .black         => 30
  | .red           => 31
  | .green         => 32
  | .yellow        => 33
  | .blue          => 34
  | .magenta       => 35
  | .cyan          => 36
  | .white         => 37
  | .brightBlack   => 90
  | .brightRed     => 91
  | .brightGreen   => 92
  | .brightYellow  => 93
  | .brightBlue    => 94
  | .brightMagenta => 95
  | .brightCyan    => 96
  | .brightWhite   => 97
  | .default       => 39

/-- Get ANSI color on sequence -/
def Color.ansiOn (c : Color) : String :=
  s!"\x1b[{c.ansiCode}m"

/-- ANSI color off sequence -/
def ansiOff : String := "\x1b[0m"

/-- Paint a character with color -/
def paint (c : Color) (ch : Char) : String :=
  if ch == ' ' then " " else c.ansiOn ++ ch.toString ++ ansiOff

/-- Default color palette -/
def paletteColors : List Color :=
  [.brightBlue, .brightMagenta, .brightCyan, .brightGreen,
   .brightYellow, .brightRed, .brightWhite, .brightBlack]

/-- Pie chart colors -/
def pieColors : List Color :=
  [.brightRed, .brightGreen, .brightYellow, .brightBlue,
   .brightMagenta, .brightCyan, .brightWhite, .brightBlack]

/-- Fill patterns -/
inductive Pattern where
  | solid | checker | diagA | diagB | sparse
  deriving Repr, BEq

/-- Check if pattern has ink at position -/
def Pattern.ink : Pattern → Nat → Nat → Bool
  | .solid, _, _ => true
  | .checker, x, y => ((x + y) % 2) == 0
  | .diagA, x, y => (x + y) % 3 != 1
  | .diagB, x, y => ((x + 3 * y - y) % 3) != 1  -- Avoids negative numbers
  | .sparse, x, y => (x % 2 == 0) && (y % 3 == 0)

/-- Default pattern palette -/
def palette : List Pattern :=
  [.solid, .checker, .diagA, .diagB, .sparse]

/-- 2D array for canvas operations -/
structure Array2D (α : Type) where
  width  : Nat
  height : Nat
  data   : Array α
  deriving Repr

namespace Array2D

def get (a : Array2D α) (x y : Nat) : Option α :=
  if x < a.width && y < a.height then
    a.data[y * a.width + x]?
  else
    none

def set (a : Array2D α) (x y : Nat) (v : α) : Array2D α :=
  if x < a.width && y < a.height then
    let idx := y * a.width + x
    { a with data := a.data.set! idx v }
  else
    a

def new (w h : Nat) (v : α) : Array2D α :=
  { width := w, height := h, data := Array.replicate (w * h) v }

end Array2D

/-- Convert relative position to braille bit -/
def toBrailleBit (ry rx : Nat) : Nat :=
  match (ry, rx) with
  | (0, 0) => 1
  | (1, 0) => 2
  | (2, 0) => 4
  | (3, 0) => 64
  | (0, 1) => 8
  | (1, 1) => 16
  | (2, 1) => 32
  | (3, 1) => 128
  | _ => 0

/-- Canvas for drawing operations -/
structure Canvas where
  width  : Nat
  height : Nat
  buffer : Array2D Nat
  cbuf   : Array2D (Option Color)
  deriving Repr

namespace Canvas

def new (w h : Nat) : Canvas :=
  { width := w
  , height := h
  , buffer := Array2D.new w h 0
  , cbuf := Array2D.new w h none
  }

def setDot (c : Canvas) (xDot yDot : Nat) (mcol : Option Color) : Canvas :=
  if xDot >= c.width * 2 || yDot >= c.height * 4 then
    c
  else
    let cx := xDot / 2
    let cy := yDot / 4
    let rx := xDot - 2 * cx
    let ry := yDot - 4 * cy
    let b := toBrailleBit ry rx
    match c.buffer.get cx cy with
    | some m =>
        let c' := { c with buffer := c.buffer.set cx cy (m ||| b) }
        match mcol with
        | none => c'
        | some col => { c' with cbuf := c.cbuf.set cx cy (some col) }
    | none => c

def fillDots (x0 y0 x1 y1 : Nat) (p : Nat → Nat → Bool) (mcol : Option Color) (c : Canvas) : Canvas :=
  let xs := List.range (min (x1 + 1) (c.width * 2)) |>.filter (· >= x0)
  let ys := List.range (min (y1 + 1) (c.height * 4)) |>.filter (· >= y0)
  ys.foldl (fun c y =>
    xs.foldl (fun c' x =>
      if p x y then c'.setDot x y mcol else c'
    ) c
  ) c

def render (c : Canvas) : String :=
  let glyph : Nat → Char
    | 0 => ' '
    | m => Char.ofNat (0x2800 + m)
  
  let rows := List.range c.height |>.map fun y =>
    List.range c.width |>.map fun x =>
      match c.buffer.get x y, c.cbuf.get x y with
      | some m, some (some col) => paint col (glyph m)
      | some m, _ => (glyph m).toString
      | _, _ => " "
  
  String.intercalate "\n" (rows.map (String.join ·))

end Canvas

/-- Right-justify a string -/
def justifyRight (n : Nat) (s : String) : String :=
  let width := s.length  -- Simplified for now
  let pad := if n > width then n - width else 0
  String.mk (List.replicate pad ' ') ++ s

/-- Format a floating point number -/
def fmt (v : Float) : String :=
  let absV := v.abs
  if absV >= 1000 || (absV < 0.01 && v != 0) then
    s!"{v}"  -- Scientific notation not directly available
  else
    s!"{v}"

/-- Calculate bounds for XY data -/
def boundsXY (pts : List (Float × Float)) : (Float × Float × Float × Float) :=
  let xs := pts.map (·.1)
  let ys := pts.map (·.2)
  let xmin := xs.foldl min (xs.head?.getD 0)
  let xmax := xs.foldl max (xs.head?.getD 1)
  let ymin := ys.foldl min (ys.head?.getD 0)
  let ymax := ys.foldl max (ys.head?.getD 1)
  let padx := (xmax - xmin) * 0.05 + 1e-9
  let pady := (ymax - ymin) * 0.05 + 1e-9
  (xmin - padx, xmax + padx, ymin - pady, ymax + pady)

/-- Clamp value between bounds -/
def clamp {α : Type} [LE α] [Min α] [Max α] (low high x : α) : α :=
  max low (min high x)

/-- Small epsilon value -/
def eps : Float := 1e-12

/-- Draw a frame around the plot -/
def drawFrame (_cfg : Plot) (titleStr contentWithAxes legendBlockStr : String) : String :=
  let parts := [titleStr, contentWithAxes, legendBlockStr].filter (·.length > 0)
  String.intercalate "\n" parts

/-- Place labels at positions -/
def placeLabels (base : String) (offset : Nat) (labels : List (Nat × String)) : String :=
  labels.foldl (fun acc (x, s) =>
    let i := offset + x
    if i < acc.length then
      acc.take i ++ s ++ acc.drop (i + s.length)
    else
      acc
  ) base

/-- Add axes to canvas -/
def axisify (cfg : Plot) (c : Canvas) (xmin xmax ymin ymax : Float) : String :=
  let plotW := c.width
  let plotH := c.height
  let left := cfg.leftMargin
  let pad := String.mk (List.replicate left ' ')
  
  let yTicks := [(0, ymax), (plotH / 2, (ymin + ymax) / 2), (plotH - 1, ymin)]
  let baseLbl := List.replicate plotH pad
  
  let yLabels := yTicks.foldl (fun acc (row, v) =>
    if row < acc.length then
      acc.set row (justifyRight left (fmt v))
    else
      acc
  ) baseLbl
  
  let canvasLines := (c.render).splitOn "\n"
  let attachY := List.zip yLabels canvasLines |>.map fun (lbl, line) =>
    lbl ++ "│" ++ line
  
  let xBar := pad ++ "│" ++ String.mk (List.replicate plotW '─')
  let xLbls := [(0, xmin), (plotW / 2, (xmin + xmax) / 2), (plotW - 1, xmax)]
  let xLine := placeLabels (String.mk (List.replicate (left + 1 + plotW) ' ')) (left + 1)
    (xLbls.map fun (x, v) => (x, fmt v))
  
  String.intercalate "\n" (attachY ++ [xBar, xLine])

/-- Create legend block -/
def legendBlock (pos : LegendPos) (_width : Nat) (entries : List (String × Pattern × Color)) : String :=
  match pos with
  | .legendBottom =>
      let cells := entries.map fun (name, pat, col) =>
        samplePattern pat col ++ " " ++ name
      let line := String.intercalate "   " cells
      line
  | .legendRight =>
      String.intercalate "\n" <| entries.map fun (name, pat, col) =>
        samplePattern pat col ++ " " ++ name
where
  samplePattern (p : Pattern) (col : Color) : String :=
    let c := List.range 4 |>.foldl (fun cv dy =>
      List.range 2 |>.foldl (fun cv' dx =>
        if p.ink dx dy then cv'.setDot (dx % 2) (dy % 4) (some col) else cv'
      ) cv
    ) (Canvas.new 1 1)
    c.render.dropRightWhile (· == '\n')

/-- Helper to cycle a list to get n elements -/
def List.cycleN (l : List α) (n : Nat) : List α :=
  if l.isEmpty || n == 0 then [] 
  else if n <= l.length then l.take n
  else 
    let cycles := n / l.length
    let remainder := n % l.length
    let repeated := List.replicate cycles l |>.foldl (· ++ ·) []
    repeated ++ l.take remainder

/-- Helper to zip three lists -/
def List.zip3 (l1 : List α) (l2 : List β) (l3 : List γ) : List (α × β × γ) :=
  match l1, l2, l3 with
  | a :: as, b :: bs, c :: cs => (a, b, c) :: List.zip3 as bs cs
  | _, _, _ => []

/-- Data series helper -/
def series (name : String) (pts : List (Float × Float)) : (String × List (Float × Float)) :=
  (name, pts)

/-- Scatter plot -/
def scatter (title : String) (sers : List (String × List (Float × Float))) (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  let plotC := Canvas.new wC hC
  let allPts := sers.flatMap (·.2)
  let (xmin, xmax, ymin, ymax) := boundsXY allPts
  
  let sx (x : Float) : Nat :=
    clamp 0 (wC * 2 - 1) ((x - xmin) / (xmax - xmin + eps) * (wC * 2 - 1).toFloat).toUInt32.toNat
  let sy (y : Float) : Nat :=
    clamp 0 (hC * 4 - 1) ((ymax - y) / (ymax - ymin + eps) * (hC * 4 - 1).toFloat).toUInt32.toNat
  
  let pats := List.cycleN palette sers.length
  let cols := List.cycleN paletteColors sers.length
  let withSty := List.zip3 sers pats cols
  
  let cDone := withSty.foldl (fun c ((_name, pts), pat, col) =>
    pts.foldl (fun c' (x, y) =>
      let xd := sx x
      let yd := sy y
      if pat.ink xd yd then c'.setDot xd yd (some col) else c'
    ) c
  ) plotC
  
  let ax := axisify cfg cDone xmin xmax ymin ymax
  let legend := legendBlock cfg.legendPos (cfg.leftMargin + cfg.widthChars)
    (withSty.map fun ((n, _), p, col) => (n, p, col))
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax legend

/-- Block character for bar charts -/
def blockChar (n : Nat) : Char :=
  match clamp 0 8 n with
  | 0 => ' ' | 1 => '▁' | 2 => '▂' | 3 => '▃'
  | 4 => '▄' | 5 => '▅' | 6 => '▆' | 7 => '▇'
  | _ => '█'

/-- Generate column glyphs for bar height -/
def colGlyphs (hC : Nat) (frac : Float) : String :=
  let total := hC * 8
  let ticks := clamp 0 total ((frac * total.toFloat).toUInt32.toNat)
  let full := ticks / 8
  let rem8 := ticks - full * 8
  let topPad := hC - full - (if rem8 > 0 then 1 else 0)
  let middle := if rem8 > 0 then [blockChar rem8] else []
  String.mk (List.replicate topPad ' ' ++ middle ++ List.replicate full '█')

/-- Resample data to specific width -/
def resampleToWidth (w : Nat) (xs : List Float) : List Float :=
  if w == 0 then []
  else if xs.isEmpty then List.replicate w 0
  else
    let n := xs.length
    if n == w then xs
    else if n > w then
      -- Average groups when downsampling
      let groupSize := (n + w - 1) / w
      List.range w |>.map fun i =>
        let group := xs.drop (i * groupSize) |>.take groupSize
        if group.isEmpty then 0 else group.foldl (· + ·) 0 / group.length.toFloat
    else
      -- Replicate values when upsampling
      let base := w / n
      let extra := w - base * n
      (List.zip xs (List.range xs.length)).flatMap fun (v, i) =>
        List.replicate (base + if i < extra then 1 else 0) v

/-- Bins configuration for histogram -/
structure Bins where
  nBins : Nat
  lo : Float
  hi : Float
  deriving Repr

/-- Create bins configuration -/
def bins (n : Nat) (a b : Float) : Bins :=
  { nBins := max 1 n, lo := min a b, hi := max a b }

/-- Add axes to grid-based chart -/
def axisifyGrid (cfg : Plot) (grid : List (List (Char × Option Color))) (xmin xmax ymin ymax : Float) : String :=
  let plotH := grid.length
  let plotW := if grid.isEmpty then 0 else grid.head!.length
  let left := cfg.leftMargin
  let pad := String.mk (List.replicate left ' ')
  
  let yTicks := [(0, ymax), (plotH / 2, (ymin + ymax) / 2), (plotH - 1, ymin)]
  let baseLbl := List.replicate plotH pad
  
  let yLabels := yTicks.foldl (fun acc (row, v) =>
    if row < acc.length then
      acc.set row (justifyRight left (fmt v))
    else
      acc
  ) baseLbl
  
  let renderRow (cells : List (Char × Option Color)) : String :=
    String.join <| cells.map fun (ch, mc) =>
      match mc with
      | none => ch.toString
      | some c => paint c ch
  
  let attachY := List.zip yLabels grid |>.map fun (lbl, cells) =>
    lbl ++ "│" ++ renderRow cells
  
  let xBar := pad ++ "│" ++ String.mk (List.replicate plotW '─')
  let xLbls := [(0, xmin), (plotW / 2, (xmin + xmax) / 2), (plotW - 1, xmax)]
  let xLine := placeLabels (String.mk (List.replicate (left + 1 + plotW) ' ')) (left + 1)
    (xLbls.map fun (x, v) => (x, fmt v))
  
  String.intercalate "\n" (attachY ++ [xBar, xLine])

/-- Histogram plot -/
def histogram (title : String) (b : Bins) (xs : List Float) (cfg : Plot) : IO String := do
  let step := (b.hi - b.lo) / b.nBins.toFloat
  let binIx (x : Float) : Nat := 
    clamp 0 (b.nBins - 1) (((x - b.lo) / step).floor.toUInt32.toNat)
  
  -- Count values in each bin
  let counts := xs.foldl (fun acc x =>
    if x < b.lo || x > b.hi then acc
    else
      let idx := binIx x
      acc.set idx ((acc[idx]!) + 1)
  ) (List.replicate b.nBins 0)
  
  let maxC := (counts.foldl max 1).toFloat
  let fracs := counts.map (·.toFloat / maxC)
  
  let wData := cfg.widthChars
  let hC := cfg.heightChars
  let colsF := resampleToWidth wData fracs
  
  let dataCols := colsF.map fun f =>
    (colGlyphs hC f, some Color.brightCyan)
  let gutterCol := (String.mk (List.replicate hC ' '), none)
  let columns := (dataCols.flatMap fun col => [col, gutterCol]).dropLast
  
  let grid := List.range hC |>.map fun y =>
    columns.map fun (str, mc) =>
      (str.data[y]?.getD ' ', mc)
  
  let ax := axisifyGrid cfg grid b.lo b.hi 0 (counts.foldl max 1).toFloat
  let legend := legendBlock cfg.legendPos (cfg.leftMargin + cfg.widthChars)
    [("count", .solid, .brightCyan)]
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax legend

/-- Bar chart -/
def bars (title : String) (kvs : List (String × Float)) (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  let vals := kvs.map (·.2)
  let vmax := vals.map (·.abs) |>.foldl max 1e-12
  
  let cats := kvs.zip (List.cycleN paletteColors kvs.length) |>.map fun ((name, v), col) =>
    (name, v.abs / vmax, col)
  
  let nCats := cats.length
  let (base, extra) := if nCats == 0 then (0, 0) else (wC / nCats, wC % nCats)
  let widths := List.range nCats |>.map fun i =>
    base + if i < extra then 1 else 0
  
  let catGroups := List.zip cats widths |>.map fun ((_, f, col), w) =>
    List.replicate w (colGlyphs hC f, some col)
  
  let gutterCol := (String.mk (List.replicate hC ' '), none)
  let columns := (catGroups.flatMap fun group =>
    group ++ [gutterCol]).dropLast
  
  let grid := List.range hC |>.map fun y =>
    columns.map fun (glyphs, mc) =>
      (glyphs.data[y]?.getD ' ', mc)
  
  let ax := axisifyGrid cfg grid 0 (max 1 nCats).toFloat 0 vmax
  let legend := legendBlock cfg.legendPos (cfg.leftMargin + cfg.widthChars)
    (cats.map fun (name, _, col) => (name, .checker, col))
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax legend

/-- Line drawing helper -/
def lineDotsC (x0 y0 x1 y1 : Nat) (mcol : Option Color) (c : Canvas) : Canvas :=
  -- Bresenham's line algorithm
  let dx := if x1 > x0 then x1 - x0 else x0 - x1
  let sx := if x0 < x1 then 1 else -1
  let dy := if y1 > y0 then y1 - y0 else y0 - y1
  let sy := if y0 < y1 then 1 else -1
  let rec go (x y : Int) (err : Int) (c : Canvas) (fuel : Nat) : Canvas :=
    match fuel with
    | 0 => c
    | fuel' + 1 =>
      let c' := c.setDot x.natAbs y.natAbs mcol
      if x == x1 && y == y1 then c'
      else
        let e2 := 2 * err
        let (x', err') := if e2 > -dy then (x + sx, err - dy) else (x, err)
        let (y', err'') := if e2 < dx then (y + sy, err' + dx) else (y, err')
        go x' y' err'' c' fuel'
  termination_by fuel
  go x0 y0 (dx - dy) c (dx + dy + 1)

/-- Line graph -/
def lineGraph (title : String) (sers : List (String × List (Float × Float))) (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  let plotC := Canvas.new wC hC
  let allPts := sers.flatMap (·.2)
  let (xmin, xmax, ymin, ymax) := boundsXY allPts
  
  let sx (x : Float) : Nat :=
    clamp 0 (wC * 2 - 1) ((x - xmin) / (xmax - xmin + eps) * (wC * 2 - 1).toFloat).toUInt32.toNat
  let sy (y : Float) : Nat :=
    clamp 0 (hC * 4 - 1) ((ymax - y) / (ymax - ymin + eps) * (hC * 4 - 1).toFloat).toUInt32.toNat
  
  let cols := List.cycleN paletteColors sers.length
  let withSty := List.zip sers cols
  
  let cDone := withSty.foldl (fun c ((_, pts), col) =>
    let sortedPts := pts.toArray.qsort (fun a b => a.1 < b.1) |>.toList
    let pairs := List.zip sortedPts sortedPts.tail!
    pairs.foldl (fun c' ((x1, y1), (x2, y2)) =>
      lineDotsC (sx x1) (sy y1) (sx x2) (sy y2) (some col) c'
    ) c
  ) plotC
  
  let ax := axisify cfg cDone xmin xmax ymin ymax
  let legend := legendBlock cfg.legendPos (cfg.leftMargin + cfg.widthChars)
    (withSty.map fun ((n, _), col) => (n, .solid, col))
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax legend

end Limestone.Granite