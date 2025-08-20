/-
  Limestone: A Lean 4 port of the Granite terminal plotting library
  Original: https://github.com/mchav/granite
  
  This version includes type-safe wrappers to prevent common runtime errors
  through compile-time guarantees.
-/

namespace Limestone.Granite

/-- Non-empty array type to prevent empty data errors -/
structure NonEmptyArray (α : Type) where
  data : Array α
  h : data.size > 0
  deriving Repr

namespace NonEmptyArray

def ofList {α : Type} (head : α) (tail : List α := []) : NonEmptyArray α :=
  { data := #[head] ++ tail.toArray, h := by 
      simp
      exact Nat.zero_lt_succ _ }

instance {α : Type} [Inhabited α] : Inhabited (NonEmptyArray α) :=
  ⟨ofList default []⟩

def toArray {α : Type} (nea : NonEmptyArray α) : Array α := nea.data
def toList {α : Type} (nea : NonEmptyArray α) : List α := nea.data.toList

def size {α : Type} (nea : NonEmptyArray α) : Nat := nea.data.size

def map {α β : Type} (f : α → β) (nea : NonEmptyArray α) : NonEmptyArray β :=
  { data := nea.data.map f, h := by simp [Array.size_map]; exact nea.h }

def foldl {α β : Type} (f : β → α → β) (init : β) (nea : NonEmptyArray α) : β :=
  nea.data.foldl f init

end NonEmptyArray

/-- Vector type with statically known length -/
abbrev Vector (α : Type) (n : Nat) := { a : Array α // a.size = n }

/-- Position of legend in plots -/
inductive LegendPos where
  | legendRight
  | legendBottom
  deriving Repr, BEq

/-- Plot configuration with validation -/
structure Plot where
  widthChars   : Nat
  heightChars  : Nat
  leftMargin   : Nat
  bottomMargin : Nat
  titleMargin  : Nat
  legendPos    : LegendPos
  deriving Repr

/-- Validated plot configuration -/
structure ValidPlot where
  widthChars   : { n : Nat // n > 0 ∧ n ≤ 200 }
  heightChars  : { n : Nat // n > 0 ∧ n ≤ 100 }
  leftMargin   : Nat
  bottomMargin : Nat
  titleMargin  : Nat
  legendPos    : LegendPos
  deriving Repr

def ValidPlot.toPlot (vp : ValidPlot) : Plot :=
  { widthChars := vp.widthChars.val
  , heightChars := vp.heightChars.val
  , leftMargin := vp.leftMargin
  , bottomMargin := vp.bottomMargin
  , titleMargin := vp.titleMargin
  , legendPos := vp.legendPos }

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
  deriving Repr, BEq, Inhabited

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

/-- Scatter plot using non-empty arrays -/
def scatter (title : String) 
            (sers : NonEmptyArray (String × NonEmptyArray (Float × Float)))
            (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  let plotC := Canvas.new wC hC
  let allPts := sers.data.flatMap (fun s => s.2.data)
  let (xmin, xmax, ymin, ymax) := boundsXY allPts.toList
  
  let sx (x : Float) : Nat :=
    clamp 0 (wC * 2 - 1) ((x - xmin) / (xmax - xmin + eps) * (wC * 2 - 1).toFloat).toUInt32.toNat
  let sy (y : Float) : Nat :=
    clamp 0 (hC * 4 - 1) ((ymax - y) / (ymax - ymin + eps) * (hC * 4 - 1).toFloat).toUInt32.toNat
  
  let pats := List.cycleN palette sers.data.size
  let cols := List.cycleN paletteColors sers.data.size  
  let withSty := List.zip3 sers.data.toList pats cols
  
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

/-- Scatter plot with List interface for convenience -/
def scatterList (title : String) (sers : List (String × List (Float × Float))) (cfg : Plot) : IO String := do
  match sers with
  | [] => pure "Error: Cannot plot empty data"
  | (name1, pts1) :: rest =>
    match pts1 with 
    | [] => pure "Error: Cannot plot empty series"
    | p1 :: ps1 =>
      let ne1 := NonEmptyArray.ofList p1 ps1
      let neRest := rest.filterMap fun (name, pts) => 
        match pts with
        | [] => none
        | p :: ps => some (name, NonEmptyArray.ofList p ps)
      let neSers : NonEmptyArray _ := 
        { data := #[(name1, ne1)] ++ neRest.toArray
        , h := by simp; exact Nat.zero_lt_succ _ }
      scatter title neSers cfg

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

/-- Validated bins with compile-time guarantees -/
structure ValidBins where
  nBins : { n : Nat // n > 0 }
  lo : Float
  hi : { h : Float // h > lo }
  deriving Repr

def ValidBins.toBins (vb : ValidBins) : Bins :=
  { nBins := vb.nBins.val, lo := vb.lo, hi := vb.hi.val }

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

/-- Histogram plot with valid bins and non-empty data -/
def histogram (title : String) (b : ValidBins)
              (xs : NonEmptyArray Float)
              (cfg : Plot) : IO String := do
  let b := b.toBins
  let step := (b.hi - b.lo) / b.nBins.toFloat
  let binIx (x : Float) : Nat := 
    clamp 0 (b.nBins - 1) (((x - b.lo) / step).floor.toUInt32.toNat)
  
  -- Count values in each bin
  let counts := xs.data.foldl (fun acc x =>
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
def bars (title : String) (kvs : NonEmptyArray (String × Float))
         (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  let vals := kvs.data.map (·.2)
  let vmax := vals.map (·.abs) |>.foldl max 1e-12
  
  let cats := kvs.data.zip (List.cycleN paletteColors kvs.data.size |>.toArray) |>.map fun ((name, v), col) =>
    (name, v.abs / vmax, col)
  
  let nCats := cats.size
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
    (cats.toList.map fun (name, _, col) => (name, Pattern.checker, col))
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
def lineGraph (title : String) (sers : NonEmptyArray (String × NonEmptyArray (Float × Float)))
              (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  let plotC := Canvas.new wC hC
  let allPts := sers.data.flatMap (fun s => s.2.data)
  let (xmin, xmax, ymin, ymax) := boundsXY allPts.toList
  
  let sx (x : Float) : Nat :=
    clamp 0 (wC * 2 - 1) ((x - xmin) / (xmax - xmin + eps) * (wC * 2 - 1).toFloat).toUInt32.toNat
  let sy (y : Float) : Nat :=
    clamp 0 (hC * 4 - 1) ((ymax - y) / (ymax - ymin + eps) * (hC * 4 - 1).toFloat).toUInt32.toNat
  
  let cols := List.cycleN paletteColors sers.data.size
  let withSty := List.zip sers.data.toList cols
  
  let cDone := withSty.foldl (fun c ((_, pts), col) =>
    let sortedPts := pts.data.qsort (fun a b => a.1 < b.1) |>.toList
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

/-- Angle normalization helper -/
def angleWithin (ang a0 a1 : Float) : Bool :=
  if a1 >= a0 then ang >= a0 && ang <= a1
  else ang >= a0 || ang <= a1

/-- Pie chart -/
def pie (title : String) (parts : NonEmptyArray (String × Float))
        (cfg : Plot) : IO String := do
  let total := parts.data.map (·.2.abs) |>.foldl (· + ·) 1e-12
  let normalized := parts.data.map fun (n, v) => (n, (v.abs / total))
  
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  let plotC := Canvas.new wC hC
  let wDots := wC * 2
  let hDots := hC * 4
  let r := min (wDots / 2 - 2) (hDots / 2 - 2)
  let cx := wDots / 2
  let cy := hDots / 2
  
  let pi := 3.141592653589793
  let toAng (p : Float) : Float := p * 2 * pi
  let wedges := (normalized.map (·.2) |>.map toAng).foldl (fun acc x => acc ++ [acc.getLast! + x]) [0]
  let angles := List.zip wedges wedges.tail!
  let names := normalized.map (·.1)
  let cols := List.cycleN pieColors names.size
  let withP := List.zip3 names.toList angles cols
  
  let cDone := withP.foldl (fun c (_, (a0, a1), col) =>
    c.fillDots 0 0 (wDots - 1) (hDots - 1) (fun x y =>
      let dx := (x : Int) - cx
      let dy := cy - (y : Int)
      let rr2 := dx * dx + dy * dy
      let r2 := r * r
      let ang := Float.atan2 (Float.ofInt dy) (Float.ofInt dx)
      let ang' := if ang < 0 then ang + 2 * pi else ang
      rr2 <= r2 && angleWithin ang' a0 a1
    ) (some col)
  ) plotC
  
  let ax := axisify cfg cDone 0 1 0 1
  let legend := legendBlock cfg.legendPos (cfg.leftMargin + cfg.widthChars)
    (withP.map fun (n, _, col) => (n, .solid, col))
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax legend

/-- Box plot helpers -/
def quartiles (xs : List Float) : (Float × Float × Float × Float × Float) :=
  let sorted := xs.toArray.qsort (· < ·) |>.toList
  let n := sorted.length
  if n < 5 then
    let m := xs.foldl (· + ·) 0 / n.toFloat
    (m, m, m, m, m)
  else
    let q1Idx := n / 4
    let q2Idx := n / 2
    let q3Idx := (3 * n) / 4
    let getIdx (i : Nat) := sorted[i]?.getD (sorted.getLast!)
    (sorted.head!, getIdx q1Idx, getIdx q2Idx, getIdx q3Idx, sorted.getLast!)

/-- Draw vertical line in grid -/
def drawVLine (grid : List (List (Char × Option Color))) (x y1 y2 : Nat) (ch : Char) (col : Option Color) : List (List (Char × Option Color)) :=
  let yStart := min y1 y2
  let yEnd := max y1 y2
  List.range grid.length |>.map fun y =>
    if y >= yStart && y <= yEnd then
      grid[y]!.set x (ch, col)
    else
      grid[y]!

/-- Draw horizontal line in grid -/
def drawHLine (grid : List (List (Char × Option Color))) (x1 x2 y : Nat) (ch : Char) (col : Option Color) : List (List (Char × Option Color)) :=
  if y >= grid.length then grid
  else
    let xStart := min x1 x2
    let xEnd := max x1 x2
    grid.set y <| List.range (grid[y]!.length) |>.map fun x =>
      if x >= xStart && x <= xEnd then (ch, col)
      else grid[y]![x]!

/-- Box plot -/
def boxPlot (title : String) (datasets : NonEmptyArray (String × NonEmptyArray Float))
            (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  
  let stats := datasets.data.map fun (name, vals) => (name, quartiles vals.data.toList)
  
  let allVals := datasets.data.flatMap (fun d => d.2.data)
  let ymin := if allVals.isEmpty then 0.0 else 
    (allVals.foldl min allVals[0]!) - (allVals.foldl max allVals[0]!).abs * 0.1
  let ymax := if allVals.isEmpty then 1.0 else
    (allVals.foldl max allVals[0]!) + (allVals.foldl max allVals[0]!).abs * 0.1
  
  let nBoxes := datasets.data.size
  let boxWidth := if nBoxes == 0 then 1 else max 1 (wC / (nBoxes * 2))
  let spacing := if nBoxes <= 1 then 0 else (wC - boxWidth * nBoxes) / (nBoxes - 1)
  
  let scaleY (v : Float) : Nat :=
    clamp 0 (hC - 1) (((ymax - v) / (ymax - ymin + eps) * (hC - 1).toFloat).toUInt32.toNat)
  
  let emptyGrid := List.replicate hC (List.replicate wC (' ', none))
  
  let finalGrid := (List.zip stats.toList (List.range stats.size)).foldl (fun grid ((_, (minV, q1, median, q3, maxV)), idx) =>
    let xStart := idx * (boxWidth + spacing)
    let xMid := xStart + boxWidth / 2
    let xEnd := xStart + boxWidth - 1
    
    let minRow := scaleY minV
    let q1Row := scaleY q1
    let medRow := scaleY median
    let q3Row := scaleY q3
    let maxRow := scaleY maxV
    
    let col := pieColors[idx % pieColors.length]?.getD Color.red
    
    let g1 := drawVLine grid xMid minRow q1Row '│' (some col)
    let g2 := drawVLine g1 xMid q3Row maxRow '│' (some col)
    let g3 := drawHLine g2 xStart xEnd q1Row '─' (some col)
    let g4 := drawHLine g3 xStart xEnd q3Row '─' (some col)
    let g5 := drawVLine g4 xStart q1Row q3Row '│' (some col)
    let g6 := drawVLine g5 xEnd q1Row q3Row '│' (some col)
    drawHLine g6 xStart xEnd medRow '═' (some col)
  ) emptyGrid
  
  let ax := axisifyGrid cfg finalGrid 0 nBoxes.toFloat ymin ymax
  let legend := legendBlock cfg.legendPos (cfg.leftMargin + cfg.widthChars)
    ((List.zip stats.toList (List.range stats.size)).map fun ((name, _), i) => 
      (name, .solid, pieColors[i % pieColors.length]?.getD Color.red))
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax legend

/-- Heatmap -/
def heatmap (title : String) (matrix : NonEmptyArray (NonEmptyArray Float))
            (cfg : Plot) : IO String := do
  let rows := matrix.data.size
  let cols := matrix.data[0]!.data.size
  
  let allVals := matrix.data.flatMap (·.data)
  let vmin := if allVals.isEmpty then 0 else allVals.foldl min allVals[0]!
  let vmax := if allVals.isEmpty then 1 else allVals.foldl max allVals[0]!
  let vrange := vmax - vmin + eps
  
  let intensityColors := [
    Color.blue, .cyan, .brightCyan, .green, .brightGreen,
    .yellow, .brightYellow, .red, .brightRed, .magenta, .brightMagenta
  ]
  
  let colorForValue (v : Float) : Color :=
    let norm := clamp 0 1 ((v - vmin) / vrange)
    let idx := clamp 0 (intensityColors.length - 1) 
      ((norm * (intensityColors.length - 1).toFloat).floor.toUInt32.toNat)
    intensityColors[idx]?.getD Color.blue
  
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  
  -- Resample matrix to fit display
  let resampleMatrix := List.range hC |>.map fun i =>
    List.range wC |>.map fun j =>
      let ri := i.toFloat * (rows - 1).toFloat / (hC - 1).toFloat
      let ci := j.toFloat * (cols - 1).toFloat / (wC - 1).toFloat
      let r0 := clamp 0 (rows - 1) ri.floor.toUInt32.toNat
      let c0 := clamp 0 (cols - 1) ci.floor.toUInt32.toNat
      -- Simple nearest neighbor for now
      matrix.data[r0]!.data[c0]!
  
  let grid := resampleMatrix.map fun row =>
    row.map fun val => ('█', some (colorForValue val))
  
  let ax := axisifyGrid cfg grid 0 cols.toFloat rows.toFloat 0
  
  let gradientLegend := "Min " ++ String.join (intensityColors.take 9 |>.map fun col =>
    paint col '█'
  ) ++ " Max"
  
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax gradientLegend

/-- Stacked bars -/
def stackedBars (title : String) (categories : NonEmptyArray (String × NonEmptyArray (String × Float)))
                (cfg : Plot) : IO String := do
  let wC := cfg.widthChars
  let hC := cfg.heightChars
  
  let seriesNames := categories.data[0]!.2.data.map (·.1)
  
  let totals := categories.data.map fun (_, series) =>
    series.data.map (·.2.abs) |>.foldl (· + ·) 0
  let maxHeight := totals.foldl max 1e-12
  
  let nCats := categories.data.size
  let (base, extra) := if nCats == 0 then (0, 0)
    else (wC / nCats, wC % nCats)
  let widths := List.range nCats |>.map fun i =>
    base + if i < extra then 1 else 0
  
  let cols := List.cycleN paletteColors seriesNames.size
  let seriesColors := List.zip seriesNames.toList cols
  
  let makeBar (cat : String × NonEmptyArray (String × Float)) (width : Nat) : List (List (Char × Option Color)) :=
    let series := cat.2.data.toList
    let values := series.map (·.2.abs / maxHeight)
    let cumHeights := values.foldl (fun acc x => acc ++ [(acc.getLast?.getD 0) + x]) [0]
    let segments := List.zip3 (series.map (·.1)) cumHeights cumHeights.tail!
    
    List.replicate width <| List.range hC |>.map fun y =>
      let heightFromBottom := (hC - y).toFloat / hC.toFloat
      segments.find? (fun (_, bottom, top) => 
        heightFromBottom > bottom && heightFromBottom <= top
      ) |>.map (fun (name, _, _) =>
        ('█', seriesColors.find? (·.1 == name) |>.map (·.2))
      ) |>.getD (' ', none)
  
  let gutterCol := List.replicate hC (' ', none)
  let allBars := List.zip categories.data.toList widths |>.map (fun (cat, w) => makeBar cat w)
  let columns := allBars.flatMap fun bar =>
    bar ++ [gutterCol]
  let columns' := if columns.isEmpty then columns else columns.dropLast
  
  let grid := List.range hC |>.map fun y =>
    columns'.map fun col => col[y]!
  
  let ax := axisifyGrid cfg grid 0 (max 1 nCats).toFloat 0 maxHeight
  let legend := legendBlock cfg.legendPos (cfg.leftMargin + cfg.widthChars)
    (seriesColors.map fun (name, col) => (name, .solid, col))
  let titled := if title.isEmpty then "" else title
  
  pure <| drawFrame cfg titled ax legend

end Limestone.Granite