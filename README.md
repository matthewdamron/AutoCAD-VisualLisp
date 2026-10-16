# AutoCAD Visual LISP

A collection of AutoLISP routines for AutoCAD drafting at Fetzer.

| File | What it holds |
|---|---|
| `Startup.lsp` | Startup: sets support paths and loads the .NET and LISP files |
| `Layer.lsp` | `SetLayer_*_Current` commands and layer on/off toggles |
| `Dimension.lsp` | Dimension text overrides (hold-to, verify in field, AFF, clear opening…) |
| `Symbol.lsp` | Part / assembly numbers, view titles, detail callouts, revisions |
| `HatchMaterial.lsp` | Material hatches (veneer, melamine, plastic laminate, backer) |
| `CNC.lsp` | Part DXF export for Mastercam and CNC program numbers |
| `Utility.lsp` | Drafting helpers: offsets, drawer box, frame, holes, draw order |
| `EndGrain.lsp`, `FSDetail.lsp` | End-grain and FS detail drawing |
| `EventHandler.lsp` | Shared error handler, undo grouping and sysvar save/restore |
| `LISP to FAS Converter.lsp` | Compiles the `.lsp` files to `.fas` |
| `russ.lsp` | Misc drafting shortcuts (`POS`, `AQ`, `SLOT`, `BKH`, `CASE`, arrays, chamfer…) |

Load with `APPLOAD`, or add `Startup.lsp` to the Startup Suite. Legacy code, kept for reference.
