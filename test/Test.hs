import Diagrams.Prelude
import Diagrams.TwoD.Spiro

import Diagrams.Backend.Cairo.CmdLine

d = pad 1.1 . spiro False . map (\ p -> SpiroPoint p SpiroG2) $
      [ (1, 1), (0, 1), (0, 0), (1, 0)
      , (1,-1) , (0,-1)
      ]

main = defaultMain d