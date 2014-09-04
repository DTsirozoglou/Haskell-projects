module Gpx where

type ViewName = String
type ColourName = String
type Frame = [GpxInstr]
type Animation = ([(ViewName, Integer, Integer)], [Frame])
data GpxInstr = DrawRect Integer Integer Integer Integer ViewName ColourName
              | DrawCirc Integer Integer Integer ViewName ColourName
              deriving (Eq, Show)
test :: Animation
test = ([("Default",400,400)],[[(DrawCirc 10 100 5 "Default" "blue")],[(DrawCirc 10 200 5 "Default" "blue")]])


--viewdef Default 400 400
--circle round 10 100 5 blue
--round -> (10,200)