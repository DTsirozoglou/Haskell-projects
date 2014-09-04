module SalsaParserTests where

import Test.HUnit
import qualified SalsaParser as A
import SalsaAst as S


-- Testing the definition "viewdef" parsing
case1 :: Either A.Error S.Program
case1 = A.parseString "viewdef One 500 500"
case1E :: Either A.Error S.Program
case1E= Right [S.Def (S.Viewdef "One" (S.Const 500) (S.Const 500))]

-- Testing the definition "group" parsing
case2 :: Either A.Error S.Program
case2 = A.parseString "group Both [One Two]"
case2E :: Either A.Error S.Program
case2E= Right [S.Def (S.Group "Both" ["One","Two"])]

-- Testing the definition "view" parsing
case3 :: Either A.Error S.Program
case3 = A.parseString "view Both"
case3E :: Either A.Error S.Program
case3E= Right [S.Def (S.View "Both")]

-- Testing the definition "rctangles and circles" parsing
case4 :: Either A.Error S.Program
case4 = A.parseString "rectangle larry 10 350 20 20 blue circle foo 10 20 20 blue"
case4E :: Either A.Error S.Program
case4E= Right [S.Def (S.Rectangle "larry" (S.Const 10) (S.Const 350) (S.Const 20) (S.Const 20) S.Blue),S.Def (S.Circle "foo" (S.Const 10) (S.Const 20) (S.Const 20) S.Blue)]

-- Testing the move Command parsing, and position parsing
case5 :: Either A.Error S.Program
case5 = A.parseString "larry fawn -> +(0, 0 - 300)"
case5E :: Either A.Error S.Program
case5E= Right [S.Com (S.Move ["larry","fawn"] (S.Rel (S.Const 0) (S.Minus (S.Const 0) (S.Const 300))))]

-- Testing multiple “|| (pronounced “par”)” Commands
case6 :: Either A.Error S.Program
case6 = A.parseString "larry -> (300, 350) || fawn -> (10,350) || box -> (110,400)"
case6E :: Either A.Error S.Program
case6E= Right [S.Com (S.Par (S.Par (S.Move ["larry"] (S.Abs (S.Const 300) (S.Const 350))) (S.Move ["fawn"] (S.Abs (S.Const 10) (S.Const 350)))) (S.Move ["box"] (S.Abs (S.Const 110) (S.Const 400))))]

--Testing multiple “@ (pronounced “at”)” Commands and parsing .x projections
case7 :: Either A.Error S.Program
case7 = A.parseString "larry -> (5 , foo.x) @ Bla @ Bla2 @ Bla3 @ Bla4"
case7E :: Either A.Error S.Program
case7E= Right [S.Com (S.At (S.At (S.At (S.At (S.Move ["larry"] (S.Abs (S.Const 5) (S.Xproj "foo"))) "Bla") "Bla2") "Bla3") "Bla4")]

--Testing multiple Commands parsing and commands inside brackets ’{’ Command ’}’
case8 :: Either A.Error S.Program
case8 = A.parseString "{larry -> (5 , 5)} @ Bla @ Bla2 || {larry -> (5 , 5)} @ Bla"
case8E :: Either A.Error S.Program
case8E= Right [S.Com (S.Par (S.At (S.At (S.Move ["larry"] (S.Abs (S.Const 5) (S.Const 5))) "Bla") "Bla2") (S.At (S.Move ["larry"] (S.Abs (S.Const 5) (S.Const 5))) "Bla"))]

-- Testing parsing a circle definition with bad number of arguments
case9 :: Either A.Error S.Program
case9 = A.parseString "circle fawn 300 15 plum"
case9E :: Either A.Error S.Program
case9E= Left "Wrong Input"

-- Testing Appendix's A Salsa program
case10 :: Either A.Error S.Program
case10 = A.parseString "viewdef Default 400 400 rectangle box 10 400 20 20 green box -> (10, 200) box -> +(100, 0) box -> (110,400) box -> +(0-100, 0)"
case10E :: Either A.Error S.Program
case10E= Right [S.Def (S.Viewdef "Default" (S.Const 400) (S.Const 400)), S.Def (S.Rectangle "box" (S.Const 10) (S.Const 400) (S.Const 20) (S.Const 20) S.Green) , S.Com (S.Move ["box"] (S.Abs (S.Const 10) (S.Const 200))) , S.Com (S.Move ["box"] (S.Rel (S.Const 100) (S.Const 0))), S.Com (S.Move ["box"] (S.Abs (S.Const 110) (S.Const 400))),S.Com (S.Move ["box"] (S.Rel (S.Minus (S.Const 0) (S.Const 100)) (S.Const 0)))]

-- Testing Appendix's B Salsa program
case11 :: Either A.Error S.Program
case11 = A.parseString "viewdef One 500 500 viewdef Two 400 400 group Both [One Two] view Both rectangle larry 10 350 20 20 blue rectangle fawn 300 350 15 25 plum view Two larry -> (300, 350) || fawn -> (10,350) view Both larry fawn -> +(0, 0 - 300)"
case11E :: Either A.Error S.Program
case11E= Right [S.Def (S.Viewdef "One" (S.Const 500) (S.Const 500)),S.Def (S.Viewdef "Two" (S.Const 400) (S.Const 400)),S.Def (S.Group "Both" ["One","Two"]),S.Def (S.View "Both"),S.Def (S.Rectangle "larry" (S.Const 10) (S.Const 350) (S.Const 20) (S.Const 20) S.Blue),S.Def (S.Rectangle "fawn" (S.Const 300) (S.Const 350) (S.Const 15) (S.Const 25) S.Plum),S.Def (S.View "Two"),S.Com (S.Par (S.Move ["larry"] (S.Abs (S.Const 300) (S.Const 350))) (S.Move ["fawn"] (S.Abs (S.Const 10) (S.Const 350)))),S.Def (S.View "Both"),S.Com (S.Move ["larry","fawn"] (S.Rel (S.Const 0) (S.Minus (S.Const 0) (S.Const 300))))]

test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11 :: Test
test1=TestCase (assertEqual "" case1 case1E)
test2=TestCase (assertEqual "" case2 case2E)
test3=TestCase (assertEqual "" case3 case3E)
test4=TestCase (assertEqual "" case4 case4E)
test5=TestCase (assertEqual "" case5 case5E)
test6=TestCase (assertEqual "" case6 case6E) 
test7=TestCase (assertEqual "" case7 case7E)
test8=TestCase (assertEqual "" case8 case8E)
test9=TestCase (assertEqual "" case9 case9E)
test10=TestCase (assertEqual "" case10 case10E)
test11=TestCase (assertEqual "" case11 case11E)

tests:: Test
tests = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11]

main::IO Counts
main = runTestTT tests