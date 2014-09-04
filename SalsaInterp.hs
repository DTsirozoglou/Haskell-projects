--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import SalsaParser
import qualified Data.Map as M
import Data.Maybe as D
--------------------------------------------------------------------------------------
--------------------------- Define the function interpolate --------------------------
--------------------------------------------------------------------------------------                

type Position = (Integer, Integer)

interpolate :: Integer -> Position -> Position -> [Position]
interpolate n (startX,startY) (finishX,finishY)  
   | n == 0                  = []
   |startY == finishY        = if resMod == 0
                                then [(x,startY)| x <- [step,(step + resDiv) ..finishX]]
                                else [(x,startY)| x <- [(startX + resDiv +1),(startX + 2*resDiv +2)..(startX + resMod*resDiv +resMod)]
                                        ++ [(startX + (resMod+1)*resDiv+resMod),(startX + (resMod+2)*resDiv+resMod)..finishX]]
   |startX == finishX        = if resMody == 0
                                then [(startX,y)| y <- [stepy,(stepy + resDivy) ..finishY]]
                                else [(startX,y)| y <- [(startY + resDivy +1),(startY + 2*resDivy +2)..(startY + resMody*resDivy +resMody)]
                                        ++ [(startY + (resMody+1)*resDivy+resMody),(startY + (resMody+2)*resDivy+resMody)..finishY]]
   | otherwise               = []                                        
     where
           diff = abs finishX - startX
           resDiv = div diff n
           step = startX + resDiv
           resMod = mod diff n
           diffy = abs finishY - startY
           resDivy = div diffy n
           stepy = startY + resDivy
           resMody = mod diffy n                                                          



--------------------------------------------------------------------------------------
------------------------------- Define the type Context ------------------------------
--------------------------------------------------------------------------------------

data Context = Context { immutableState   :: ImmutableState
                     , mutableState ::MutableState
                     } deriving (Show)

type MutableState = [(Ident,(Integer, Integer),[Ident])]

data ImmutableState = ImmutableState { identEnv    :: IdentEnv 
                                     , binds       :: BindsEnv
                                     , shapesInfo  :: ShapesInfo
                                     , actViews    :: ActiveViews
                                     , framerate   :: Integer
                                     } deriving (Show)

type ShapesInfo = M.Map Ident (String,String,Integer,Integer,ActiveViews,Integer,Integer)
type BindsEnv = M.Map Ident [Ident]
type IdentEnv = M.Map Ident (Integer,Integer)
type ActiveViews = [Ident]

--------------------------------------------------------------------------------------
----------------------------- Define the type SalsaCommand ---------------------------
--------------------------------------------------------------------------------------

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a,MutableState)}
instance Monad SalsaCommand where
                return x = SalsaCommand $ \state -> (x, mutableState state)
                m1 >>= f = SalsaCommand $ \state1-> do
                                                    let (c, muTstate) = runSC m1 state1                                                    
                                                        (b, state3) = runSC (f c)  (state1 { mutableState = muTstate})
                                                    (b, state3)

--------------------------------------------------------------------------------------
------------------ functions for manipulating the immutable context ------------------
--------------------------------------------------------------------------------------

put :: Context -> SalsaCommand ()
put state = SalsaCommand $ const ((), mutableState state)

get :: SalsaCommand Context
get = SalsaCommand $ \state ->  (state,mutableState state)

getMcont :: SalsaCommand MutableState
getMcont = SalsaCommand $ \cont ->  (mutableState cont,mutableState cont)

getV :: SalsaCommand ActiveViews
getV = SalsaCommand $ \state ->  (actViews (immutableState state),mutableState state)

--------------------------------------------------------------------------------------
----------------- helper functions for manipulating tuples and lists -----------------
--------------------------------------------------------------------------------------

helpX :: (Ident,(Integer, Integer),[Ident])  -> Integer
helpX (_, (x,_),_) = x 

helpY :: (Ident,(Integer, Integer),[Ident])  -> Integer
helpY (_, (_,y),_) = y 

fstH :: (Ident,(Integer, Integer),[Ident]) -> Ident
fstH (ident, (_,_),_) = ident

getXcord :: MutableState -> [Integer]
getXcord  =  map helpX 

getYcord :: MutableState -> [Integer]
getYcord = map helpY

---- colourToString function ----

colourToString :: Colour -> String
colourToString col 
        |col == Blue    = "blue"
        |col == Plum    = "plum" 
        |col == Red     = "Red" 
        |col == Green   = "Green" 
        |col == Orange  = "Orange" 
        |otherwise      = fail "Wrong colour"     

--------------------------------------------------------------------------------------
----------------------- functions for manipulating the context -----------------------
--------------------------------------------------------------------------------------

getAct :: Salsa ActiveViews
getAct = Salsa $ \state ->  (actViews (immutableState state),state)                                     
 
--findaViews ::IdentEnv -> Ident -> Ident
--findaViews viewsEnv aViewID  = case lookupDef aViewID viewsEnv of
--                                    Nothing -> []
--                                    Just _ -> aViewID

insertShapeInfo :: Ident -> (String,String,Integer,Integer,ActiveViews,Integer,Integer) -> ShapesInfo -> ShapesInfo
insertShapeInfo = M.insert

lookupShapeInfo  :: Ident -> ShapesInfo -> Maybe (String,String,Integer,Integer,ActiveViews,Integer,Integer)
lookupShapeInfo  = M.lookup

askShapeInfo  :: Salsa ShapesInfo
askShapeInfo  = Salsa $ \shap -> (shapesInfo (immutableState shap), shap)

insertDef :: Ident -> (Integer,Integer) -> IdentEnv -> IdentEnv
insertDef = M.insert

--lookupDef  :: Ident -> IdentEnv -> Maybe (Integer,Integer)
--lookupDef  = M.lookup

ask :: Salsa IdentEnv
ask = Salsa $ \cont -> (identEnv (immutableState cont), cont)

putDef :: Context -> Salsa ()
putDef cont = Salsa $ const ((), cont)

getDef :: Salsa Context
getDef = Salsa $ \cont -> (cont,cont)

--- Manipulate Bindigs Enviroment

insertBind :: Ident -> [Ident] -> BindsEnv -> BindsEnv
insertBind = M.insert

lookupBind  :: Ident -> BindsEnv -> Maybe [Ident]
lookupBind  = M.lookup
--
lookupBindm :: Ident -> Salsa [Ident]
lookupBindm name = do env <- askBind
                      case lookupBind name env of
                           Just c  -> return c
                           Nothing -> return [name]

askBind  :: Salsa BindsEnv
askBind  = Salsa $ \cont -> (binds (immutableState cont), cont)

--------------------------------------------------------------------------------------
----------------------------- Define the function command ----------------------------
--------------------------------------------------------------------------------------

command :: Command -> SalsaCommand ()  

--------------------------------------------------------------------------------------
---------------------------------- Evaluate Commands ---------------------------------
--------------------------------------------------------------------------------------

command (Par _ _) = fail "Not implemented"
command (At (Par _ _ ) _) = fail "Not implemented"  
command (At (At com _) ident1) = command (At com ident1)                             
command (At (Move ids toPosition) ident) = 
       do
        cont <- get
        mcont <- getMcont
        (int1,int2) <- evalPos toPosition
        case toPosition of
                (Abs _ _) -> 
                        if length ids == 1
                                then put cont{mutableState = ( head ids,(int1,int2),[ident]):mcont} 
                                else do
                                        put cont{mutableState = ( head ids,(int1,int2),[ident]):mcont} 
                                        command (At (Move (tail ids) toPosition) ident)                                        
                (Rel _ _) -> 
                        if length ids == 1
                                then do
                                          let (_,(int3,int4),[_]) = head (filter ((== head ids).fstH) mcont) -- check the head
                                              pos1 = int3 + int1
                                              pos2 = int2 + int4
                                          put cont{mutableState = (head ids,(pos1,pos2),[ident]):mcont} 
                                else do
                                        let (_,(int3,int4),[_]) = head (filter ((== head ids).fstH) mcont) -- check the head
                                            pos1 = int3 + int1
                                            pos2 = int2 + int4
                                        put cont{mutableState = (head ids,(pos1,pos2),[ident]):mcont}
                                        command (At (Move (tail ids) toPosition) ident)
                                     
command (At _ _) = fail " Wrong 'At' command " 
command (Move ids toPosition) = 
       do
        cont <- get
        actVs <- getV
        mcont <- getMcont
        (int1,int2) <- evalPos toPosition
        case toPosition of
                (Abs _ _) -> 
                        if length ids == 1
                                then put cont{mutableState = (head ids,(int1,int2),actVs):mcont} 
                                else do
                                        put cont{mutableState = (head ids,(int1,int2),actVs):mcont}
                                        command (Move (tail ids) toPosition)                              
                (Rel _ _) -> 
                        if length ids == 1
                                then do
                                -- take the head in order to take the last inserted position of the shape
                                          let (_,(int3,int4),[_]) = head (filter ((== head ids).fstH) mcont) 
                                              pos1 = int3 + int1
                                              pos2 = int2 + int4
                                          -- this is our's shape new position so we add it to our mutable state of contex
                                          put cont{mutableState = (head ids,(pos1,pos2),actVs):mcont} 
                                else do
                                -- take the head in order to take the last inserted position of the shape
                                          let (_,(int3,int4),[_]) = head (filter ((== head ids).fstH) mcont) 
                                              pos1 = int3 + int1
                                              pos2 = int2 + int4
                                          -- this is our's shape new position so we add it to our mutable state of contex
                                          put cont{mutableState = (head ids,(pos1,pos2),actVs):mcont} 
                                          command (Move (tail ids) toPosition) 

--command (Move _ _) = fail " Wrong 'Move' command "

--------------------------------------------------------------------------------------
---------------------------------- Evaluate Positions --------------------------------
--------------------------------------------------------------------------------------

evalPos :: Pos -> SalsaCommand (Integer,Integer)
evalPos (Abs exprX exprY) =
                        do
                                int1 <- evalExpr exprX
                                int2 <- evalExpr exprY
                                return (int1,int2)
evalPos (Rel exprX exprY) =
                        do
                                int1 <- evalExpr exprX
                                int2 <- evalExpr exprY
                                return (int1,int2)                              
 
--------------------------------------------------------------------------------------
---------------------------------- Evaluate Expresions -------------------------------
--------------------------------------------------------------------------------------
                                
evalExpr :: Expr -> SalsaCommand Integer
evalExpr (Const int1) = return int1
evalExpr (Xproj ident1) =
                do
                 mstate <- getMcont
                 return $ minimum (getXcord  (filter ((== ident1).fstH) mstate)) 
evalExpr (Yproj ident1) =
                do
                 mstate <- getMcont
                 return $ minimum (getYcord  (filter ((== ident1).fstH) mstate)) 
evalExpr (Minus expr1 expr2) = 
                        do
                        int1 <- evalExpr expr1
                        int2 <- evalExpr expr2
                        return $ int1 - int2
evalExpr (Plus expr1 expr2) = 
                        do
                        int1 <- evalExpr expr1
                        int2 <- evalExpr expr2
                        return $ int1 + int2                                 

--------------------------------------------------------------------------------------
-------------------------------- Define the type Salsa -------------------------------
--------------------------------------------------------------------------------------

data Salsa a  = Salsa {runSA :: Context ->(a,Context)}
instance Monad Salsa where
                return x = Salsa $ \state0 ->(x, state0)
                m1 >>= f = Salsa $ \state1-> do
                                               let (c, state2) = runSA m1 state1
                                                   (b,state3) = runSA (f c)  state2
                                               (b,state3)

--------------------------------------------------------------------------------------
------------------------------ Define the function liftC -----------------------------
--------------------------------------------------------------------------------------

liftC :: SalsaCommand a -> Salsa a
liftC mon = do
                cont <- getDef
                let  (im,mu) = runSC mon cont  
                putDef cont {mutableState = mu} 
                return im

--------------------------------------------------------------------------------------
---------------------------- Define the function definition --------------------------
--------------------------------------------------------------------------------------

definition :: Definition -> Salsa ()
definition (Viewdef idView expr1 expr2)=
                        do
                                newEnv <- ask
                                contex <- getDef 
                                intX <- liftC (evalExpr expr1)
                                intY <- liftC (evalExpr expr2)                           
                                putDef contex { immutableState = (immutableState contex)
                                                                {identEnv = insertDef idView (intX,intY) newEnv
                                                                ,actViews = [idView]}}
--definition (Viewdef _ _ _)= fail " Wrong 'Viewdef' definition " 
definition (View ident) = 
                        do
                                activeIdents <- lookupBindm ident
                                contex <- getDef
                                putDef contex { immutableState = (immutableState contex)
                                                                {actViews = activeIdents}}
--definition (View _) = fail " Wrong 'View' definition "    
definition (Group id1 listIds) = 
                        do
                                contex <- getDef
                                bindeniR <- askBind
                                putDef contex { immutableState = (immutableState contex) 
                                                { binds = insertBind id1 listIds bindeniR }}                              
--definition (Group _ _) = fail " Wrong 'Group' definition "                                                            
definition (Circle ident expr1 expr2 expr3 colour) =
                       do
                        contex <- getDef
                        acViews <- getAct
                        info <- askShapeInfo
                        int1 <-liftC (evalExpr expr1)
                        int2 <-liftC (evalExpr expr2)
                        int3 <-liftC (evalExpr expr3)
                        putDef contex { immutableState = (immutableState contex)
                                        { shapesInfo = insertShapeInfo ident ("circle",colourToString colour,int3,int3,acViews,int1,int2) info}}
definition (Rectangle ident expr1 expr2 expr3 expr4 colour) =
                       do
                        contex <- getDef
                        acViews <- getAct
                        info <- askShapeInfo
                        int1 <-liftC (evalExpr expr1)
                        int2 <-liftC (evalExpr expr2)
                        int3 <-liftC (evalExpr expr3)
                        int4 <-liftC (evalExpr expr4)
                        putDef contex { immutableState = (immutableState contex)
                                       {shapesInfo = insertShapeInfo ident ("rect",colourToString colour,int3,int4,acViews,int1,int2) info}}
                                      
--------------------------------------------------------------------------------------
------------------------------ Define the function defCom ----------------------------
--------------------------------------------------------------------------------------                                                      

defCom :: DefCom -> Salsa ()
defCom(Def defin) = definition defin
defCom(Com comman) = liftC (command comman)

--------------------------------------------------------------------------------------
------------------------------ Define the function runProg ---------------------------
--------------------------------------------------------------------------------------    

mutStatetoFrame :: (Ident,Integer, Integer) -> ShapesInfo -> Ident -> GpxInstr
mutStatetoFrame (id1,pos1,pos2) sinfo actV 
                | shape1 == "rect" = DrawRect pos1 pos2 ifo1 ifo2 actV col  
                | otherwise        = DrawCirc pos1 pos2 ifo1 actV col
          where 
                infos = lookupShapeInfo id1 sinfo
                (shape1,col,ifo1,ifo2,_,_,_) = fromJust infos
                
mframeList :: ShapesInfo -> (Ident,(String,String,Integer, Integer,[Ident],Integer, Integer)) -> Frame
mframeList infos (iden,(_,_,_,_,av,p1,p2))  = map (mutStatetoFrame (iden,p1,p2) infos) av

frameList :: ShapesInfo -> (Ident,(Integer, Integer),[Ident]) -> Frame 
frameList infos (id2,(int1, int2),aviews)   = map (mutStatetoFrame (id2,int1,int2) infos) aviews

makeInterpolate :: Integer -> MutableState -> ShapesInfo -> MutableState
makeInterpolate _ [] _ = []
makeInterpolate n ((id1,(intx, inty),actVis):xs) infos = 
                                                    makeMutable (interpolate n (posx,posy) (intx,inty)) id1 actVis ++ (makeInterpolate n xs infos3)
                                            where 
                                                infos2 = lookupShapeInfo id1 infos
                                                (shape1,col,ifo1,ifo2,lko,posx,posy) = fromJust infos2
                                                infos3 = insertShapeInfo id1 (shape1,col,ifo1,ifo2,lko,intx,inty) infos
                                                

makeMutable :: [Position] -> Ident -> [Ident] -> MutableState
makeMutable [] _ _ = []
makeMutable ((x,y):xs) id1 actV = (id1,(x,y),actV):(makeMutable xs id1 actV)

listToanime :: (Ident, (Integer, Integer)) ->  (ViewName, Integer, Integer)
listToanime (id1,(pos1,pos2)) = (id1,pos1,pos2)

runProg ::Integer -> String -> Animation
runProg n s = let ((),con) = getContex s
                  infos = shapesInfo (immutableState con)
                  sFrameList = map (mframeList infos) (M.toList (shapesInfo (immutableState con)))
                  mFrameList = map (frameList infos) (makeInterpolate n (reverse (mutableState con)) infos)
                  anim = map listToanime (M.toList (identEnv (immutableState con)))
              in (anim, sFrameList ++ mFrameList)

getContex :: String -> ((),Context)
getContex  s = case parseString s of
                   Left _ -> ((),initState)
                   Right progr -> runSA (program progr)  initState 
                   
program :: Program -> Salsa() 
program prog =  if length prog ==1 
                    then defCom (head prog)
                    else do
                                defCom (head prog)
                                program (tail prog)          
                                                                          
--------------------------------------------------------------------------------------
----------------------- Define an initial state for the Context-----------------------
-------------------------------------------------------------------------------------- 
initState :: Context
initState  = Context { immutableState  = initmut
                     , mutableState    = []
                     }
                     
initmut :: ImmutableState   
initmut  =  ImmutableState   { identEnv    = M.empty 
                             , binds       = M.empty 
                             , shapesInfo  = M.empty
                             , actViews    = [] 
                             , framerate   = 50
                             }        

case5 = getContex "View One"                             
case1 = getContex "viewdef One 500 500"
case2 = getContex "group Both [One Two]"
case3 = getContex "larry -> (0, 0 + 300)"
case4 = getContex "{larry -> (5 , 5)} @ Bla"
case6 = getContex "larry fawn dds -> (0,300)"
case7 = getContex "larry fawn -> +(0, 0 - 300)"
case8 = getContex "viewdef Default 400 400 viewdef Two 400 400 rectangle box 10 400 20 20 green"
case9 = getContex "viewdef Default 400 400 rectangle box 10 400 20 20 green box -> (10, 200)"
case10 = runProg 2 "viewdef Default 400 400 rectangle box 10 400 20 20 green box -> (10, 200) box -> (10, 0) box -> (10,400)" -- box -> +(0-100, 0)"
case11 = getContex "viewdef Default 400 400 rectangle box 10 400 20 20 green box -> (10, 200) box -> (10, 0) box -> (10,400)"
test = interpolate 6 (5,300) (5,400)
test1 = interpolate 0 (0,5) (100,5)
test2 = interpolate 28 (0,325) (0,735)
case12 = getContex "viewdef One 500 500 viewdef Two 400 400 group Both [One Two] view Both rectangle larry 10 350 20 20 blue rectangle fawn 300 350 15 25 plum fawn -> (300,299) view Two fawn larry -> (300,20) {larry -> (300 , fawn.y)} @ One "
case13 = runProg 3 "viewdef One 500 500 viewdef Two 400 400 group Both [One Two] view Both rectangle larry 10 350 20 20 blue rectangle fawn 300 350 15 25 plum fawn -> (300,299) view Two fawn larry -> (300,20) {larry -> (300 , fawn.y)} @ One"
case14 = getContex "viewdef Two 900 900 circle larry 300 350 20 blue rectangle fawn 300 350 15 25 plum fawn -> (300, 200) fawn -> (300, 400) larry -> (300, 900) fawn larry -> +(0,0 - 200)"
case15 = runProg 1 "viewdef Two 900 900 circle larry 300 350 20 blue rectangle fawn 300 350 15 25 plum fawn -> (300, 200) fawn -> (300, 400) larry -> (300, 900) fawn larry -> +(0,0 - 200)"
salsa1 = runProg 4 "viewdef One 500 500 viewdef Two 400 400 group Both [One Two] view Both rectangle larry 10 350 20 20 blue rectangle fawn 300 350 15 25 plum view Two larry -> (300, 350) fawn -> (10,350) view Both larry fawn -> +(0, 0 - fawn.y)"