module SalsaParser where

import Text.ParserCombinators.ReadP
import System.IO()
import SalsaAst

type Error      = String                -- We defined Error as synonym for String

----Helper Functions
token :: ReadP a -> ReadP a
token p = do 
                skipSpaces
                x <- p
                skipSpaces
                return x

symbol :: String -> ReadP String
symbol =  token . string

-- A function parseString for parsing a Salsa program.
-- If we have error on our Syntax it will give us the error "wrong input"
parseString :: String -> Either Error Program
parseString str 
        | null (readP_to_S parseProgram str) = Left  "Wrong Input"
        | otherwise =  Right $ fst(head (readP_to_S parseProgram str))

-- A function parseFile for parsing a Salsa program in a file:
parseFile :: FilePath -> IO (Either Error Program)
parseFile filename  = fmap parseString $ readFile filename

--The main function to parse our program
parseProgram :: ReadP Program
parseProgram = do
                defComs <- many1 parseDefCom
                eof
                return defComs          
                                
-- parseDefCom function is our main one                 
parseDefCom :: ReadP DefCom
parseDefCom =  do
                 definition <- token parseDefinition
                 return $ Def definition
               +++
               do
                 command <-  parseCommand
                 return $ Com command               

-- Parse a definition
parseDefinition ::ReadP Definition
parseDefinition = definition
  where definition = viewdef  +++ rectangle +++ circle +++ view +++ group
                where viewdef = do
                                      _ <- symbol "viewdef"
                                      vident <- token parseVIdent
                                      exp1 <- token parseExpr
                                      exp2 <- token parseExpr
                                      return $ Viewdef vident exp1 exp2
                      rectangle = do 
                                      _ <- symbol "rectangle"
                                      sident <- token parseSIdent
                                      exp1 <- token parseExpr
                                      exp2 <- token parseExpr
                                      exp3 <- token parseExpr
                                      exp4 <- token parseExpr
                                      colour <- token parseColour
                                      return $ Rectangle sident exp1 exp2 exp3 exp4 colour
                      circle = do
                                      _ <- symbol "circle"
                                      sident <- token parseSIdent
                                      exp1 <- token parseExpr
                                      exp2 <- token parseExpr
                                      exp3 <- token parseExpr
                                      colour <- token parseColour
                                      return $ Circle sident exp1 exp2 exp3 colour
                      view = do
                                      _ <- symbol "view"
                                      vident <- token parseVIdent
                                      return $ View vident
                      group = do
                                      _ <- symbol "group"
                                      vident <- token parseVIdent
                                      _ <- symbol "["
                                      vidents <- many1 (token parseVIdent)
                                      _ <- symbol "]"
                                      return $ Group vident vidents
                                        
-- Parse a Command
parseCommand ::ReadP Command
parseCommand = command
       where command = chainl1 comm1 (symbol "||" >> return Par) 
             comm1 =  atCommand +++ braCommand +++ moveCommand 
                        where atCommand = do
                                                comman <- braCommand +++ moveCommand 
                                                _ <- symbol "@"
                                                viden  <- token parseVIdent
                                                parseRight (At comman viden)                                                 
                              moveCommand = do
                                                sidents <- many1 (token parseSIdent) 
                                                _ <- symbol "->"
                                                position <- token parsePos
                                                return $ Move sidents position
                              braCommand = do
                                                _ <- symbol "{"
                                                braCom <-token parseCommand
                                                _ <- symbol "}"
                                                return braCom
-- Check multiple at Commands.                                                                                                               
parseRight :: Command -> ReadP Command
parseRight com = do
                        _ <- symbol "@"
                        viden  <- token parseVIdent
                        parseRight (At com viden)
                +++
                return com                                

-- Parse a Position
parsePos :: ReadP Pos
parsePos = do
                _ <- symbol "("
                expA1 <- token parseExpr
                _ <- symbol ","
                expA2 <- token parseExpr
                _ <- symbol ")"
                return  $ Abs expA1 expA2
           +++
           do
                _ <- symbol "+"
                _ <- symbol "("
                expR1 <- token parseExpr
                _ <- symbol ","
                expR2 <- token parseExpr
                _ <- symbol ")"
                return  $ Rel expR1 expR2       
 
-- Parse Colour
parseColour :: ReadP Colour
parseColour = do
                colour <- many1(satisfy(`elem` ['a'..'z']))
                if colour `elem`["blue"]
                        then return Blue
                        else if colour `elem`["plum"]
                                     then return Plum
                                     else if colour `elem`["red"]
                                     then return Red
                                     else if colour `elem`["green"]
                                     then return Green
                                     else if colour `elem`["orange"]
                                     then return Orange
                                     else pfail    

-- Parse an Expression
parseExpr :: ReadP Expr
parseExpr = expr
            where expr  = chainl1 expr1 (symbol "-" >> return Minus) 
                  expr1 = chainl1 expr2 (symbol "+" >> return Plus)
                  expr2 = sIdent  +++ token parseConst +++ braExpr
                        where sIdent    = do
                                                sid <- token parseSIdent
                                                _ <- symbol "."
                                                _ <- symbol "y"
                                                return $ Yproj sid
                                          +++
                                          do
                                                sid <- token parseSIdent
                                                _ <- symbol "."
                                                _ <- symbol "x"
                                                return $ Xproj sid                                                        
                              braExpr   = do 
                                                _ <- symbol "("
                                                braExp <-token parseExpr
                                                _ <- symbol ")"
                                                return braExp                                                                                                                                                             

---- Parse a Constant (integer)
parseConst :: ReadP Expr
parseConst = do
                digits <- munch1 (`elem` "0123456789")
                let var = read digits
                return $ Const var              
                                     
-- Parse SIdent 
parseSIdent :: ReadP Ident
parseSIdent = 
  do       
       y <- satisfy(`elem` ['a'..'z'])
       xs <- munch (`elem` ['a'..'z']++['A'..'Z']++['0'..'9']++"_")
       let sident = y:xs
       if sident `elem` reserverWord
           then pfail
           else return sident
    where reserverWord = ["viewdef", "rectangle","circle", "group", "view","blue"," Plum","Red","Green","Orange"]

---- Parse VIdent                          
parseVIdent :: ReadP Ident
parseVIdent = do
               y <- satisfy(`elem` ['A'..'Z'])
               xs <- munch (`elem` ['a'..'z']++['A'..'Z']++['0'..'9']++"_")
               return (y:xs)

--EXAMPLE of Parsing a .txt file with a salsa program
fileTest::IO (Either Error Program)            
fileTest = parseFile "test.txt"

tes = parseString "larry fawn -> (0,300)"
tes2 = parseString "larry -> (0,300)"