-- Mandatory Exercise 2 IN3040 
import Prelude hiding(lookup)


data Program = Program Grid Robot 
data Grid = Size Int Int deriving Show
data Robot = Robot Start [Statement] FacingDirection AssocList
data Binding = Binding Identifier Expression deriving Show

data Statement =
    Stop
    | Turn Direction 
    | Step Expression
    | Assignment Assignment Identifier
    | Loop [Statement] While
    deriving (Show)

data Start = Start Expression Expression deriving Show 
data Direction = CW | CCW deriving Show
data FacingDirection = NORTH | SOUTH | WEST | EAST
data Assignment = INC | DECR deriving Show
data While = While Expression deriving Show

data Expression =
    IdentifierExpression Identifier
    | NumberExpression Int
    | ArithmeticExpression BinaryOperation Expression Expression
    | BooleanExpression BinaryOperation Expression Expression
    deriving Show

type Identifier = String     
data BinaryOperation = ADD | SUB | MULT | LESS | GREATER | EQUAL deriving Show


type AssocList = [Binding]

lookup :: Identifier -> AssocList -> Int
lookup _ [] = 0
lookup key ((Binding keyx valx):xs) = 
    if key == keyx
        then eval valx ((Binding keyx valx):xs)
        else lookup key xs

add :: Identifier -> Int  -> AssocList -> AssocList
add string int [] = [Binding string (NumberExpression int)]
add string int (x:xs) = add string int (xs)

change :: Identifier -> Int -> AssocList -> AssocList
change idToChange valToSet list = map (\(Binding id val) -> 
    if id == idToChange
        then Binding id (NumberExpression valToSet)
        else Binding id val) list


eval :: Expression -> AssocList -> Int

eval (IdentifierExpression str) bindings = lookup str bindings

eval (NumberExpression number) bindings = number

eval (ArithmeticExpression op l r) bindings = 
    let
        left = eval l bindings
        right = eval r bindings

    in case op of 
            ADD -> left + right
            SUB -> left - right
            MULT -> left * right
            LESS -> if (left < right) then 1 else 0
            GREATER -> if (left > right) then 1 else 0
            EQUAL -> if (left == right) then 1 else 0

eval (BooleanExpression op l r) bindings = 
    let 
        left = eval l bindings
        right = eval r bindings

    in case op of 
            ADD -> left + right
            SUB -> left - right
            MULT -> left * right
            LESS -> if (left < right) then 1 else 0
            GREATER -> if (left > right) then 1 else 0
            EQUAL -> if (left == right) then 1 else 0


interpretStatement :: Robot -> Program -> Robot

interpretStatement (Robot start (Stop:xs) dir list) program = (Robot start [] dir list)

interpretStatement (Robot start ((Turn newDir):xs) dir list) program = 
    interpretStatement (case newDir of 
        CW -> case dir of 
            NORTH -> (Robot start xs EAST list) 
            SOUTH -> (Robot start xs WEST list) 
            WEST -> (Robot start xs NORTH list) 
            EAST -> (Robot start xs SOUTH list) 
        CCW -> case dir of 
            NORTH -> (Robot start xs WEST list) 
            SOUTH -> (Robot start xs EAST list) 
            WEST -> (Robot start xs SOUTH list) 
            EAST -> (Robot start xs NORTH list)) program

interpretStatement (Robot (Start startx starty) ((Step numSteps):xs) dir list) (Program (Size sizeX sizeY) r) = 
    interpretStatement (
        case dir of
            NORTH -> 
                let newY = eval (ArithmeticExpression ADD starty numSteps) list
                in 
                    if (newY < 0 || newY > sizeY)
                        then errorOutOfBounds (Program (Size sizeX sizeY) (Robot (Start startx (NumberExpression newY)) xs dir list))
                        else (Robot (Start startx (NumberExpression newY)) xs dir list) 
            SOUTH -> 
                let newY = eval (ArithmeticExpression SUB starty numSteps) list
                in 
                    if (newY < 0 || newY > sizeY)
                        then errorOutOfBounds (Program (Size sizeX sizeY) (Robot (Start startx (NumberExpression newY)) xs dir list))
                        else (Robot (Start startx (NumberExpression newY)) xs dir list)
            WEST -> 
                let newX = eval (ArithmeticExpression SUB startx numSteps) list
                in 
                    if (newX < 0 || newX > sizeX)
                        then errorOutOfBounds (Program (Size sizeX sizeY) (Robot (Start (NumberExpression newX) starty) xs dir list))
                        else (Robot (Start (NumberExpression newX) starty) xs dir list)
            EAST -> 
                let newX = eval (ArithmeticExpression ADD startx numSteps) list
                in 
                    if (newX < 0 || newX > sizeX)
                        then errorOutOfBounds (Program (Size sizeX sizeY) (Robot (Start (NumberExpression newX) starty) xs dir list))
                        else (Robot (Start (NumberExpression newX) starty) xs dir list)) (Program (Size sizeX sizeY) r)
    

interpretStatement (Robot start ((Assignment assign key):xs) dir list) program =
    let 
        val = lookup key list
    in case assign of
        INC -> 
            let newList = change key (val + 1) list
            in interpretStatement (Robot start xs dir newList) program
        DECR -> 
            let newList = change key (val - 1) list 
            in interpretStatement (Robot start xs dir newList) program

interpretStatement (Robot start ((Loop statements (While condition)):xs) dir list) program = 
    if (eval condition list == 1) 
        then 
            let 
                (Robot newStart _ newDir newList) = 
                    interpretStatement (Robot start statements dir list) program
            in 
                interpretStatement (Robot newStart ((Loop statements (While condition)):xs) newDir newList) program
        else interpretStatement (Robot start xs dir list) program

interpretStatement (Robot start [] dir list) program = (Robot start [] dir list)


interpret :: Program -> (Int, Int)

interpret (Program grid (Robot start statements dir list)) = 
    let 
        (Robot (Start (NumberExpression x) (NumberExpression y)) _ _ _) = 
            interpretStatement (Robot start statements dir list) (Program grid (Robot start statements dir list))
    in (x, y)


errorOutOfBounds :: Program -> a
errorOutOfBounds (Program grid (Robot pos _ _ _)) = 
    error $
        "Robot at position " ++ (show pos) ++ " out of bounds in grid " ++ (show grid) 


main :: IO ()
main = do
    -- Testing Code 1
    putStrLn $ "-------------------------------------------------------------"
    tc1 <- return (testCode 1)
    putStrLn $ "Test code 1: " ++ show tc1
    
    -- Testing Code 2
    putStrLn $ "-------------------------------------------------------------"
    tc2 <- return (testCode 2)
    putStrLn $ "Test code 2: " ++ show tc2
    
    -- Testing Code 3
    putStrLn $ "-------------------------------------------------------------"
    tc3 <- return (testCode 3)
    putStrLn $ "Test code 3: " ++ show tc3

    -- Testing Code 4
    putStrLn $ "-------------------------------------------------------------"
    tc4 <- return (testCode 4)
    putStrLn $ "Test code 4: " ++ show tc4
    

testCode :: Int -> (Int, Int)

{- Test code 1. The result should be (13,52). -}
testCode 1 =
    let 
        grid1 = Size 64 64
        start1 = Start (NumberExpression 23) (NumberExpression 30)

        statements1 = [
            Turn CW,
            Turn CW,
            Step (NumberExpression 15),
            Turn CCW,
            Step (NumberExpression 15),
            Turn CCW,
            Step (ArithmeticExpression ADD (NumberExpression 2) (NumberExpression 3)),
            Turn CCW,
            Step (ArithmeticExpression ADD (NumberExpression 17) (NumberExpression 20)),
            Stop ]

        robot1 = Robot start1 statements1 EAST []
        program1 = Program grid1 robot1
    in
        interpret program1


{- Test code 2. The result should be (18,17). -}
testCode 2 = 
    let 
        grid2 = Size 64 64
        start2 = Start (NumberExpression 23) (NumberExpression 6)
        
        bindings2 = [
            Binding "i" (NumberExpression 5),
            Binding "j" (NumberExpression 3) ]
     
        statements2 = [
            Turn CCW,
            Step (ArithmeticExpression MULT (NumberExpression 3) (IdentifierExpression "i")),
            Turn CW,
            Step (NumberExpression 15),
            Turn CW, 
            Step (ArithmeticExpression SUB (ArithmeticExpression SUB (NumberExpression 12) (IdentifierExpression "i")) (IdentifierExpression "j")),
            Turn CW,
            Step (ArithmeticExpression ADD (ArithmeticExpression MULT (NumberExpression 2) (IdentifierExpression "i")) (ArithmeticExpression ADD (ArithmeticExpression MULT (NumberExpression 3) (IdentifierExpression "j")) (NumberExpression 1))),
            Stop ]

        robot2 = Robot start2 statements2 EAST bindings2
        program2 = Program grid2 robot2
    in
        interpret program2

{- Test code 3. The result should be (12,16). -}
testCode 3 =
    let 
        grid3 = Size 64 64 
        start3 = Start (NumberExpression 23) (NumberExpression 6)

        bindings3 = [
            Binding "i" (NumberExpression 5),
            Binding "j" (NumberExpression 3) ]

        statements3 = [
            Turn CCW,
            Step (ArithmeticExpression MULT (NumberExpression 3) (IdentifierExpression "i")),
            Turn CCW,
            Step (NumberExpression 15),
            Turn CW,
            Turn CW,
            Step (NumberExpression 4),
            Turn CW,
            Loop [
                Step (IdentifierExpression "j"), 
                Assignment DECR "j"] 
                (While (BooleanExpression GREATER (IdentifierExpression "j") (NumberExpression 1))) ]

        robot3 = Robot start3 statements3 EAST bindings3
        program3 = Program grid3 robot3
    in
        interpret program3

{- Test code 4. The result should be an error. -}
testCode 4 =
    let 
        grid4 = Size 64 64
        start4 = Start (NumberExpression 1) (NumberExpression 1)
        bindings4 = [Binding "i" (NumberExpression 8)]

        statements4 = [
            Loop [Step (IdentifierExpression "i")] (While (BooleanExpression LESS (IdentifierExpression "i") (NumberExpression 100))),
            Stop ]

        robot4 = Robot start4 statements4 EAST bindings4
        program4 = Program grid4 robot4
    in 
        interpret program4
