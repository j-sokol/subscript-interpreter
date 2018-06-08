import SubsInterpreter
import SubsAst
import System.IO()
import Text.Printf()
import Data.List(intercalate)

correctNumTest :: Bool
correctNumTest = (runExpr (Number 4) == Right (IntVal 4))

correctStringTest :: Bool
correctStringTest = (runExpr (String "Message") == Right (StringVal "Message"))

undefinedValTest :: Bool
undefinedValTest = (runExpr (Undefined) == Right (UndefinedVal))

correctEqualsTest :: Bool
correctEqualsTest = (runExpr (Call "===" [Number 4, Number 4]) == Right TrueVal)

correctLessTest :: Bool
correctLessTest = (runExpr(Call "<" [Number 4, Number 8]) == Right TrueVal)

correctPlusTest :: Bool
correctPlusTest = (runExpr (Call "+" [Number 3, Number 3]) == Right (IntVal (3+3)))

correctPlusTest2 :: Bool
correctPlusTest2 = (runExpr (Call "+" [String "A", String "B"]) == Right (StringVal ("A"++"B")))

correctPlusTest3 :: Bool
correctPlusTest3 = (runExpr (Call "+" [String "A", Number 3]) == Right (StringVal ("A"++(show 3))))

correctMultTest :: Bool
correctMultTest = (runExpr (Call "*" [Number 3, Number 3]) == Right (IntVal (3*3)))

correctSubTest :: Bool
correctSubTest = (runExpr (Call "-" [Number 3, Number 3]) == Right (IntVal (3-3)))

correctModuloTest :: Bool
correctModuloTest = (runExpr (Call "%" [Number 4, Number 8]) ==
                                                      Right (IntVal (mod 4 8)))

--testmy :: Bool
--testmy = (runExpr (Comma Assign "squares") == Right ("+","+"))

myTest :: IO()
myTest = do
  s <- readFile "intro-ast.txt"
  case runExpr (read s) of
    Left e -> error $ e
    Right res -> putStrLn $ "Result is: " ++ nice res ++ "\n"

nice :: Value -> String
nice (IntVal v) = show v
nice TrueVal = "true"
nice FalseVal = "false"
nice (StringVal s) = show s
nice UndefinedVal = "undefined"
nice (ArrayVal vs) = "["++ intercalate ", " (map nice vs) ++"]"


testAll :: IO ()
testAll = do print correctNumTest
             print correctStringTest
             print undefinedValTest
             print correctModuloTest
             print correctEqualsTest
             print correctLessTest
             print correctPlusTest
             print correctPlusTest2
             print correctPlusTest3
             print correctMultTest
             print correctSubTest
             myTest
