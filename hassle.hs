import Data.Char
import Data.List (splitAt)
import Data.Map
import qualified Data.Map (empty, fromList, insert, lookup)
import Test.QuickCheck

type JumpMap = Map Int Int

-- adds pair (x, y) and (y, x) to the map
symmetricInsert :: Int -> Int -> JumpMap -> JumpMap
symmetricInsert x y map = Data.Map.insert y x (Data.Map.insert x y map)

-- construct a map which keeps track of the locations of all bracket pairs
buildJumpMap :: [Char] -> JumpMap -> [Int] -> Int -> JumpMap
buildJumpMap [] jumpMap _ _ = jumpMap
buildJumpMap (curr_ins : rest_ins) jumpMap stack pos = case curr_ins of
    '[' -> buildJumpMap rest_ins jumpMap (pos : stack) (pos + 1)
    ']' ->
        buildJumpMap
            rest_ins
            (symmetricInsert pos (head stack) jumpMap)
            (tail stack)
            (pos + 1)
    _ -> buildJumpMap rest_ins jumpMap stack (pos + 1)

-- returns a copy of lst with element at pos set to val
setVal :: Int -> Int -> [Int] -> [Int]
setVal pos val lst =
    let (before, after) = Data.List.splitAt pos lst
     in before ++ [val] ++ tail after

data ProgramState = BfState [Char] Int [Int] Int JumpMap [Char]
    deriving (Eq, Show)

doBfStep :: ProgramState -> ProgramState
doBfStep
    (BfState inss pctr cells cptr jmap buffer) =
        if (length inss) <= pctr
            then BfState inss pctr cells cptr jmap buffer
            else
                doBfStep
                    ( case inss !! pctr of
                        '>' ->
                            -- increment pointer
                            BfState
                                inss
                                (pctr + 1)
                                ( if cptr < (length cells) - 1
                                    then cells
                                    else cells ++ [0]
                                )
                                (cptr + 1)
                                jmap
                                buffer
                        '<' ->
                            -- decrement pointer
                            BfState
                                inss
                                (pctr + 1)
                                cells
                                ( if cptr > 0
                                    then cptr - 1
                                    else 0
                                )
                                jmap
                                buffer
                        '+' ->
                            -- increment value at pointer
                            BfState
                                inss
                                (pctr + 1)
                                ( setVal
                                    cptr
                                    ( if (cells !! cptr) < 255
                                        then (cells !! cptr) + 1
                                        else 0
                                    )
                                    cells
                                )
                                cptr
                                jmap
                                buffer
                        '-' ->
                            -- decrement value at pointer
                            BfState
                                inss
                                (pctr + 1)
                                ( setVal
                                    cptr
                                    ( if (cells !! cptr) > 0
                                        then (cells !! cptr) - 1
                                        else 255
                                    )
                                    cells
                                )
                                cptr
                                jmap
                                buffer
                        '[' ->
                            -- jump to matching bracket if value at pointer is 0
                            case ((Data.Map.lookup pctr jmap), (cells !! cptr)) of
                                (Just jloc, 0) -> BfState inss (jloc + 1) cells cptr jmap buffer
                                (_, _) -> BfState inss (pctr + 1) cells cptr jmap buffer
                        ']' ->
                            -- jump to matching bracket if value at pointer is non-zero
                            case ((Data.Map.lookup pctr jmap), (cells !! cptr)) of
                                (_, 0) -> BfState inss (pctr + 1) cells cptr jmap buffer
                                (Just jloc, _) -> BfState inss (jloc + 1) cells cptr jmap buffer
                                (_, _) -> BfState inss (pctr + 1) cells cptr jmap buffer -- a correct program should never reach this case
                        '.' ->
                            -- add char of value at pointer to output buffer
                            BfState
                                inss
                                (pctr + 1)
                                cells
                                cptr
                                jmap
                                (buffer ++ [chr (cells !! cptr)])
                        -- TODO: implement input
                        _ -> BfState inss (pctr + 1) cells cptr jmap buffer -- if we don't know the instructionm we just skip it
                    )

runbf :: [Char] -> [Char]
runbf inss =
    let (BfState _ _ _ _ _ buffer) =
            doBfStep
                ( BfState
                    inss
                    0
                    [0]
                    0
                    ( buildJumpMap
                        inss
                        Data.Map.empty
                        []
                        0
                    )
                    ""
                )
     in buffer

-------- TESTS --------
bufferEqual :: ProgramState -> [Char] -> Bool
bufferEqual (BfState _ _ _ _ _ buffer) str = buffer == str

prop_buildMapSimple :: Bool
prop_buildMapSimple =
    buildJumpMap ['[', '-', ']'] (Data.Map.empty) [] 0
        == fromList [(0, 2), (2, 0)]

prop_buildMapNested :: Bool
prop_buildMapNested =
    buildJumpMap ['[', '-', '[', '+', '+', '<', ']', ']'] (Data.Map.empty) [] 0
        == fromList [(0, 7), (7, 0), (2, 6), (6, 2)]

prop_emptyProgram :: Bool
prop_emptyProgram =
    doBfStep
        (BfState "" 0 [0] 0 (buildJumpMap "" (Data.Map.empty) [] 0) "")
        == BfState "" 0 [0] 0 (buildJumpMap "" (Data.Map.empty) [] 0) ""

prop_singleInstr :: Bool
prop_singleInstr =
    doBfStep
        (BfState "+" 0 [0] 0 (buildJumpMap "+" (Data.Map.empty) [] 0) "")
        == BfState "+" 1 [1] 0 (buildJumpMap "+" (Data.Map.empty) [] 0) ""

prop_helloWorld :: Bool
prop_helloWorld =
    let jmap =
            buildJumpMap
                "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
                Data.Map.empty
                []
                0
     in bufferEqual
            ( doBfStep
                ( BfState
                    "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
                    0
                    [0]
                    0
                    jmap
                    ""
                )
            )
            "Hello World!\n"

main :: IO ()
main = do
    quickCheck prop_buildMapSimple
    quickCheck prop_buildMapNested
    quickCheck prop_emptyProgram
    quickCheck prop_singleInstr
    quickCheck prop_helloWorld
