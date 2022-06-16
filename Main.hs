{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.IO
import System.Exit

import Data.List
import Data.Char
import Data.Maybe

import Debug.Trace

import System.Environment
import System.Console.CmdArgs

import Data.Functor

import Control.Exception

data HED = HED
    { inFile :: String } deriving (Data, Typeable, Show, Eq)

hedDef = HED { inFile = def &= args &= typ "FILE" }

data Buffer = Buffer { blines :: [String] } deriving (Show)

data HedState = HedState 
    { buffer :: Buffer
    , bufferModified :: Bool
    , line :: Int
    , promptChar :: String
    , showPrompt :: Bool
    , file :: String
    , printErrors :: Bool
    , lastError :: Maybe HedError 
    , prevState :: Maybe HedState } deriving (Show)

data HedError = ErrorBadCommand | ErrorUnknown | ErrorInvalidAddress | ErrorModifiedBuffer deriving (Show)

data Range = DualRange { rstart :: Address, rend :: Address }
           | SingleRange { raddr :: Address }
           | NoRange
           deriving (Show)

data Address = AssumedAddress {addr :: Int} -- Initial value assumed
             | LiteralAddress {addr :: Int} -- Initial value explicitly defined
             | NoAddress -- No Address read
             deriving (Show)

if' :: Bool -> a -> a -> a
if' True  a _ = a
if' False _ b = b

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler
                   $ withFile path ReadMode (\h -> do size <- hFileSize h
                                                      return $ Just size)
                   where
                       handler :: SomeException -> IO (Maybe Integer)
                       handler _ = return Nothing

printFileSize :: HedState -> IO ()
printFileSize st = (getFileSize . file $ st) >>= maybe (return ()) (putStrLn . show)

modifyStateWithUndo :: HedState -> HedState -> HedState
modifyStateWithUndo original modified = modified { prevState = Just original }

undoClosure :: HedCmd -> HedCmd
undoClosure cmd hs r s = do ret <- cmd hs r s
                            case ret of
                                Left e    -> return . Left $ e
                                Right hs' -> return . Right $ modifyStateWithUndo hs hs'
                            

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- cmdArgs hedDef
    buf <- readFileToBuffer $ inFile args
    let s = HedState buf False 1 "*" False (inFile args) False Nothing Nothing
    printFileSize s
    mainLoop s

mainLoop :: HedState -> IO ()
mainLoop st = do
    command <- prompt st
    st' <- interpretCommand st command >>= handleError st
    mainLoop st'

readFileToBuffer :: String -> IO Buffer
readFileToBuffer s = readFile s <&> Buffer . lines

modifyBuffer :: HedState -> HedState
modifyBuffer hs = hs {bufferModified = True}

lastLine :: Buffer -> Int
lastLine = length . blines

getBufferLine :: Buffer -> Int -> String
getBufferLine b i = (blines b) !! (i - 1)

prompt :: HedState -> IO String
prompt st = do
    putStr (if showPrompt st then promptChar st else "")
    getLine

asSingleRange :: HedState -> Range -> Int
asSingleRange st NoRange = line st
asSingleRange _ (SingleRange (LiteralAddress l)) = l
asSingleRange _ (DualRange _ (LiteralAddress r)) = r

isNoRange :: Range -> Bool
isNoRange NoRange = True
isNoRange _ = False

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

rangeSlice :: Range -> Buffer -> [String]
rangeSlice (SingleRange (LiteralAddress l)) (Buffer sx) = [sx !! (l -1)]
rangeSlice (DualRange (LiteralAddress l) (LiteralAddress r)) (Buffer sx) = slice (l-1) (r-1) sx

getLastLine :: Range -> Int
getLastLine (DualRange _ (LiteralAddress r)) = r
getLastLine (SingleRange (LiteralAddress l)) = l

getFirstLine :: Range -> Int
getFirstLine (DualRange (LiteralAddress l) _) = l
getFirstLine (SingleRange (LiteralAddress l)) = l

interpretCommand :: HedState -> String -> IO (Either HedError HedState)
interpretCommand st s = do
    let (range, sc) = readRange st s 
    if sc == "" then do
        if isNoRange range then do
            return . Left $ ErrorInvalidAddress
        else do 
            printCmd st range ""
    else do
        let opts = tail sc
        getCmd (head sc) st range opts

handleError :: HedState -> Either HedError HedState -> IO HedState
handleError hs (Left e)    = do
    putStrLn "?"
    if printErrors hs then 
        printError $ lastError hs 
    else
        putStr ""
    return (hs {lastError = Just e})
handleError hs (Right new) = return new

getInputLines :: IO [String]
getInputLines =
    let loop :: [String] -> IO [String]
        loop ("." : xs) = return (reverse xs)
        loop xs = do
            i <- getLine
            loop (i : xs)
    in loop []

type HedCmd = HedState -> Range -> String -> IO (Either HedError HedState)

quitCmd :: HedCmd
quitCmd st r s = if bufferModified st then return . Left $ ErrorModifiedBuffer else forceQuitCmd st r s

forceQuitCmd :: HedCmd
forceQuitCmd st _ _ = exitSuccess >> return (Right st)

replaceStr :: String -> Char -> String -> String
replaceStr _ _ "" = ""
replaceStr r c (x : xs) = if x == c then r ++ (replaceStr r c xs) else (x : replaceStr r c xs)

listCmd :: HedCmd
listCmd st NoRange _ = putStrLn (getBufferLine (buffer st) (line st)) >> return (Right st)
listCmd st r _ = mapM_ (putStrLn . (++ "$") . replaceStr "\\$" '$') (rangeSlice r (buffer st)) >> return (Right $ st {line = getLastLine r})

enumerateCmd :: HedCmd
enumerateCmd st NoRange _ = putStrLn ((show . line $ st) ++ "\t" ++ (getBufferLine (buffer st) (line st))) >> return (Right st)
enumerateCmd st r _ = mapM_ putStrLn (zipWith (\x y -> show x ++ "\t" ++ y) [getFirstLine r .. getLastLine r] (rangeSlice r (buffer st))) >> 
                      return (Right $ st {line = getLastLine r})

printCmd :: HedCmd
printCmd st NoRange _ = putStrLn (getBufferLine (buffer st) (line st)) >> return (Right st)
printCmd st r _ = mapM_ putStrLn (rangeSlice r (buffer st)) >> return (Right $ st {line = getLastLine r})

promptCmd :: HedCmd
promptCmd st _ _ = return . Right $ st {showPrompt = not $ showPrompt st}

showErrorCmd :: HedCmd
showErrorCmd st _ _ = printError (lastError st) >> return (Right st)

toggleErrorCmd :: HedCmd
toggleErrorCmd st _ _ = return . Right $ st {printErrors = not $ showPrompt st}

unknownCmd :: HedCmd
unknownCmd _ _ _ = return . Left $ ErrorBadCommand  

appendCmd :: HedCmd
appendCmd st r _ = do
    let p = asSingleRange st r
    l <- getInputLines
    let modified = if' (l == [""]) id modifyBuffer
    let oldLines = blines $ buffer st
    let st' = modified $ st {buffer = Buffer (take p oldLines ++ l ++ drop p oldLines)}
    return (Right st')

joinCmd :: HedCmd
joinCmd _ NoRange _ = return . Left $ ErrorInvalidAddress
joinCmd st r _ = let joined = concat (rangeSlice r (buffer st))
                     oldLines = blines . buffer $ st
                     rl = getFirstLine r
                     rr = getLastLine r
                     newLeft = take (rl - 1) oldLines
                     newRight = drop rr oldLines
                 in return . Right . modifyBuffer $ st {buffer = Buffer (newLeft ++ [joined] ++ newRight)}

insertCmd :: HedCmd
insertCmd st r _ = do
    let p = asSingleRange st r
    l <- getInputLines
    let modified = if' (l == [""]) id modifyBuffer
    let oldLines = blines $ buffer st
    let st' = modified $ st {buffer = Buffer (take (p-1) oldLines ++ l ++ drop (p-1) oldLines)}
    return (Right st')

writeCmd :: HedCmd
writeCmd st _ _ = do
    writeFile (file st) (intercalate "\n" . blines . buffer $ st)
    printFileSize st
    return . Right $ st {bufferModified = False}

undoCmd :: HedCmd
undoCmd HedState{prevState = Nothing} _ _ = return . Left $ ErrorUnknown
undoCmd HedState{prevState = Just pv} _ _ = return . Right $ pv

commentCmd :: HedCmd
commentCmd st _ _ = return . Right $ st

getCmd :: Char -> HedCmd
getCmd 'a' = undoClosure appendCmd
getCmd 'i' = undoClosure insertCmd
getCmd 'u' = undoClosure undoCmd
getCmd 'j' = undoClosure joinCmd
getCmd 'w' = writeCmd
getCmd 'q' = quitCmd
getCmd 'Q' = forceQuitCmd
getCmd 'p' = printCmd
getCmd 'l' = listCmd
getCmd 'n' = enumerateCmd
getCmd 'P' = promptCmd
getCmd 'h' = showErrorCmd
getCmd 'H' = toggleErrorCmd
getCmd '#' = commentCmd
getCmd _ = unknownCmd

-- Reads a number from a stirng, concats it off, and return the number & the rest of the string.
stringToNumber :: String -> (Int, String)
stringToNumber s = let digit c = elemIndex c "0123456789"
                       h :: Int -> String -> (Int, String)
                       h o "" = (o, "")
                       h o (c : s)
                           | isJust $ digit c = h ((o * 10) + fromJust (digit c)) s
                           | otherwise = (o, c : s)
                   in h 0 s

readAddress :: HedState -> String -> (Address, String)
readAddress hs s' =
    let i :: String -> Maybe (Int, String) -- base value of address
        i "" = Nothing
        i ('.' : s) = Just (line hs, s)
        i ('$' : s) = Just (lastLine . buffer $ hs, s)
        i s
            | isDigit $ head s = Just $ stringToNumber s
            | otherwise = Nothing
        math n s sum = if (s /= "") && isDigit (head s) 
                       then let (val, rest) = stringToNumber s in m True (sum + (val * n)) rest
                       else m True (sum + n) s
        m :: Bool -> Int -> String -> Maybe (Int, String) -- Any math operations after the main 
        m _ sum ('+' : s) = math 1 s sum
        m _ sum ('-' : s) = math (-1) s sum 
        m b sum rest = if b then Just (sum, rest) else Nothing
        res = i s'
    in case res of -- TODO: Convert to DO notation?
        Just (x, s) -> let res = m False x s
                       in case res of
                           Just (v, rest) -> (LiteralAddress v, rest)
                           Nothing        -> (LiteralAddress x, s) 
        Nothing     -> let res = m False 0 s' 
                       in case res of 
                           Just (v, rest) -> (AssumedAddress v, rest)
                           Nothing        -> (NoAddress, s')

data SepType = CommaSep | SemiSep | NoSep deriving (Show)

readRange :: HedState -> String -> (Range, String)
readRange hs s' =
    let (leftAddr, lcont) = readAddress hs s'
        getSep :: String -> (SepType, String)
        getSep (',' : s) = (CommaSep, s)
        getSep (';' : s) = (SemiSep, s)
        getSep s         = (NoSep, s)
        (sep, scont) = getSep lcont
        (rightAddr, rcont) = readAddress hs scont
        makeraddr :: Address -> Address
        makeraddr NoAddress = LiteralAddress (lastLine . buffer $ hs)
        makeraddr (AssumedAddress a) = LiteralAddress ((+) a . lastLine . buffer $ hs)
        makeraddr l = l
        rtype :: Address -> SepType -> Address -> Range
        rtype NoAddress          NoSep    NoAddress = NoRange
        rtype (LiteralAddress l) CommaSep r         = DualRange   (LiteralAddress l)               (makeraddr r)
        rtype (LiteralAddress l) SemiSep  r         = DualRange   (LiteralAddress l)               (makeraddr r)
        rtype (AssumedAddress l) CommaSep r         = DualRange   (LiteralAddress (1 + l))         (makeraddr r)
        rtype (AssumedAddress l) SemiSep  r         = DualRange   (LiteralAddress ((line hs) + l)) (makeraddr r)
        rtype NoAddress          CommaSep r         = DualRange   (LiteralAddress 1)               (makeraddr r)
        rtype NoAddress          SemiSep  r         = DualRange   (LiteralAddress (line hs))       (makeraddr r)
        rtype (LiteralAddress l) NoSep    NoAddress = SingleRange (LiteralAddress l)
        rtype (AssumedAddress l) NoSep    NoAddress = SingleRange (LiteralAddress ((line hs) + l))
    in (rtype leftAddr sep rightAddr, rcont)


errorString :: HedError -> String
errorString ErrorInvalidAddress = "Invalid Address"
errorString ErrorBadCommand = "Unknown Command"
errorString ErrorUnknown = "Unknown Error"
errorString ErrorModifiedBuffer = "Modified Buffer"

printError :: Maybe HedError -> IO ()
printError = putStrLn . maybe "" errorString

