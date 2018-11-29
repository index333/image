import System.Process
import System.Exit
import Graphics.UI.Gtk
import MyImage
mkopt' dig f = ["-verbose","-rotate",dig,f,f]
mkopt'' dig f = ["-rotate",dig,f]
mkopt fu ("right",f) = fu "90" f
mkopt fu ("left",f) = fu "-90" f
mkopt fu ("updown",f) = fu "180" f
mkopt _ _ = [] -- mkopt' "0" f
disp coms = do 
    fs0 <- getJpegFiles0
    let ops' = filter (\x -> null x == False)
            $ zipWith (\x y ->  mkopt mkopt'' (x, y)) coms fs0
    mapM_ (\x -> do 
                mapM_ (\y -> do {putStr y; putStr " "}) x
                putStrLn "")
        ops'
main = do
    c <- getContents
    let l = pac4 (toBoollist c) 
    fs <- getJpegFiles
    let coms = map b2c l
    let ops = filter (\x -> null x == False) 
            $ zipWith (\x y ->  mkopt mkopt'  (x, y)) coms fs
    if null ops 
        then exitWith ExitSuccess >> return () 
        else return () 
    disp coms
    initGUI 
    dia <- dialogNew
    dialogAddButton dia stockApply  ResponseApply
    dialogAddButton dia stockCancel ResponseCancel
    label <- labelNew (Just "Exec 'convert' command with these options?")
    upbox <- dialogGetUpper dia
    boxPackStart upbox label PackGrow 10
    widgetShowAll upbox

    answer <- dialogRun dia
    if answer == ResponseApply 
        then do {mapM_ (rawSystem "convert") ops; return ()}
        else widgetDestroy dia
    dia `on` unrealize $ mainQuit
