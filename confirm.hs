import MyTable
import MyImage
import Graphics.UI.Gtk
import Control.Monad
main = do
    c <- getContents
    let l = pac4 (toBoollist c) 
    let coms = map b2c l
    fl <- getJpegFiles
    initGUI
    w <-  windowNew
    set w[windowTitle := "Confirm window", 
            windowWindowPosition := WinPosCenter,
            windowDefaultWidth := (length fl*100), 
            windowDefaultHeight := 350]
    hb <- hBoxNew True 0
    hb1 <- hBoxNew True 0 
    vb <- vBoxNew True 0
    let txt = "Close this window,to go to the next step.\n" ++
            "Modified pictures are in the upper row.\n" ++
            "Original pictures are in the lower row."
    l0 <- labelNew $ Just txt
    ps <- newPixBufs 50 fl  
    bs <- newImgButtons ps
    zipWithM (\x y -> x `set` [buttonLabel := (show y)]) bs [0..(length fl - 1)]
    vbs <- zipWithM (viewButton) ps coms
    t <- mkTable [vbs, bs] (2::Row) ((length bs)::Col) 
    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport sw t
    boxPackStart hb l0 PackNatural 0
    boxPackStart vb hb PackNatural 0
    containerAdd vb sw
    containerAdd w vb
    widgetShowAll w
    w `on` unrealize $ mainQuit >> putStrLn c
    mainGUI
