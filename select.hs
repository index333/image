import MyImage
import Graphics.UI.Gtk 
import Control.Monad

newSelectButton label box = do
    b <- buttonNewWithLabel label
    b `on` buttonActivated $ func1 b
    containerAdd box b
    return b
newSelectButtons labels box = do
    mapM ((flip newSelectButton) box) labels
func1 b = do
    b `get` buttonLabel
    >>=
    putStrLn 
    >>
    mainQuit
main = do
    fl <- getJpegFiles
    initGUI
    w <-  windowNew
    set w [windowWindowPosition := WinPosCenter, 
            windowDefaultWidth := 1000, windowDefaultHeight := 350]
    hb <- hBoxNew True 0
    vb <- vBoxNew True 0
    let selections = [  "Right All", 
                        "Left All",
                        "Right-Left",
                        "Left-Right",
                        "UpDown All",
                        "Up-Down",
                        "Down-Up",
                        "NO Pattern" ] 
    newSelectButtons selections hb
    containerAdd vb hb

    ps <- newPixBufs 50 fl  
    bs <- newImgButtons ps
    zipWithM (\x y -> set x [buttonLabel := (show y)]) bs [0..(length fl - 1)]
    hb1 <- hBoxNew False 0 
    mapM (containerAdd hb1) bs
    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport sw hb1
    containerAdd vb sw
    containerAdd w vb
    widgetShowAll w
    w `on` unrealize $ mainQuit 
    mainGUI
