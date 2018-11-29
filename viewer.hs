import System.FilePath.Posix
import Graphics.UI.Gtk
import System.Directory
import Control.Monad
import System.FilePath.Posix
newPixBufs size = mapM (\fn -> pixbufNewFromFileAtSize fn size size)
dirFile = "dir"
getD = readFile dirFile >>= (return . head . lines)
isJpg fn =  ex == ".jpg" || ex == ".JPG" where ex = takeExtension fn 
getJpegFiles0 = do
    getD >>= getDirectoryContents >>= filterM (return . isJpg) 
getJpegFiles = do
    getJpegFiles0  >>= mapM (\x -> do {d <- getD;return (d++"/"++x)})
newImgButton size f f' = do
    vb <- vBoxNew True 0
    p <- pixbufNewFromFileAtSize f size size 
    i <- imageNewFromPixbuf p
    b <- buttonNew 
    set b [buttonImage := i]
    l <- labelNew $ Just $ f' ++ " "
    containerAdd vb b
    containerAdd vb l
    return vb
newImgButtons :: Int ->[FilePath] -> [FilePath]-> IO [VBox]
newImgButtons size fs fs' = do
    bs <- zipWithM (\x y -> newImgButton size x y) fs fs'
    return bs
main = do
    files <- getJpegFiles 
    files' <- getJpegFiles0 
    initGUI
    w <-  windowNew
    set w [windowWindowPosition := WinPosCenter, 
            windowDefaultWidth := 1000, windowDefaultHeight := 150]
    hb <- hBoxNew False 0
    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport sw hb
    containerAdd w sw
    bs <- newImgButtons 64 files files'
    mapM_ (containerAdd hb) bs 
    widgetShowAll w
    w `on` unrealize $ mainQuit 
    mainGUI
