module MyImage where
import System.Exit
import System.FilePath.Posix
import Graphics.UI.Gtk
import System.Directory
import Control.Monad
dirFile = "dir"
getD = readFile dirFile >>= (return . head . lines)
isJpg fn =  ex == ".jpg" || ex == ".JPG" 
    where ex = takeExtension fn 
getJpegFiles0 = do
    getD 
    >>= 
    getDirectoryContents 
    >>= 
    filterM (return . isJpg) 
getJpegFiles = do 
    getJpegFiles0 
    >>= 
    (\x -> if null x    
            then do print " No Jpg file in this directory." 
                    exitWith ExitSuccess  
                    return x 
            else return x)
    >>= 
    mapM (\x -> do {d <- getD;return (d++"/"++x)})
viewButtons pvs blist = zipWithM viewButton pvs $ map b2c $ pac4 blist
viewButton' pv rotation = do 
    b <- buttonNew
    pv' <- pixbufRotateSimple pv rotation 
    i <- imageNewFromPixbuf pv'
    set b [buttonImage := i]
    return b
viewButton pv "right" = viewButton' pv PixbufRotateClockwise
viewButton pv "left" = viewButton' pv PixbufRotateCounterclockwise
viewButton pv "updown" = viewButton' pv PixbufRotateUpsidedown
viewButton pv _ = viewButton' pv PixbufRotateNone
newPixBufs size = mapM (\fn -> pixbufNewFromFileAtSize fn size size)
newImgButton p = do
    b <- buttonNew 
    i <- imageNewFromPixbuf p
    set b [buttonImage := i]
    return b
newImgButtons :: [Pixbuf] -> IO [Button]
newImgButtons = mapM newImgButton 
newRadioButton c n = do
    b <- radioButtonNew
    p <- pixbufNewFromFileAtSize (c ++ ".png") 32 32
    i <- imageNewFromPixbuf p
    set b [buttonImage := i]
    return b
newRadioButtons i = do
    bb@(b:bs) <- mapM (\x -> newRadioButton x i) ["keep","right","left","updown"]
    mapM_ (\x -> x `set` [radioButtonGroup := b]) bs
    return bb
b2c :: [Bool] -> String
b2c [True,False,False,False] = "keep"
b2c [False,True,False,False] = "right"
b2c [False,False,True,False] = "left"
b2c [False,False,False,True ]= "updown"
pac' _ [] l = reverse l
pac' i l ll = pac' i (drop i l) (take i l : ll)
pac i l = pac' i l []
pac4 = pac 4 
toBoollist :: String -> [Bool]
toBoollist = read 