module MyTable where
import Graphics.UI.Gtk
import Control.Monad
type Row = Int
type Col = Int
mkTable :: (Foldable t, WidgetClass widget) => t [widget] -> Row -> Col -> IO Table
mkTable ws rs cs = do
    t <- tableNew rs cs True
    zipWithM (\x (a,b,c,d)-> tableAttachDefaults t x a b c d) 
        (concat ws)
        (mkPos rs cs) 
    return t
mkPos r c = let rc = cross r c in 
    map (\(x,y) -> (y,y+1,x,x+1)) rc  
cross r c = [(a,b)|a <- [0..(r-1)],b <- [0..(c-1)]]
