import  Graphics.UI.Gtk 
main = do
    initGUI
    fchdal <- fileChooserDialogNew Nothing Nothing 
        FileChooserActionSelectFolder
        [("Cancel", ResponseCancel), ("Select", ResponseAccept)]
    fchdal `set` [fileChooserDoOverwriteConfirmation := True]
    widgetShow fchdal
    response <- dialogRun fchdal
    case response of
        ResponseCancel -> putStrLn "You cancelled..."
        ResponseAccept -> do 
            nwf <- fileChooserGetFilename fchdal
            case nwf of
                Nothing -> putStrLn "Nothing" 
                Just path -> putStrLn path 
    w <- windowNew 
    w `set` [windowDefaultWidth := 16, windowDefaultHeight := 16]
    s <- spinnerNew
    containerAdd w s 
    widgetShowAll w
    spinnerStart s
    widgetDestroy fchdal
    fchdal `on` objectDestroy $ mainQuit
    mainGUI
