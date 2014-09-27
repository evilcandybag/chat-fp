import Haste


main = do
  alert "tjena!"
  withElem "entry" $ \e -> do
    onEvent e OnKeyPress $ \key -> case key of
      13 -> alert "wrotelol"
      _ -> return ()
