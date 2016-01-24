benchMap :: Int -> ()
benchMap v = runIdentity (for (map (+ 1) $ map (* 20) $ traverse_ yield [1..v]) (const (return ())))

benchDrop :: Int -> ()
benchDrop v = runIdentity (for (drop v $ traverse_ yield [1..v]) (const (return ())))

benchMapDropMap :: Int -> ()
benchMapDropMap v = runIdentity (for (drop (v - 10) $ map (+1) $ drop 10 $ traverse_ yield [1..v]) (const (return ())))
