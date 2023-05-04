nand :: Bool -> Bool -> Bool
nand True True = False
nand True False = True
nand False p = True

maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj _ True True = True
maj True _ True = True
maj False False _ = False
maj False _ False = False
maj _ False False = False
