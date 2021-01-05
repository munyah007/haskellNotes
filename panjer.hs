listOfGs fs lambda pnot | ( length fs ) == 0 = [ pnot ] | (length fs)==1 = [ (((head fs ) * pnot ) * lambda ) , pnot] | otherwise = foldl ( \ acc x -> (( sum  (zipWith (*) [ 1..(fromIntegral (length acc))] (zipWith (*) ( take (length acc) fs )  acc ) ) )  * ( lambda / (fromIntegral (length acc )))) : acc ) [ (((head fs) * pnot) *lambda ), pnot ]  (drop 1 fs )    

     
              



