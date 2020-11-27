data NestedList a = Elem a | List [ NestedList a ]

converters ( Elem x ) = x  ; converters ( List xs ) = (foldl (\ acc x -> (converters x ) ++ acc ) [] xs )


dataIdentifier (Elem a) = True ; dataIdentifier (List xs ) = False 

allConvertor (List[]) = [] ; allConvertor (List (x:xs)) = if ( dataIdentifier x ) then ( (converters x ): allConvertor (List xs ) ) else  ((allConvertor x) ++ (allConvertor ( List xs)))

dataIdentifier (List [ Elem 2 , Elem 7 ] )


(converters ( Elem 2 ))

allConvertor (List [Elem 1,  Elem 2, Elem 3, Elem 4, Elem 5])

allConvertor (List [ Elem 2 , Elem 7 , List [ Elem 2 , Elem 7 ] ] )  
allConvertor (List  [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

