(defn power-set [ coll ]
  
 ( let [  distinct-indeces ( fn      [n p]
                                  (loop   [ coll #{}
                                     add  (int (Math/floor (rand n)))]
                                         (if ( = (count coll ) p )
                                         coll
                                        ( recur (if (coll add) coll (conj coll add)) (int (Math/floor (rand n)))))))
          combs             (fn       [ p q ]



                                    (let [ fact    (fn [ z ]  
                                         (loop [ t (if (zero? z) 1 z) coll [] ]
                                          (if (= t 1 )
                                         (apply * coll)
                                             (recur (dec t)  (conj coll t ) )))) ]
                                             (/ (fact p) ( * (fact q) (fact (- p q))))))

          mum-set             (fn    [j k ] 
                                   (loop [ mother-set #{} child-set (distinct-indeces j k)]
                    
                            (if (= (count mother-set) (combs j k ) )
                             mother-set 
                     
                              (recur (if (mother-set child-set) mother-set (conj mother-set child-set)) (distinct-indeces j k ) ))))
       unrefined   (for [ t (conj  (vec(for [ x  ( drop 1 (for [ x (for [ x ( range (count coll )) ] 
                              ( mum-set (count coll ) x )) ] (vec x ))) ] ( for [ y x] (for  [ z y  ] ( coll z))))) coll ) ] (vec t )) 
       take-last-out  (drop-last unrefined)    
       take-last-back (conj take-last-out [(last  unrefined )])
       nested   (vec (conj   (vec take-last-back)   [[]] ))  ]
                                     
         (loop [ refined [] cnt 0 ]   ( if (= cnt (count  nested )) refined 
             (recur (into refined  (nested cnt)) (inc cnt ) )))                              ))

   (count (power-set (vec (range 10)))


