fd x = case x of { 0 -> 1 ;   1 -> 10 ; _ -> 100 }




the recursive fibo gets stuck with fibo 50
myfib n | n == 1  = 1 | n == 2 = 1 | otherwise = myfib (n-1) + myfib (n-2)

the ones below go all the wya to myfibo 1000

myfib n | n == 1  = 1 | n == 2 = 1 | otherwise = head (foldl (\ acc x -> ((acc !! 0 ) + (acc !! 1 )):acc ) [ 1 , 1 ] [ 3..n])





fib n = fib (n-1) + fib (n-2)

(defn myfibo [ n ]
    
      ( cond (or ( = 1 n) (= 2 n ) ) 1 
               :else (loop [ last-n-1 1 last-n-2 1  n-val 3 ]
                           (if (= n n-val)
                                 ( + last-n-1 last-n-2  )
                                 (recur last-n-2 ( + (bigint last-n-1) last-n-2  ) (inc n-val) )))))
(myfibo 1000)


def myfibo(n):
    last_n_1 = 1
    last_n_2 = 1
    newfib = 1
    new_last_n_2 = 0
    i = 3
    if n==1 or n==2:
          return 1
    else:
      while i < (n + 1 ):
             newfib =    (last_n_2 + last_n_1 )
             new_last_n_2 = last_n_1
             last_n_1 = (last_n_2 + last_n_1 )
             last_n_2 =  new_last_n_2
             i = i + 1

    return newfib












