f x s@(h:y) = 
    let (r,c) = divMod (length x) 9;
        m # n = m `div` 3 == n `div` 3;
        e = [0..8]
    in
        [a|z<-['1'..'9'], h==z||h=='.'&&notElem z(map((x++s)!!)[i*9+j|i<-e,
  j<-e,i==r||j==c||i#r&&j#c]),a<-f(x++[z])y]

f x[]=[x]
 
test=f [] "53..7....6..195....98....6.8...6...34..8.3..17...2...6.6....28....419..5....8..79"

-- not original
