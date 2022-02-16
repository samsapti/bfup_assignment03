let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;

let hello : word = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)];;