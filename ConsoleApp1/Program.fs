printfn "Hello World"
let solve (a,b,c) =
     let D = b*b-4.*a*c
     ((-b+sqrt(D))/(2.*a), (-b-sqrt(D))/(2.*a)) in
         solve (1.0, 2.0,-3.0);;