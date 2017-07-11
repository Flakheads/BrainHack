f' :: ([Integer], [Integer], [Integer]) -> ([Integer], [Integer], [Integer])
f' ((la:lb:xl), (ra:rb:xr), (xs)) = (lb:la:xl, rb:ra:xr, xs)
f' ((la:lb:xl), xr, xs) = f' ((la:lb:xl), (xr++[0]), xs)
f' (xl, xr, xs) = f' ((xl++[0]), (xr), xs)
