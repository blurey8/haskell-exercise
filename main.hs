
-- 5
primes = sieve [2..]
    where sieve (x:xs) = x : sieve [y | y<-xs, y `mod` x /= 0]


-- 6
pythaTriple = [(x,y,z) | z<-[5..], y<-[1..z], x<-[1..y],  z^2 == x^2 + y^2]

pythaTripleFast = [(x,y,z) | z<-[5..], y<-[z, z-1 .. 1], x<-[y, y-1 .. 1],  z^2 == x^2 + y^2]

fun x = x + 1

yay x = x * 2


asik x = x - 1

mantap x = x + 100