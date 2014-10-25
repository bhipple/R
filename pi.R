# Monte Carlo method to calculate pi
# Circle at origin with radius 1 has area pi
# Bounding square with side=2 has area 4

inCircle <- function(pt) {
    if(sqrt(pt[1]^2 + pt[2]^2) < 1) {
        return(1)
    }
    return(0)
}

cnt <- 0
hits <- 0
while(cnt < 1000000000000000) {
    cnt <- cnt + 1
    pt <- runif(2, -1, 1)
    if(inCircle(pt)) { hits <- hits + 1 }

    pi <- 4 * (hits / cnt)
    cat("(run", cnt, ")\t\t", pi, "\n")
}
