# Monte Carlo method to calculate the area
# of intersection of two circles

# pt = [x, y]
# circ = [x, y, r], where x and y are the coordinates of the center
inCircle <- function(pt, circ) {
    if(sqrt((pt[1] - circ[1])^2 + (pt[2] - circ[2])^2) < circ[3]) {
        return(1)
    }
    return(0)
}

# Setup
circle1 <- c(0, 0, 1) # [x, y, r]
circle2 <- c(1, 0, 1)
boundingRectangle <- c(-1, 2, -1, 1)  # [x1, x2, y1, y2]


# Simulate
rectangleArea <- (boundingRectangle[2] - boundingRectangle[1]) * (boundingRectangle[4] - boundingRectangle[3])
cnt <- 0
hit1 <- 0
hit2 <- 0
hitBoth <- 0
while(cnt < 1000000000000000000000000000000) {
    cnt <- cnt + 1
    pt <- c(runif(1, boundingRectangle[1], boundingRectangle[2]),
            runif(1, boundingRectangle[3], boundingRectangle[4]))

    in1 <- inCircle(pt, circle1)
    in2 <- inCircle(pt, circle2)
    hit1 <- hit1 + in1
    hit2 <- hit2 + in2
    if(in1 && in2) { hitBoth <- hitBoth + 1 }

    coef <- rectangleArea / cnt
    cat("(run", cnt, ")\t",
        "Area 1: ", (hit1 * coef), "\t",
        "Area 2: ", (hit2 * coef), "\t",
        "Intersection: ", (hitBoth * coef), "\n")
}
