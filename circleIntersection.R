# Monte Carlo method to calculate the area
# of intersection of two circles

## ============================================================================
##                             Configure Circles
## ============================================================================
# [x, y, r]
circle1 <- c(3, 1, 4)
circle2 <- c(1, 1, 3)


## ============================================================================
##                                  Program
## ============================================================================
# pt = [x, y]
# circ = [x, y, r], where x and y are the coordinates of the center
inCircle <- function(pt, circ) {
    if((pt[1] - circ[1])^2 + (pt[2] - circ[2])^2 < circ[3]^2) {
        return(1)
    }
    return(0)
}

# Calculate bounding rectangle
boundingRectangle <- c(min(circle1[1] - circle1[3], circle2[1] - circle2[3]),
                       max(circle1[1] + circle1[3], circle2[1] + circle2[3]),
                       min(circle1[2] - circle1[3], circle2[2] - circle2[3]),
                       max(circle1[2] + circle1[3], circle2[2] + circle2[3]))


rectangleArea <- (boundingRectangle[2] - boundingRectangle[1]) * (boundingRectangle[4] - boundingRectangle[3])


# Simulate
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
