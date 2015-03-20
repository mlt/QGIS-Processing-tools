##Vector processing=group
##Bank_1=vector
##Bank_2=vector
##Fraction=number .5
##Centerline=output vector
##spar=number .4
##Step_pattern=string symmetricP05
##Step=number 25

library(dtw)

stopifnot(identicalCRS(Bank_1, Bank_2))
stopifnot(length(Bank_1)==length(Bank_2))

get_curvature <- function(mtx) {
    total.length <- apply(
        apply(mtx, 2, "diff"),
        1, function(x) sqrt(sum(x^2))
    )
    m <- cumsum(c(0, total.length))
    out <- lapply(1:2, function(y){
        smooth.spline(m, mtx[,y],
                      spar=spar, all.knots=TRUE, keep.data=FALSE)
    })
    names(out) <- c("xsp","ysp")
    m <- seq(0, tail(m, 1), by=Step)
    xx <- predict(out[["xsp"]], m)$y
    yy <- predict(out[["ysp"]], m)$y
    xp <- predict(out[["xsp"]], m, deriv=1)$y
    xpp <- predict(out[["xsp"]], m, deriv=2)$y
    yp <- predict(out[["ysp"]], m, deriv=1)$y
    ypp <- predict(out[["ysp"]], m, deriv=2)$y
    data.frame(x=xx,y=yy, Curvature = (xp*ypp - yp*xpp)/(xp^2 + yp^2)^1.5,
               m = m, fr = m/max(m))
}

out <- lapply(seq_along(Bank_1), function(iLine) {
    # No multipart features!
    stopifnot(length(coordinates(Bank_1)[[iLine]]) == 1,
              length(coordinates(Bank_2)[[iLine]]) == 1)

    from.pts <- coordinates(Bank_1)[[iLine]][[1]]
    to.pts <- coordinates(Bank_2)[[iLine]][[1]]

    from.df <- get_curvature(from.pts)
    to.df <- get_curvature(to.pts)

    message('Performing alignment...')
    is.open <- FALSE
    alignment <- dtw(
        with(from.df, data.frame(x,y)),
        with(to.df, data.frame(x,y)),
        window.type='sakoechiba',
        window.size=if (nrow(from.df) < 200) nrow(from.df) else nrow(from.df)/10,
        step.pattern=eval(parse(text=Step_pattern)),#get(Step_pattern),
        open.begin=is.open, open.end=is.open)
    
    message('Generating centerline...')
    l <- Lines(
        Line( (1-Fraction)*from.df[alignment$index1, c('x', 'y')] +
                 Fraction * to.df[alignment$index2, c('x', 'y')]
             ), iLine-1)
    
    sl <- SpatialLines(list(l), proj4string=CRS(proj4string(Bank_1)))
    SpatialLinesDataFrame(sl, data.frame(refid=iLine-1, row.names=iLine-1))
})

message('Merging results')
Centerline <- do.call(rbind.SpatialLinesDataFrame, out)
