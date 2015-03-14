##Vector processing=group
##showplots
##Original=vector
##Final=vector
##Years=number 1
##Output=output vector
##spar=number .4
##Min_curvature=number 1e-3
##Step_pattern=string symmetricP2
##Step=number 25

library(dtw)
library(lattice)

stopifnot(proj4string(Original) ==  proj4string(Final))

from.pts <- coordinates(Original)[[1]][[1]]
to.pts <- coordinates(Final)[[1]][[1]]

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

from.df <- get_curvature(from.pts)
to.df <- get_curvature(to.pts)

# from.df <- subset(from.df, abs(Curvature) >= Min_curvature)
# to.df <- subset(to.df, abs(Curvature) >= Min_curvature)

is.open <- FALSE
alignment <- dtw(from.df$Curvature, to.df$Curvature,
                 step.pattern=eval(parse(text=Step_pattern)),#get(Step_pattern),
                 open.begin=is.open, open.end=is.open)

i <- seq_along(alignment$index1)
l <- sapply(i, function(idx) {
    Lines(
        Line( rbind(
            from.df[alignment$index1[idx], c('x', 'y')],
            to.df[alignment$index2[idx], c('x', 'y')]
        )), idx)
})
sl <- SpatialLines(l, proj4string=CRS(proj4string(Original)))
sl.df <- data.frame(i_from=alignment$index1, i_to=alignment$index2,
                    c_from=from.df[alignment$index1, 'Curvature'],
                    c_to=to.df[alignment$index2, 'Curvature'],
                    len=SpatialLinesLengths(sl, longlat=FALSE))
sl.df$rate <- sl.df$len/Years
sl.df$bend <- NA
bend <- 0
last.from <- 0
last.to <- 0
ready <- TRUE
for(i in seq_len(nrow(sl.df))) {
  if (abs(sl.df[i, 'c_from']) > Min_curvature && sl.df[i, 'c_from']*last.from>0 ||
      abs(sl.df[i, 'c_to']) > Min_curvature && sl.df[i, 'c_to']*last.to>0 ) {
    sl.df[i, 'bend'] <- bend
    ready <- TRUE
  } else {
    if (ready) { bend <- bend+1 }
    ready <- FALSE
  }
  last.from <- sl.df[i, 'c_from']
  last.to <- sl.df[i, 'c_to']
}

plot(bwplot(as.factor(bend) ~ rate, sl.df))
Output <- SpatialLinesDataFrame(sl, sl.df)
