##Vector processing=group
##Original=vector
##Final=vector
##Years=number 1
##Migration=output vector
##spar=number .4
##Curvature_multiplier=number 1e5
#Min_curvature=number 1e-3
##Step_pattern=string symmetricP05
##Step=number 25

library(dtw)

stopifnot(identicalCRS(Original, Final))
stopifnot(length(Original)==length(Final))

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

cLines <- -1

out <- lapply(seq_along(Original), function(iLine) {
    # No multipart features!
    stopifnot(length(coordinates(Original)[[iLine]]) == 1,
              length(coordinates(Final)[[iLine]]) == 1)

    from.pts <- coordinates(Original)[[iLine]][[1]]
    to.pts <- coordinates(Final)[[iLine]][[1]]

    from.df <- get_curvature(from.pts)
    to.df <- get_curvature(to.pts)

    message('Performing alignment...')
    is.open <- FALSE
    alignment <- dtw(
        with(from.df, data.frame(x,y,Curvature*Curvature_multiplier)),
        with(to.df, data.frame(x,y,Curvature*Curvature_multiplier)),
        window.type='sakoechiba',
        window.size=if (nrow(from.df) < 50) nrow(from.df) else nrow(from.df)/10,
        step.pattern=eval(parse(text=Step_pattern)),#get(Step_pattern),
        open.begin=is.open, open.end=is.open)

    message('Generating migration lines...')
    i <- seq_along(alignment$index1)
    l <- sapply(i, function(idx) {
        Lines(
            Line( rbind(
                from.df[alignment$index1[idx], c('x', 'y')],
                to.df[alignment$index2[idx], c('x', 'y')]
            )), cLines+idx)
    })
    sl <- SpatialLines(l, proj4string=CRS(proj4string(Original)))
    sl.df <- data.frame(i_from=alignment$index1, i_to=alignment$index2,
                        refid=iLine-1, # =?= rownames(Original@data)[[iLine]]
                        c_from=from.df[alignment$index1, 'Curvature'],
                        c_to=to.df[alignment$index2, 'Curvature'],
                        m_from=from.df[alignment$index1, 'm'],
                        m_to=to.df[alignment$index2, 'm'],
                        fr_from=from.df[alignment$index1, 'fr'],
                        fr_to=to.df[alignment$index2, 'fr'],
                        migration=SpatialLinesLengths(sl, longlat=FALSE))
    sl.df$rate <- sl.df$migration/Years
    rownames(sl.df) <- cLines+i
    cLines <<- cLines+length(alignment$index1)

    SpatialLinesDataFrame(sl, sl.df)
})

message('Merging results')
Migration <- do.call(rbind.SpatialLinesDataFrame, out)
