##Vector processing=group
##Enumerated=vector
##Meanders=output vector
#showplots

library(plyr)

sl.df <- Enumerated@data
sl.df$idx <- seq_len(nrow(sl.df))
bends <- na.omit(
  ddply(sl.df, .(refid, bend), function(x) {
    rng <- range(x$idx)
    cntr <- round(mean(rng))
    len1 <- diff(range(x$m_from))#diff(sl.df[rng, 'm_from'])
    len2 <- diff(range(x$m_to))#diff(sl.df[rng, 'm_to'])
    c_from <- x$c_from[which.max(abs(x$c_from))]
    c_to <- x$c_to[which.max(abs(x$c_to))]
    r <- abs(c_from/c_to)
    cl <- if (r>10 && (abs(c_from)+abs(c_to)>.005)) 'collapsed'
    else if (r<.1 && (abs(c_from)+abs(c_to)>.005)) 'emerged'
    else 'migrated'
    cbind(data.frame(cntr=cntr, len=mean(len1,len2),
                     class=cl,
                     c_from=c_from, c_to=c_to),
          as.list(setNames(summary(x$rate),
                           c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max'))))
  }))
pts <- sapply(Enumerated@lines[bends$cntr], function(x) apply(coordinates(x)[[1]], 2, "mean"))
Meanders <- SpatialPointsDataFrame(t(pts), bends, proj4string = CRS(proj4string(Enumerated)))
