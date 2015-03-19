##Vector processing=group
##Migration=vector
##Enumerated=output vector
##Min_curvature=number 1e-3
#showplots

sl.df <- Migration@data
sl.df$bend <- NA
last.refid <- -1
for(j in seq_len(nrow(sl.df))) {
    if (sl.df[j, 'refid'] != last.refid) {
        bend <- 1
        last.from <- 0
        last.to <- 0
        last.refid <- sl.df[j, 'refid']
        ready <- FALSE
    }
    if (abs(sl.df[j, 'c_from']) > Min_curvature && sl.df[j, 'c_from']*last.from>0 ||
            abs(sl.df[j, 'c_to']) > Min_curvature && sl.df[j, 'c_to']*last.to>0 ) {
        sl.df[j, 'bend'] <- bend
        ready <- TRUE
    } else {
        if (ready) { bend <- bend+1 }
        ready <- FALSE
    }
    last.from <- sl.df[j, 'c_from']
    last.to <- sl.df[j, 'c_to']
}

# plot(bwplot(as.factor(bend) ~ rate | as.factor(refid), sl.df))
Enumerated <- SpatialLinesDataFrame(Migration, sl.df)
