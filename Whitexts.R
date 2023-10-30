White <- function(x, methode = "sampling", Sy = 0.1, Meyboom = 0.5) {
    require(xts)
    ## Idősor mintavételezés (fél óra) vagy simítás
    if(methode == "sampling")
    {
        ## Fél órák utolsó mérései 10 min gyakoriság 20 min-nél
        halfhour.idx <- endpoints(x, on = "minutes", k = 30)
        ## A félórás ablakokban átlag
        smooth.x <- period.apply(x, halfhour.idx, mean)
        ## 15 perc visszacsúsztatás a félórásnál
        smooth.x <- xts(coredata(smooth.x), index(smooth.x) - 5*60)
    }
    ## Idősor differenciálás
    diff.x <- diff.xts(smooth.x)
    diff.x <- na.omit(diff.x)
    diff.x <- c(head(smooth.x,1),diff.x)
    diff.x[1,1] <- diff.x[2,1]
    ## 1) 0-4 a differenciált idősor átlaga
    ## Négy órás szakaszok lehatárolása
    fourhour.idx <- endpoints(diff.x, on = "hours", k = 4)
    ## Itt el volt csúszva, ez a helyreállítás
    fourhour.idx <- fourhour.idx + 4
    fourhour.idx[1] <- 0
    fourhour.idx <- fourhour.idx[-length(fourhour.idx)]
    ## A négy órás időszakokban a meredekség meghatározása mediánnal
    slope <- period.apply(diff.x, fourhour.idx, median)
    ## Minden hatodik elem kiszedése
    valid.idx <- seq(1, length(slope), by = 6)
    ## Iránytangens órás, mivel félórásra simított idősorból megy, így a duplája
    slope.ok <- slope[valid.idx]*2
    slope.core <- coredata(slope.ok)
    slope.hourly <- xts(slope.core, round.POSIXt(index(slope.ok), unit="day"))
    ## Napi vonal 24r
    slope.end <- as.numeric(slope.core * 24)
    slope.end.calc <- slope.end[-length(slope.end)]
    slope.end.xts <- xts(slope.end, index(slope.hourly))
    ## 2) Az eredeti idősor 0:00 különbsége.
    ## 3) Mostani 00:00 órából van kivonva az előző merre változik a trend?
    day.end <- endpoints(x, on="days")
    day.begin <- day.end + 1
    values.at.midnight <- x[day.begin[-length(day.begin)]]
    ## Az egymást követő napok különbségei (+- s)
    daily.diffs <- diff.xts(values.at.midnight)
    ## A következő nap értéke éjfélkor
    daily.at.place <- values.at.midnight - as.numeric(coredata(daily.diffs))
    daily.at.place <- na.omit(daily.at.place)
    ## 4) Összeadni időlépés mennyi változik? idő szerint?
    top.slope <- daily.at.place + slope.end.calc
    ## 5) Sy elődnél lesz Sy=0.1/2 Meyboom a pórustér 50 % ürül csak le!
    ## mm konverzió és a Meyboom-féle szorzó az argumentumokból.
    ET <- (top.slope - values.at.midnight)*Sy*1000*Meyboom
    colnames(ET) <- "CalculatedET"
    ET <- round(ET,4)
    list(ori.gw = x,
         smooth.gw = smooth.x,
         results = merge.xts(slope.hourly,
                                         slope.end.xts,
                                         daily.diffs,
                                         daily.at.place,
                                         values.at.midnight,
                                         top.slope,
                                         ET)
         )
}

plot.White <- function(x, daily.num = NULL, ...){
    ## Az indexet csak a létezőkhöz
    top.idx <- index(x$results$top.slope)
    plot.top <- xts(coredata(x$results$top.slope), top.idx-1)
    white.line <- c(plot.top, x$results$values.at.midnight)
    maxline <- max(white.line, na.rm=TRUE)
    gwmax <- max(x$ori.gw, na.rm=TRUE)
    maxline <- ifelse(maxline > gwmax, maxline, gwmax)
    mindata <- min(x$ori.gw)
    par(las=1, mar=c(3.1,4.1,.5,3.1))
    plot(index(x$ori.gw),
         coredata(x$ori.gw),
         xaxs="i",ylim=c(mindata,maxline),
         ylab="h [m]", xlab="", xaxt="n", type="n", ...)
    lines(index(x$ori.gw),
         coredata(x$ori.gw), lwd=2)
    lines(index(x$smooth.gw),
         coredata(x$smooth.gw), col = 2)
    ts.start <- trunc(start(plot.top), unit="months")
    ts.dayend <- end(plot.top)
    ts.end <- trunc(ts.dayend, unit="months")
    ts.month <- seq(ts.start,ts.end, by="months")
    ts.day <- seq(ts.start,ts.dayend, by="days")
    axis(1, at = ts.month, lab=F)
    axis(1, at = ts.day, lab=F, col="lightgray", lty="dotted", tck=1)
    axis.POSIXct(1, x = ts.month, at=ts.month + 15*24*60*60, format="%B", tcl=0)
    if(!is.null(daily.num)) {
        axis.POSIXct(1, x = ts.day, at=ts.day + 12*60*60, format="%d", tcl=0)
    }
    lines(index(white.line),coredata(white.line))
    points(index(x$results$daily.at.place),coredata(x$results$daily.at.place))
}
