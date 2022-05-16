White <- function(x, methode = "sampling", Sy = 0.1, Meyboom = 0.5) {
    require(xts)
    ## Idősor mintavételezés (fél óra) vagy simítás
    if(methode == "sampling")
    {
        halfhour.idx <- endpoints(x, on = "minutes", k = 30)
        smooth.x <- period.apply(x, halfhour.idx, mean)
        ## 10 perc visszacsúsztatás
        smooth.x <- xts(coredata(smooth.x), index(smooth.x) - 10*60)
    }
    ## Idősor differenciálás
    diff.x <- diff.xts(smooth.x)
    diff.x <- na.omit(diff.x)
    ## 1) 0-4 a differenciált idősor átlaga
    ## period.apply(x,,mean)
    fourhour.idx <- endpoints(diff.x, on = "hours", k = 4)
    slope <- period.apply(diff.x, fourhour.idx, median)
    valid.idx <- seq(1, length(slope), by = 6)
    ## Iránytangens órás
    slope.ok <- slope[valid.idx]*2
    slope.core <- coredata(slope.ok)
    slope.hourly <- xts(slope.core, round.POSIXt(index(slope.ok), unit="day"))
    ## Napi vonal
    slope.end <- as.numeric(slope.core * 24)
    slope.end <- slope.end[-length(slope.end)]
    ## 2) Az eredeti idősor 0:00 különbsége.
    ## 3) Mostani 00:00 órából van kivonva az előző merre változik a trend?
    day.end <- endpoints(x, on="days")
    day.begin <- day.end + 1
    values.at.midnight <- x[day.begin[-length(day.begin)]]
    daily.diffs <- diff.xts(values.at.midnight)
    daily.at.place <- values.at.midnight - as.numeric(coredata(daily.diffs))
    daily.at.place <- na.omit(daily.at.place)
    ## 4) Összeadni időlépés mennyi változik? idő szerint?
    top.slope <- daily.at.place + slope.end
    ## 5) Sy elődnél lesz Sy=0.1/2 Meyboom a pórustér 50 % ürül csak le!
    ## mm konverzió és a Meyboom-féle szorzó az argumentumokból.
    ET <- (top.slope - values.at.midnight)*Sy*1000*Meyboom
    colnames(ET) <- "CalculatedET"
    ET <- round(ET,4)
    list(ori.gw = x, results = merge.xts(slope.hourly,
                                         daily.at.place,
                                         values.at.midnight,
                                         top.slope,
                                         ET)
         )
}

plot.White <- function(x){
    ## Az indexet csak a létezőkhöz
    top.idx <- index(x$results$top.slope)
    plot.top <- xts(coredata(x$results$top.slope), top.idx-1)
    white.line <- c(plot.top, x$results$values.at.midnight)
    maxline <- max(white.line, na.rm=TRUE)
    gwmax <- max(x$ori.gw, na.rm=TRUE)
    maxline <- ifelse(maxline > gwmax, maxline, gwmax)
    mindata <- min(x$ori.gw)
    par(las=1, mar=c(3.1,4.1,.5,.5))
    plot(index(x$ori.gw),
         coredata(x$ori.gw),
         xaxs="i",ylim=c(mindata,maxline),
         ylab="h [m]", xlab="", xaxt="n", type="n")
    lines(index(x$ori.gw),
         coredata(x$ori.gw), lwd=2)
    ts.start <- trunc(start(plot.top), unit="months")
    ts.dayend <- end(plot.top)
    ts.end <- trunc(ts.dayend, unit="months")
    ts.month <- seq(ts.start,ts.end, by="months")
    ts.day <- seq(ts.start,ts.dayend, by="days")
    axis(1, at = ts.month, lab=F)
    axis(1, at = ts.day, lab=F, col="lightgray", lty="dotted", tck=1)
    axis.POSIXct(1, x = ts.month, at=ts.month + c(20,15,5)*24*60*60, format="%B", tcl=0)
    lines(index(white.line),coredata(white.line))
    points(index(x$results$daily.at.place),coredata(x$results$daily.at.place))
}
