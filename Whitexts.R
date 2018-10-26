White <- function(x, methode = "sampling", Sy = 0.1) {
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
    slope <- period.apply(diff.x, fourhour.idx, mean)
    valid.idx <- seq(2, length(slope), by = 6)
    slope <- slope[valid.idx]
    slope.end <- as.numeric(coredata(slope)*48)
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
    top.slope - values.at.midnight
}
