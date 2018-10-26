White <- function(x, methode = "sampling") {
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
    ## 1) 0-4 a differenciált idősor átlaga
    ## period.apply(x,,mean)
    ## 2) Az eredeti idősor 0:00 különbsége.
    ## 3) Mostani 00:00 órából van kivonva az előző merre változik a trend?
    ## 4) Összeadni időlépés mennyi változik? idő szerint?
    ## 5) Sy elődnél lesz Sy=0.1/2 Meyboom a pórustér 50 % ürül csak le!
    diff.x
}
