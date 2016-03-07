library(fields)

pal <- wes_palette("Zissou", 100, type = "continuous")

xFGi <- cbind(mgdat2i$DOFF, mgdat2i$HeightRGRC)
outFGi <- Tps(xFGi, mgdat2i$RelFit)

surface(outFGi, type="p", xlab="Flowering Day", ylab="Growth", zlab="Fitness", add.legend=FALSE, col= pal, border = NA)
surface(outFGi, type="C", xlab="Flowering Day", ylab="Growth", add.legend=TRUE, col = pal)

par(new = T)
with(mgdat2i, plot(HeightRGRC, DOFF, axes=TRUE, ylim = c(-2.2, 2.2), xlim = c(-3.4, 2), xlab = "", ylab = ""))
par(new = F)

xFGac <- cbind(mgdat2ac$DOFF, mgdat2ac$HeightRGRC)
outFGac <- Tps(xFGac, mgdat2ac$RelFit)

surface(outFGac, type="p", xlab="Flowering Day", ylab="Growth", zlab="RelFit", add.legend=FALSE, col= pal, border = NA)
surface(outFGac, type="C", xlab="Flowering Day", ylab="Growth", zlab="RelFit", add.legend=TRUE, col = pal)



surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit")
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=0)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=45)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=90)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=135)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=180)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=225)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=270)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=315)
surface(outFGac, type="p", xlab="Growth", ylab="Flowering Day", zlab="RelFit", theta=360)
