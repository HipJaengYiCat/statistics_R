# Visualization of Multivariate Data
# install.packages("HSAUR2")
library("HSAUR2")
data("USairpollution")
X=as.matrix(USairpollution) 

var.names=names(USairpollution)
city.names=abbreviate(row.names(USairpollution),4) 
# if it is a matrix, use rownames(X)

# (1) scatter plot

# 2-d scatter plot
win.graph() # X11() in mac
attach(USairpollution)
plot(popul, SO2, xlab="popul", ylab="SO2", col="brown")

# label the scatter by unit names.
win.graph()
plot(popul, SO2, xlab="popul", ylab="SO2", type="n")
text(popul,SO2, labels=row.names(USairpollution),cex=0.6)
rug(popul, side=1, col="red")
rug(SO2, side=2, col="green")

# (2) box plot
boxplot(USairpollution)
boxplot(USairpollution[,1:2])
boxplot(USairpollution[,3:4])

# bivariate boxplot
# install.packages("MVA") 
library("MVA")
win.graph()
bvbox(USairpollution[,c(4,1)], xlab="popul",ylab="SO2",main="Bivariate Boxplot")
lab=c("Chicago","Providence","Philadelphia","Detroit")
outcity=match(lab, rownames(USairpollution))
text(X[outcity, 4], X[outcity,1], labels=lab, cex=0.7)

# chi-plot is used to test independence of two variables
win.graph()
chiplot(popul,SO2)

# bubble plot
win.graph()
plot(popul, SO2, xlab="popul", ylab="SO2",type="n",main="bubbles: temp")
symbols(popul, SO2, circles=temp,inches=0.2, add=TRUE)

# star plot
win.graph()
palette(rainbow(20,s=0.7,v=0.9))
stars(USairpollution, len=0.8, cex=0.55, key.loc=c(15,1.6),
      main="star plot", draw.segments=TRUE)

# pairwise scatter plot
win.graph()
pairs(USairpollution, pch=".",cex=2) 

# 
win.graph()
panel.hist <- function(x, ...)
     {
         usr <- par("usr"); on.exit(par(usr))
         par(usr = c(usr[1:2], 0, 1.5) )
         h <- hist(x, plot = FALSE)
         breaks <- h$breaks; nB <- length(breaks)
         y <- h$counts; y <- y/max(y)
         rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
     }
panel.reg<-function(x,y,...)
    {     
         points(x, y) #pch=".",cex=1.5)
         abline(lm(y ~ x,...), col = "red")  
    }
pairs(USairpollution, panel=panel.reg, cex.labels=1, font.labels=1, diag.panel=panel.hist)

# 3-d scatter plot
library("scatterplot3d")
win.graph()
scatterplot3d(temp,wind, SO2, type="h", angle=55)

# trellis graphics
plot(xyplot(SO2 ~temp| cut(wind,2)))


# statlactite plot
stalac(USairpollution)
