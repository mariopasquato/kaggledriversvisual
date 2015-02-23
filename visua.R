
filter.window <- c(0.075, 0.225,0.4,0.225, 0.075) #filter for the coordinates
halflength.filter.window <- (length(filter.window)-1)/2

prefix <- "" #directory where the diver directories are

colorscheme <- heat.colors #function to generate colors to plot the trip

smoothing.filter <- function(x)
{ #smoothing filter I apply to the positions
  xf <- as.vector(filter(x, filter.window))
  l <- length(x)
  xf[1:halflength.filter.window] <- x[1:halflength.filter.window] #we don't want to lose the first and last two records, replace from original   
  xf[(l-halflength.filter.window+1):l] <- x[(l-halflength.filter.window+1):l]	 
  xf
}

dritrip <- function(driver, trip)
{ #reads a file given a driver and trip number.
  dt <- read.csv(paste(prefix, driver, "/", trip, ".csv", sep = ""))
  dt$x <- smoothing.filter(dt$x)
  dt$y <- smoothing.filter(dt$y)
  dt	
}

plot.trip <- function(driver, trip)
{
	dt <- dritrip(driver,trip)
	l <- length(dt$x)
	plot(dt, xlab = "x", ylab = "y", type = "l", main = paste("Driver/trip: ", driver, "/", trip, ", Duration = ", l, " s", sep = ""))
	segments(x0 = dt$x[1:(l-1)], y0 = dt$y[1:(l-1)], x1 = dt$x[2:l], y1 = dt$y[2:l], col = colorscheme(l-1))
}

plot.driver <- function(driver, xlim, ylim, trips)
{
	plot(xlim, ylim, xlim = xlim, ylim = ylim, xlab = "x", ylab = "y", type = "n", main = paste("Driver: ", driver, sep = ""))

	plot.thistrip <- function(trip)
	{
		dt <- dritrip(driver,trip)
		l <- length(dt$x)
		segments(x0 = dt$x[1:(l-1)], y0 = dt$y[1:(l-1)], x1 = dt$x[2:l], y1 = dt$y[2:l], col = colorscheme(l-1))
	}
	sapply(trips, plot.thistrip)
}

pdf("visua.pdf")
plot.trip(1,1)
plot.driver(1, c(-10000, 10000), c(-10000, 10000), 1:20)
