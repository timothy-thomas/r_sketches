data.loess <- loess(qsec~wt*hp, data = mtcars)

xgrid <- seq(min(mtcars$wt),
             max(mtcars$wt),
			 (max(mtcars$wt)-min(mtcars$wt))/25)
ygrid <- seq(min(mtcars$hp),
             max(mtcars$hp),
			 (max(mtcars$hp)-min(mtcars$hp))/25)

data.fit <- expand.grid(wt = xgrid, hp = ygrid)

my.matrix <- predict(data.loess, newdata = data.fit)

contour(x = xgrid, y = ygrid, z = my.matrix, xlab = "Weight (x1000lbs)", ylab="Horsepower")