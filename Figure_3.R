
library(hexbin)
library(grid)
library(gridBase)
library(fields) # for tim.colors
library(mgcv)
library(lme4)

# plotting data
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Code/Modelling")
load("Hansen_glmer_models.RData")
data <- glmer_mod4@frame

# get predictions to visualise the model within the range 
# of the two variables
rangeFC <- range(data$forest_cover_prop, na.rm=TRUE)
rangeDE <- range(data$sqrt_dist_edge, na.rm=TRUE)

hist(data$sqrt_dist_edge)


# set a little bit of space around the model predictions and
# the resolution of the predictions
res <- 51
seqFC <- seq(rangeFC[1], rangeFC[2], length=res)
seqDE <- seq(rangeDE[1], rangeDE[2], length=res)




# get all combinations for the prediction values 
pred <- expand.grid(forest_cover_prop = seqFC, sqrt_dist_edge = seqDE)
# and get unconditional predictions for each point
pred$pred <- predict(glmer_mod4, newdata=pred, re.form= ~ 0)

# Contour plot of model using hex bin coloured drape using the alpha channel
# to indicate data density as well as showing the model surface.
pdf('Hansen_Figure_2_nonegative.pdf', height=89/25.4, width=89/25.4, pointsize=7, family='Helvetica')
	
	# Empty base graphics plot
	par(mai=c(0.22,0.22,0.05,0.05), mgp=c(0.75, 0.05, 0), tcl=0.2, pch=19, cex=1, cex.axis=5/7, cex.lab=1) 

	plot(seqDE ~ seqFC, type='n', 
		 xlab='Proportion forest cover', ylab='Distance to range edge (km)',
		 yaxt='n')
	yax <- seq(-30, 50, by=10)
	axis(2, at=yax, labels=yax^2 * sign(yax))

	# generate a hexbin plot to show study data distribution
	hb <- with(data, hexbin(forest_cover_prop, sqrt_dist_edge))
	hb_xy <- as.data.frame(hcell2xy(hb))
	names(hb_xy) <- c('forest_cover_prop', 'sqrt_dist_edge')
	hb_xy$pred <- predict(glmer_mod4, newdata=hb_xy, re.form= ~ 0, type='response')
	hb_xy$log2_n <- log2(hb@count)
	
	# colour
	p_range <- with(hb_xy, seq(min(pred), max(pred), length=101))
	cols <- terrain.colors(100, alpha=NULL)
	hb_xy$col <- cols[findInterval(hb_xy$pred, p_range, all.inside=TRUE)]
	
	# add alpha as doubling 
	n_log2_bins <- (0:ceiling(max(hb_xy$log2_n)))
	alpha <- rev(as.hexmode(255 - 20 * n_log2_bins))
	hb_xy$alpha <- alpha[findInterval(hb_xy$log2_n, n_log2_bins, all.inside=TRUE)]
	hb_xy$col <- toupper(with(hb_xy, paste(col, alpha, sep='')))
	
	# setup the plot window as a grid viewport
	vp <- baseViewports()
	pushViewport(vp$plot)
	
	# use low level functions to impose color scale - scaling from inside grid.hexagons
	sx <- hb@xbins/diff(hb@xbnds)
	sy <- (hb@xbins * hb@shape)/diff(hb@ybnds)
	inner <- 0.5
	outer <- (2 * inner)/sqrt(3)
	dx <- inner/sx
	dy <- outer/(2 * sy)
	
	with(hb_xy, hexpolygon(forest_cover_prop, sqrt_dist_edge, dx=dx, dy=dy, fill=col, border=NA))
	popViewport()
	
	prob <- plogis(matrix(pred$pred, ncol=res))
	contour(seqFC, seqDE, prob,  type='n', add=TRUE, lwd=1, labcex=0.75)
	abline(h=34, ## median(th4) ## works on the scale (scale increments are 10)
	       lty=2, lwd=1)
	
dev.off()

### (median(th3)^2)/10 this for small values for the abline

median(th)
median(th2)
median(th3)
median(th4)
mean(th4)

summary(th3)

summary(glmer_mod1)
summary(glmer_mod2)
summary(glmer_mod3)
summary(glmer_mod4)






