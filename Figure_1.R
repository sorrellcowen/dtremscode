
setwd("C:/Users/solco/OneDrive/TFE/FINAL PROJECT WORK/Code/Modelling")

randf <- read.csv("Hansen_Range_Edge_Forest_Cover_Incidence.csv")

library(grid)
library(boot)
library(lme4)

full_width <- 183 / 25.4
fh <- full_width / 2

load("hansen_intraspecific_models.Rdata")

pdf('hansen_Figure_1_part1.pdf', height=fh, width=full_width, pointsize=7, family='Helvetica')

# A matrix of the plots - we have 17 (1 big, one insert, 3*5 small)
layout.matrix <- matrix(c(1, 17,  2, 18,  3,  4,
                          1, 17,  5, 18,  6,  7,
                          1, 17,  8, 18,  9, 10,
                          1, 17, 11, 18, 12, 13,
                          1, 17, 14, 18, 15, 16), byrow=TRUE, ncol=6)

# Main plot setup - big outer margin for annotation and images
# Note that layout change cex values, so set par() after layout
layout(layout.matrix, widths=c(2, 0.1, 1, 0.1, 1, 1))
# Text annotation (cex) is at device set 7pt, as are axis labels, 
# with axis numbering at 5pt. mgp for the axis numbers is negative
# because they are in a smaller font than the line heights, so can
# shift up
par(omi=c(0.22,0.22,0.25,1.05), mgp=c(0.75, 0.05, 0), tcl=0.2, pch=19, cex=1, cex.axis=5/7, cex.lab=1) 

# focal species, studies and colour scheme for panels C, D, E
focal_st <- matrix(studies, ncol=2, byrow=TRUE)
focal_sp <- matrix(sp, ncol=2, byrow=TRUE)[,1]
focal <- data.frame(cbind(focal_sp, focal_st), stringsAsFactors=FALSE)
names(focal) <- c('species', 'site1', 'site2')
focal$short <- c("A. lituratus","C. perspicillata","Ch. villosum","D. rotundus","G. soricina")


focal$angle <- c(0,10,55,0,0)
focal$body_size <- c(25,30,36,28,45)
focal$real_scale <- focal$body_size / max(focal$body_size)
focal$use_scale <- c(0.8,0.65,1,0.6,1)

sp_col <- c(artibeus.lituratus="#FF9933",
            carollia.perspicillata="#669900",
            chiroderma.villosum="#3366FF",
            desmodus.rotundus="#FFCC00",
            glossophaga.soricina="#CC0033")
sp_col_tint <- paste(sp_col, '70', sep='')
names(sp_col_tint) <- names(sp_col)

# Panel A
# - get model data and coefficients from model list
par(mar=c(0,0,0,0))
sp_coefs <- coef(edge.results)
names(sp_coefs) <- c('int', 'slope')
sp_coefs$sp <- rownames(sp_coefs)
sp_coefs$minx <- sapply(edge.results, function(x) min(x$model$sqrt_dist_edge))
sp_coefs$maxx <- sapply(edge.results, function(x) max(x$model$sqrt_dist_edge))
sp_coefs$col <- with(sp_coefs, ifelse(sp %in% names(sp_col), sp_col[sp], 'black'))

mod_data <- lapply(edge.results, function(x) x$call$data)
mod_data_comb <- do.call(rbind, mod_data)
mod_data_comb$Species <- as.character(mod_data_comb$Species)
mod_data_comb$col <- with(mod_data_comb, ifelse(Species %in% names(sp_col), sp_col[Species], 'black'))

# plot the data non-focal species in black, overplotting the coloured points 
# for the focal species after the others
plot(forest_cover_prop ~ sqrt_dist_edge, data=mod_data_comb, col=col, subset=col=='black',
     ylim = c(0,1), xlim = c(-15,55), xaxt="n", xlab="", ylab="")

# generate the predictions from the logit models back transformed into proportions
# split the sp_coefs df to get sets of parameters
sp_coef_list <- split(sp_coefs, sp_coefs$sp)

for(sc in sp_coef_list){
  if(sc$col == 'black'){
    xseq <- seq(sc$minx, sc$maxx, len=30)
    yseq <- inv.logit(sc$int + sc$slope * xseq)
    lines(xseq, yseq)			
  }
}

# same for focal species
points(forest_cover_prop ~ sqrt_dist_edge, data=mod_data_comb, col=col, subset=col!='black')

for(sc in sp_coef_list){
  if(sc$col != 'black'){
    xseq <- seq(sc$minx, sc$maxx, len=30)
    yseq <- inv.logit(sc$int + sc$slope * xseq)
    lines(xseq, yseq, col=sc$col, lwd=2)
  }
}

# labelling
axis(1, at=c(-10,0,10,20,30,40,50), labels =c(-100,0,100,400,900,1600,2500), cex=1)
legend('bottomright', legend=focal$short, col=sp_col, lty=1,lwd=1, cex=5/7, text.font=3, bty='n')
## mtext("a", side=3, cex=8/7, font=2, line=0.5,, at=par('usr')[1], adj=0)

mtext("Species sensitivity", side=2, line=par('mgp')[1], adj=0.5)
mtext(expression('D'[edge]~'(km)') , side=1, line=par('mgp')[1] + 0.3, adj=0.5)
mtext("Multi-species responses", side=3, line=0.5, adj=0.5)


#Panel B - D for focal species and studies

# y limits for prob
#yl <- c(-0.1,1.1)

# spacing for images - get the top left of the images
#omd <- par('omd')
#im_yt <- seq(omd[4], omd[3], length=6)[-6]
#im_yh <- -diff(im_yt)[1]
#im_yc <- im_yt - im_yh / 2
#im_xl <- (omd[2] + 0.02) + (1 - (omd[2] + 0.02)) / 2
#im_xw <- 1 - (omd[2] + 0.02)

#for(idx in seq_len(nrow(focal))){
  
#  this <- as.list(focal[idx,])
  
  # PANEL B
  # get the model data and predictions
#  mod <- edge.results[[this$species]]
#  mod_dat <- mod$call$data
#  mod_dat$pch <- ifelse(mod_dat$PID == this$site1, 0, 
#                        ifelse(mod_dat$PID == this$site2, 2, 19))
#  
#  pred_seq <- seq(min(mod_dat$sqrt_dist_edge), max(mod_dat$sqrt_dist_edge), length=51)
#  pred_dat <- data.frame(sqrt_dist_edge = pred_seq)
#  pred_dat <- cbind(pred_dat, predict(mod, newdata=pred_dat, se.fit=TRUE)[1:2])
#  pred_dat$hi <- with(pred_dat, fit + qt(0.025, mod$df.residual) * se.fit)
#  pred_dat$lo <- with(pred_dat, fit - qt(0.025, mod$df.residual) * se.fit)
#  
#  # blank plot
#  plot(forest_cover_prop ~ sqrt_dist_edge, data=mod_dat, 
#       ylim = yl, xlim = c(-15,50), xaxt='n', yaxt='n', type='n')
#  axis(2, at=c(0,1), las=1, mgp=c(0.75,0.2,0))
#  
#  # model fit
#  with(pred_dat, polygon(c(sqrt_dist_edge, rev(sqrt_dist_edge)), c(inv.logit(lo), rev(inv.logit(hi))), 
#                         col=sp_col_tint[this$species], border=NA))
#  lines(inv.logit(fit) ~ sqrt_dist_edge, data=pred_dat, col=sp_col[this$species], lwd=2)
#  
#  # data
#  points(forest_cover_prop ~ sqrt_dist_edge, data=mod_dat, pch=pch)
#  
#  if(idx == 5){
#    axis(1, at=c(-10,0,10,20,30,40,50), labels =c(-100,0,100,400,900,1600,2500), cex=1)
#    mtext(expression('D'[edge]~'(km)') , side=1, line=par('mgp')[1] + 0.3, adj=0.5)
#  } else if(idx == 1){
#    mtext("b", side=3, cex=8/7, font=2, line=0.5, at=par('usr')[1], adj=0)
#    #legend('topright', legend="b", cex=8/7, text.font=2, bty='n')
#    mtext('Species sensitivity', side=3, line=0.5, adj=0.5)
#  }
#  
#  # PANEL C - site 1 near range edge
#  bin_mod <- list.5sp[[which(sp == this$species & studies == this$site1)]]
#  bin_mod_dat <- model.matrix(bin_mod)
#  
#  # blank plot
#  plot(Incidence ~ forest_cover_prop, data=bin_mod$model, type='n', ylim=yl, xlim=c(0,1), yaxt='n', xaxt='n')
#  axis(2, at=c(0,1), las=1, mgp=c(0.75,0.2,0))
#  
#  # model fit
#  pred_dat <- data.frame(forest_cover_prop = seq(min(bin_mod_dat[,2]), max(bin_mod_dat[,2]), length=51))
#  pred_dat <- cbind(pred_dat, predict(bin_mod, newdata=pred_dat, se.fit=TRUE)[1:2])
#  pred_dat$hi <- with(pred_dat, plogis(fit + qt(0.025, bin_mod$df.residual) * se.fit))
#  pred_dat$lo <- with(pred_dat, plogis(fit - qt(0.025, bin_mod$df.residual) * se.fit))
#  pred_dat$fitr <- with(pred_dat, plogis(fit))
#  
#  with(pred_dat, polygon(c(forest_cover_prop, rev(forest_cover_prop)), c(lo, rev(hi)), 
#                         col=sp_col_tint[this$species], border=NA))
  
#  if(summary(bin_mod)$coef[2,4] <= 0.05){
#    lines(fitr ~ forest_cover_prop, data=pred_dat, col=sp_col[this$species], lwd=2)
#  }
  
  # data
#  points(Incidence ~ forest_cover_prop, data=bin_mod$model, col='#00000070', pch=19)
  
#  if(idx == 5){
#    axis(1, at=c(0,0.5,1), labels=c(0,0.5,1), cex=1)
#    mtext('Forest cover', side=1,  at=par('usr')[2], line=par('mgp')[1], adj=0.5)
#  } else if(idx == 1){
#    # legend('topleft', legend="c", cex=8/7, text.font=2, bty='n')
#    mtext("c", side=3, cex=8/7, font=2, line=0.5, at=par('usr')[1], adj=0)
#    mtext('Incidence', side=3,  at=par('usr')[2], line=0.5, adj=0.5)
#  }
  
  # PANEL D - site 2 far away from range edge
#  bin_mod <- list.5sp[[which(this$species == sp & this$site2 == studies)]]
#  bin_mod_dat <- model.matrix(bin_mod)
#  
  # blank plot
#  plot(Incidence ~ forest_cover_prop, data=bin_mod$model, type='n', ylim=yl, xlim=c(0,1), yaxt='n', xaxt='n')
  
#  pred_dat <- data.frame(forest_cover_prop = seq(min(bin_mod_dat[,2]), max(bin_mod_dat[,2]), length=51))
#  pred_dat <- cbind(pred_dat, predict(bin_mod, newdata=pred_dat, se.fit=TRUE)[1:2])
#  pred_dat$hi <- with(pred_dat, plogis(fit + qt(0.025, bin_mod$df.residual) * se.fit))
#  pred_dat$lo <- with(pred_dat, plogis(fit - qt(0.025, bin_mod$df.residual) * se.fit))
#  pred_dat$fitr <- with(pred_dat, plogis(fit))
  
#  with(pred_dat, polygon(c(forest_cover_prop, rev(forest_cover_prop)), c(lo, rev(hi)), 
#                         col=sp_col_tint[this$species], border=NA))
  
#  if(summary(bin_mod)$coef[2,4] <= 0.05){
#    lines(fitr ~ forest_cover_prop, data=pred_dat, col=sp_col[this$species], lwd=2)
#  }
  
  # data
#  points(Incidence ~ forest_cover_prop, data=bin_mod$model, col='#00000070', pch=19)
  
  # labelling
#  mtext(side=4, this$short, font=3, line=0.3)
#  if(idx == 5){
#    axis(1, at=c(0,0.5,1), labels=c(0,0.5,1), cex=1)
#  } else if(idx == 1){
#    mtext("d", side=3, cex=8/7, font=2, line=0.5,, at=par('usr')[2], adj=1)
    #legend('topleft', legend="d", cex=8/7, text.font=2, bty='n')
#  }
  
#}
  
dev.off()



