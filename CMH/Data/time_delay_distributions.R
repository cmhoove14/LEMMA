# Parameters
# Mostly code from Graham and Sean early covid ABM model development

# -------------------------------------
# Incubation period 
# -------------------------------------
library(fitdistrplus)

weibull_shapes <- c()
weibull_scales <- c()
lnorm_means <- c()
lnorm_sds <- c()
gamma_shapes <- c()
gamma_scales <- c()

###Draw from estimated distributions
for (i in 1:1000){
  backer <- rweibull(88,3.04,8.344)
  tindale1 <- rweibull(93,1.88,7.97)
  tindale2 <- rweibull(135,1.88,7.97)
  lauer <- rlnorm(181,1.621,0.418)
  li <- rlnorm(425,1.65,0.533)
  linton <- rlnorm(158,1.722,1.03)
  
  times <- c(backer,tindale1,tindale2,lauer,li,linton)
  #hist(times, breaks = 50)
  fw <- fitdist(times, "weibull")
  fl <- fitdist(times, "lnorm")
  fg <- fitdist(times, "gamma")
  weibull_shapes <- c(weibull_shapes,fw$estimate[1])
  weibull_scales <- c(weibull_scales,fw$estimate[2])
  lnorm_means <- c(lnorm_means, fl$estimate[1])
  lnorm_sds <- c(lnorm_sds, fl$estimate[2])
  gamma_shapes <- c(gamma_shapes, fg$estimate[1])
  gamma_scales <- c(gamma_scales, fg$estimate[2])
}

###Get mean parameters
w_shape <- mean(weibull_shapes)
shape_CI <- c(quantile(weibull_shapes,.025),quantile(weibull_shapes,.975))

w_scale <- mean(weibull_scales)
scale_CI <- c(quantile(weibull_scales,.025),quantile(weibull_scales,.975))

l_mean <- mean(lnorm_means)
mean_CI <- c(quantile(lnorm_means,.025),quantile(lnorm_means,.975))

l_dev <- mean(lnorm_sds)
dev_CI <- c(quantile(lnorm_sds,.025),quantile(lnorm_sds,.975))

g_shape <- mean(gamma_shapes)
g_shape_CI <- c(quantile(gamma_shapes, 0.025), quantile(gamma_shapes, 0.975))

g_scale <- mean(gamma_scales)
g_scale_CI <- c(quantile(gamma_scales, 0.025), quantile(gamma_scales, 0.975))

###Plot them, if you want
xs<- seq(0,21,0.1)
w_line <- dweibull(xs,w_shape,w_scale)
l_line <- dlnorm(xs,l_mean,l_dev)
g_line <- dgamma(xs, g_shape, g_scale)

plot(x=xs,y=w_line,type = 'l',ann=F,axes=F,xlim=c(0,21),ylim=c(0,0.15),col="#E69F00",lwd=2) 
lines(xs,l_line,col="#56B4E9",lwd=2)
lines(xs,g_line,col="chartreuse3",lwd=2)
axis(side=1,at=seq(0,21,5),lwd=1,lwd.ticks=0.5,cex.axis=0.8)
axis(side=2,at=seq(0,0.15,0.05),las=1,cex.axis=0.8,lwd.ticks=0.5,lwd=1)
mtext(side=2,line=2,'Density',cex=1)
mtext(side=1,line=2,'Duration from Infection to Symptoms',cex=1)
mtext(side=3,line=0,'Estimated Distribution of Incubation Periods',cex=1)
legend(11,0.14,
       c(paste('Weibull(',round(w_shape,2),',', round(w_scale,2),')'),
         paste('LogNorm(',round(l_mean,2),',', round(l_dev,2),')'),
         paste('Gamma(', round(g_shape,2), ',', round(g_scale,2),')')),
       lty=c(1,1,1),lwd=c(2,2),col=c("#E69F00","#56B4E9","chartreuse3"),
       cex=0.65, bty = "n")

t_i_s_gamma_pars <- c(g_shape, g_scale)

save(t_i_s_gamma_pars,
     file = "CMH/Data/t_i_s_gamma.Rdata")

# -------------------------------------
# Symptom onset to hospitalization
# -------------------------------------

# Linton et al data https://doi.org/10.3390/jcm9020538
linton_etal <- read.csv("CMH/Data/Linton_etal_time_scales.csv", skip = 1)

  linton_t_s_h <- as.numeric(as.Date(linton_etal$DateHospitalizedIsolated, format = "%m/%d/%Y") - 
    as.Date(linton_etal$Onset, format = "%m/%d/%Y"))
  
# Sanche et al https://doi.org/10.3201/eid2607.200282
sanche_etal <- readxl::read_xlsx("CMH/Data/20-0282-techapp1.xlsx",
                                 sheet = 1, skip = 4)

  sanche_t_s_h <- as.numeric(as.Date(sanche_etal$`Hospitalization date`) - 
                               as.Date(sanche_etal$`Onset date`))

# Thompson et al https://dx.doi.org/10.3390%2Fjcm9051297  
thomp_etal <- read.csv("CMH/Data/jcm-756735-supplementary.csv")

  thomp_t_s_h <- as.numeric(as.Date(thomp_etal$Date.of.hospitalisation, format = "%m/%d/%Y") - 
                               as.Date(thomp_etal$Date.of.symptom.onset, format = "%m/%d/%Y"))

t_s_h_all <- c(linton_t_s_h, sanche_t_s_h, thomp_t_s_h)
t_s_h_all <- t_s_h_all[!is.na(t_s_h_all)]
# -------------------------------------
# Infection to hospitalization as combination of incubation period and time to hospitalization
# -------------------------------------
weibull_shapes2 <- c()
weibull_scales2 <- c()
lnorm_means2 <- c()
lnorm_sds2 <- c()
gamma_shapes2 <- c()
gamma_scales2 <- c()
  
for (i in 1:1000){
  incubation <- rgamma(200, g_shape, g_scale)
  hosp <- sample(t_s_h_all,
                 size = 200, replace = T)
  
  t_i_h <- incubation+hosp

  fw <- fitdist(t_i_h, "weibull")
  fl <- fitdist(t_i_h, "lnorm")
  fg <- fitdist(t_i_h, "gamma")
  weibull_shapes2 <- c(weibull_shapes2,fw$estimate[1])
  weibull_scales2 <- c(weibull_scales2,fw$estimate[2])
  lnorm_means2 <- c(lnorm_means2, fl$estimate[1])
  lnorm_sds2 <- c(lnorm_sds2, fl$estimate[2])
  gamma_shapes2 <- c(gamma_shapes2, fg$estimate[1])
  gamma_scales2 <- c(gamma_scales2, fg$estimate[2])
}

###Plot them, if you want
xs2<- seq(0,30,0.1)
w_line2 <- dweibull(xs2,mean(weibull_shapes2),mean(weibull_scales2))
l_line2 <- dlnorm(xs2,mean(lnorm_means2),mean(lnorm_sds2))
g_line2 <- dgamma(xs2, mean(gamma_shapes2), mean(gamma_scales2))

plot(x=xs2,y=w_line2,type = 'l',ann=F,axes=F,xlim=c(0,30),ylim=c(0,0.15),col="#E69F00",lwd=2) 
lines(xs2,l_line2,col="#56B4E9",lwd=2)
lines(xs2,g_line2,col="chartreuse3",lwd=2)
axis(side=1,at=seq(0,30,5),lwd=1,lwd.ticks=0.5,cex.axis=0.8)
axis(side=2,at=seq(0,0.15,0.05),las=1,cex.axis=0.8,lwd.ticks=0.5,lwd=1)
mtext(side=2,line=2,'Density',cex=1)
mtext(side=1,line=2,'Duration from Infection to Hospitalizaion',cex=1)
mtext(side=3,line=0,'Estimated Distribution of Infection to Hospitalization Periods',cex=1)
legend(11,0.14,
       c(paste('Weibull(',round(w_shape,2),',', round(w_scale,2),')'),
         paste('LogNorm(',round(l_mean,2),',', round(l_dev,2),')'),
         paste('Gamma(', round(g_shape,2), ',', round(g_scale,2),')')),
       lty=c(1,1,1),lwd=c(2,2),col=c("#E69F00","#56B4E9","chartreuse3"),
       cex=0.65, bty = "n")

t_i_h_gamma_pars <- c(mean(gamma_shapes2), mean(gamma_scales2))

save(t_i_h_gamma_pars,
     file = "CMH/Data/t_i_h_gamma.Rdata")

