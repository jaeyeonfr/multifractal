library(MSM)
OIL = read.csv("WTI.csv")
n = length(OIL$Adj.Close)
OIL_return = diff(log(OIL$Adj.Close),lag=1
fit = Msm(OIL_return, kbar=7, n.vol=252, nw.lag=2)
summary(fit)
#plot(fit)
em = Msm_decompose(fit)
#plot(em)
dates = as.Date(as.POSIXct(OIL$Date[2:n], format="%Y-%m-%d"))

# The original plot function is not good enough so changed it below (still more room for improvement)
plot_Msm_withtime = function(object,timestamp){
  smoothed.p <- Msm_smooth_cpp(object$A,object$filtered)
  pred       <- Msm_predict(object$g.m, object$para[4], object$n, smoothed.p, object$A)
  
  plot.df = data.frame( abs(object$ret),pred$vol,timestamp)
  colnames(plot.df) <- c("Absolute Returns","Conditional Volatility","Date")
  plot.df <- reshape2::melt(plot.df,id = "Date")
  msm_plot <- ggplot2::ggplot(plot.df, aes(x=Date, y=value, colour=variable)) +
    ggplot2::geom_line(alpha=0.7) + ggplot2::xlab("Date") +
    ggplot2::ylab("Volatility") + ggplot2::ggtitle("Conditional Volatility vs Absolute Returns") +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position="bottom")+
    ggplot2::scale_x_date(labels = date_format("%m-%Y"),date_breaks = "3 month")+
    ggplot2::theme(axis.text.x=element_text(angle=60, hjust=1))
  print(msm_plot)
}

plot_Msm_withtime(fit,dates)

# same changes 
plot_msmcomp_withtime <- function(x, timestamp){
  x = data.frame(as.matrix.msmcomp(x),timestamp)
  em.long <- reshape2::melt(x,id = "timestamp")
  
  m.plot <- ggplot2::ggplot(em.long, ggplot2::aes(x=timestamp, y=value)) +
    ggplot2::geom_line() + ggplot2::guides(colour=FALSE) + ggplot2::xlab("Time") +
    ggplot2::ylab("M") + ggplot2::ggtitle("MSM Volatility Components")+
    ggplot2::scale_x_date(labels = date_format("%m-%Y"),date_breaks = "3 month")+
    ggplot2::theme(axis.text.x=element_text(angle=60, hjust=1))
  
  m.plot <- m.plot + ggplot2::aes(colour=factor(variable))
  #m.plot <- m.plot + ggplot2::facet_wrap(~ Var2, ncol=1, scales = "free")
  m.plot <- m.plot + ggplot2::facet_grid(variable~., scale  = "free_y")
  print(m.plot)
  
}

plot_msmcomp_withtime(em,dates)