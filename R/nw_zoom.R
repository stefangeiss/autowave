
# *ts* feeds the moving average time series to the function. This is what will be plotted.
# *nw* specifies which news wave should be analyzed, and what the properties of that news wave are. Will determine the center of the time window of the visual inspection.
# *nw.df* specifies the entire data set of all news waves.
# *tframe* specifies how large the four time windows will be around the news wave, starting with the smallest up to the largest. For example, c(365, 730, 1825, 9125) will provide 1-year, 2-year, 5-year, and 25-year time windows, respectively. 

  nw_zoom <- function(ts,nw,nw.df,tframe){
    tsx <- (list.dma[[which(to.lab$topic250==nw$topic250[1])]])
    tsx$date <- as.Date(tsx$day,origin="1784-12-31")
    tsxl <- melt(tsx,measure.vars=c("DMA30", "DMA90", "DMA180", "DMA365", "DMA730", "DMA1825"))
    tf1.start <- min(nw.df$start.date)
    tf1.end <- max(nw.df$end.date)
    tf2.start <- as.Date(nw$start-tframe[4],origin="1784-12-31")
    tf2.end <- as.Date(nw$end+tframe[4],origin="1784-12-31")
    tf3.start <- as.Date(nw$start-tframe[3],origin="1784-12-31")
    tf3.end <- as.Date(nw$end+tframe[3],origin="1784-12-31")
    tf4.start <- as.Date(nw$start-tframe[2],origin="1784-12-31")
    tf4.end <- as.Date(nw$end+tframe[2],origin="1784-12-31")
    tf5.start <- as.Date(nw$start-tframe[1],origin="1784-12-31")
    tf5.end <- as.Date(nw$end+tframe[1],origin="1784-12-31")
    tf6.start <- as.Date(nw$start-100,origin="1784-12-31")
    tf6.end <- as.Date(nw$end+100,origin="1784-12-31")
    gg1 <- ggplot(tsxl) + 
        geom_line(aes(x=date,y=value,group=variable,color=variable,linewidth=variable,linetype=variable)) + 
        geom_rect(data=nw, xmin=nw$start.date, xmax=nw$end.date, ymin=0, ymax=0.20, fill="red", alpha=0.5) + 
        geom_rect(data=nw.df, xmin=tf2.start, xmax=tf2.end, ymin=0, ymax=0.10, fill=NA, color="red", linetype="dashed", size=1) + 
        scale_color_viridis_d()+
        scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed","dashed")) +
        scale_linewidth_manual(values=c(1, 0.5, 0.5, 0.5, 0.5, 0.5)) +
        theme_soft() + xlab("Time") + ylab("News stories per day") + ggtitle(nw$topic250[1])
    gg2 <- ggplot(subset(tsxl,date<tf2.end & date>tf2.start))+
        geom_line(aes(x=date,y=value,group=variable,color=variable,linewidth=variable,linetype=variable)) + 
        geom_rect(data=nw,xmin=nw$start.date,xmax=nw$end.date,ymin=0,ymax=0.20,fill="red",alpha=0.5)+
        geom_rect(data=nw.df,xmin=tf3.start,xmax=tf3.end,ymin=0,ymax=0.10,fill=NA,color="red",linetype="dashed",size=1)+
        scale_color_viridis_d()+
        scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed","dashed")) +
        scale_linewidth_manual(values=c(1, 0.5, 0.5, 0.5, 0.5, 0.5)) +
        theme_soft()+xlab("Time")+ylab("News stories per day")
    gg3 <- ggplot(subset(tsxl,date<tf3.end & date>tf3.start))+
        geom_line(aes(x=date,y=value,group=variable,color=variable,linewidth=variable,linetype=variable)) + 
        geom_rect(data=nw,xmin=nw$start.date,xmax=nw$end.date,ymin=0,ymax=0.20,fill="red",alpha=0.5)+
        geom_rect(data=nw.df,xmin=tf4.start,xmax=tf4.end,ymin=0,ymax=0.10,fill=NA,color="red",linetype="dashed",size=1)+
        scale_color_viridis_d()+
        scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed","dashed")) +
        scale_linewidth_manual(values=c(1, 0.5, 0.5, 0.5, 0.5, 0.5)) +
        theme_soft()+xlab("Time")+ylab("News stories per day")
    gg4 <- ggplot(subset(tsxl,date<tf4.end & date>tf4.start))+
        geom_line(aes(x=date,y=value,group=variable,color=variable,linewidth=variable,linetype=variable)) + 
        geom_rect(data=nw,xmin=nw$start.date,xmax=nw$end.date,ymin=0,ymax=0.20,fill="red",alpha=0.5)+
        geom_rect(data=nw.df,xmin=tf5.start,xmax=tf5.end,ymin=0,ymax=0.10,fill=NA,color="red",linetype="dashed",size=1)+
        scale_color_viridis_d()+
        scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed","dashed")) +
        scale_linewidth_manual(values=c(1, 0.5, 0.5, 0.5, 0.5, 0.5)) +
        theme_soft()+xlab("Time")+ylab("News stories per day")
    gg5 <- ggplot(subset(tsxl,date<tf5.end & date>tf5.start))+
        geom_line(aes(x=date,y=value,group=variable,color=variable,linewidth=variable,linetype=variable)) + 
        geom_rect(data=nw,xmin=nw$start.date,xmax=nw$end.date,ymin=0,ymax=0.20,fill="red",alpha=0.5)+
        geom_rect(data=nw.df,xmin=tf6.start,xmax=tf6.end,ymin=0,ymax=0.10,fill=NA,color="red",linetype="dashed",size=1)+
        scale_color_viridis_d()+
        scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed","dashed")) +
        scale_linewidth_manual(values=c(1, 0.5, 0.5, 0.5, 0.5, 0.5)) +
        theme_soft()+xlab("Time")+ylab("News stories per day")
    gg6 <- ggplot(subset(tsxl,date<tf6.end & date>tf6.start))+
        geom_line(aes(x=date,y=value,group=variable,color=variable,linewidth=variable,linetype=variable)) + 
        geom_rect(data=nw,xmin=nw$start.date,xmax=nw$end.date,ymin=0,ymax=0.20,fill="red",alpha=0.5)+
        scale_color_viridis_d()+
        scale_linetype_manual(values=c("solid","dashed","dashed","dashed","dashed","dashed")) +
        scale_linewidth_manual(values=c(1, 0.5, 0.5, 0.5, 0.5, 0.5)) +
        theme_soft()+xlab("Time")+ylab("News stories per day")	
    gg_all <- grid.arrange(gg1,gg2,gg3,gg4,gg5,gg6,ncol=1)
    return(gg_all)
    }