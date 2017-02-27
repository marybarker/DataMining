#Data Mining hw 21

source('~/Dropbox/Tarleton/data_mining/generic_functions/dataset_ops.R')
source('~/Dropbox/Tarleton/data_mining/class_notes/outliers.R')
auto <- read.csv("~/Dropbox/Tarleton/data_mining/dfiles/auto_data.csv", 
                  header=F,  na.strings='?', dec='.', strip.white=T)

 
#1. Remove all categorical variables from the Auto MPG data set from the UCI Machine 
#   learning Repository, and then examine this data set for anomalies, using the 
#   following methods for computing outlier scores: 

#    * Distance for k-nearest neighbor, where k = 5

#    * Density

#    * Local Outlier Factor method.

#   For each of these methods, plot the density of the outlier score, and produce 
#   scatterplots with a heat map of the outlier scores. 

#   Do you have any comments on this data set after performing the outlier analysis? 
    cols = c(1, 3, 4, 5, 6)
    combinations <- combn(cols,2)
    name <- names(deleted_auto)
    path='~/Dropbox/Tarleton/data_mining/hw21/'

    for(i in cols){
        auto[,i] <- as.numeric(auto[,i])
    }

    idx = auto[,4] != 'NA'
    deleted_auto <- auto[idx == TRUE,]
    deleted_auto <- standardize(deleted_auto, cols)

    ################################################################
    # knn: 
    ################################################################
    kdist = my.kdist(deleted_auto[,cols],5)
    plot(density(kdist))
    dev.copy(png,'~/Dropbox/Tarleton/data_mining/hw21/density_kdist.png')
    dev.off()

    (1:nrow(deleted_auto))[kdist> 1.5]
    (1:nrow(deleted_auto))[kdist>= 1.25]

    for(i in 1:ncol(combinations)){
        v1 = combinations[1,i]
        v2 = combinations[2,i]
        plots <- ggplot(data=deleted_auto[,cols],
                        aes(x=deleted_auto[,v1],y=deleted_auto[,v2],col=kdist,size=3))+
                        geom_point()+
                        scale_colour_gradientn(colours=c('blue','red'))
                        ggsave(plots, filename=paste(path,'using_kdist',v1,'_',v2,'.png',sep=''))
    }


    ################################################################
    # density:
    ################################################################
    thisdensity = my.density(deleted_auto[,cols],5)

    for(i in 1:ncol(combinations)){
        v1 = combinations[1,i]
        v2 = combinations[2,i]
        plots <- ggplot(data=deleted_auto[,cols],
                        aes(x=deleted_auto[,v1],y=deleted_auto[,v2],col=thisdensity,size=3))+
                        geom_point()+
                        scale_colour_gradientn(colours=c('blue','red'))
                        ggsave(plots, filename=paste(path,'using_density',v1,'_',v2,'.png',sep=''))
    }


    plot(density(thisdensity))
    dev.copy(png,'~/Dropbox/Tarleton/data_mining/hw21/density_density.png')
    dev.off()

    (1:nrow(deleted_auto))[thisdensity>=5]
    (1:nrow(deleted_auto))[thisdensity>=5.5]


    ################################################################
    # local outlier factor:
    ################################################################
    thislof = lofactor(deleted_auto[,cols],k = 5)
    for(i in 1:ncol(combinations)){
        v1 = combinations[1,i]
        v2 = combinations[2,i]
        plots <- ggplot(data=deleted_auto[,cols],
                        aes(x=deleted_auto[,v1],y=deleted_auto[,v2],col=thislof,size=3))+
                        geom_point()+
                        scale_colour_gradientn(colours=c('blue','red'))
                        ggsave(plots, filename=paste(path,'using_lof',v1,'_',v2,'.png',sep=''))
    }

    plot(density(thislof))
    dev.copy(png,'~/Dropbox/Tarleton/data_mining/hw21/density_lof.png')
    dev.off()

    (1:nrow(deleted_auto))[thislof >= 1.25]
    (1:nrow(deleted_auto))[thislof >= 1.5]
    ################################################################


outliers = (thislof>=1.5)
coloring=rep('black',nrow(deleted_auto))
coloring[outliers] <- 1:sum( 1.0 * (outliers))
plot(deleted_auto[,cols], col=coloring)
dev.copy(png, '~/Dropbox/Tarleton/data_mining/hw21/all_plot.png')
dev.off()

