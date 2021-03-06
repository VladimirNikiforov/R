################################################################################
#                                                                                
# ikashnitsky.github.io 2017-05-25
# Map urb/rur
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#                                                                                                    
################################################################################

# Erase all objects in memory
rm(list = ls(all = TRUE))

# additional packages
library(tidyverse)
library(ggthemes)
library(rgdal)
library(viridis)
library(extrafont)
myfont <- "Roboto Condensed"

# load the already prepared data
load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/df-27-261-urb-rur.RData"))
load(url("https://ikashnitsky.github.io/doc/misc/map-subplots/spatial-27-261.RData"))


# fortify spatial objects
bord <- fortify(Sborders)
fort <- fortify(Sn2, region = "id")

fort_map <- left_join(df,fort,"id")

# create a blank map
basemap <- ggplot()+
        geom_polygon(data = fortify(Sneighbors),aes(x = long, y = lat, group = group),
                     fill = "grey90",color = "grey90")+
        coord_equal(ylim = c(1350000,5450000), xlim = c(2500000, 6600000))+
        theme_map(base_family = myfont)+
        theme(panel.border = element_rect(color = "black",size = .5,fill = NA),
              legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_rect(colour = NA, fill = NA),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15))+
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        labs(x = NULL, y = NULL)

ggsave(filename = "basemap.png", basemap, width = 5, height = 5)



# create a nice mosaic plot; solution from SO:
# http://stackoverflow.com/a/19252389/4638884
makeplot_mosaic <- function(data, x, y, ...){
        xvar <- deparse(substitute(x))
        yvar <- deparse(substitute(y))
        mydata <- data[c(xvar, yvar)];
        mytable <- table(mydata);
        widths <- c(0, cumsum(apply(mytable, 1, sum)));
        heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});
        
        alldata <- data.frame();
        allnames <- data.frame();
        for(i in 1:nrow(mytable)){
                for(j in 1:ncol(mytable)){
                        alldata <- rbind(alldata, c(widths[i], 
                                                    widths[i+1], 
                                                    heights[j, i], 
                                                    heights[j+1, i]));
                }
        }
        colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
        
        alldata[[xvar]] <- rep(dimnames(mytable)[[1]], 
                               rep(ncol(mytable), nrow(mytable)));
        alldata[[yvar]] <- rep(dimnames(mytable)[[2]], nrow(mytable));
        
        ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) + 
                geom_rect(color="white", aes_string(fill=yvar)) +
                xlab(paste(xvar, "(count)")) + 
                ylab(paste(yvar, "(proportion)"));
}

typ_mosaic <- makeplot_mosaic(data = df %>% mutate(type = as.numeric(type)), 
                              x = subregion, y = type)+
        theme_void()+
        scale_fill_viridis(option = "B", discrete = T, end = .8)+
        scale_y_continuous(limits = c(0, 1.4))+
        annotate("text",x = c(27, 82.5, 186), y = 1.05, 
                 label=c("EAST", "SOUTH", "WEST"), 
                 size = 4, fontface = 2, 
                 vjust = 0.5, hjust = 0,
                 family = myfont) + 
        coord_flip()+
        theme(legend.position = "none")

ggsave(filename = "mosaic.png", typ_mosaic, width = 4, height = 3)

# a nice small function to overcome some mapping problems with nested polygons
# see more at SO
# https://stackoverflow.com/questions/21748852
gghole <- function (fort) {
        poly <- fort[fort$id %in% fort[fort$hole, ]$id, ]
        hole <- fort[!fort$id %in% fort[fort$hole, ]$id, ]
        out <- list(poly, hole)
        names(out) <- c("poly", "hole")
        return(out)
}

# annotate a small map of the subregions of Europe
an_sub <- basemap +
        geom_polygon(data = gghole(fort_map)[[1]], 
                     aes(x = long, y = lat, group = group, fill = subregion),
                     color = NA)+
        geom_polygon(data  =  gghole(fort_map)[[2]], 
                     aes(x = long, y = lat, group = group, fill = subregion),
                     color = NA)+
        scale_fill_manual(values = rev(brbg3)) +
        theme(legend.position = "none")

ggsave(filename = "sub.png", an_sub, width = 4, height = 4)


# finally the map of Urb/Rur typology

caption <- "Classification: De Beer, J., Van Der Gaag, N., & Van Der Erf, R. (2014). New classification of urban and rural NUTS 2 regions in Europe. NIDI Working Papers, 2014/3. Retrieved from http://www.nidi.nl/shared/content/output/papers/nidi-wp-2014-03.pdf
\nIlya Kashnitsky (ikashnitsky.github.io)"

typ <-  basemap + 
        
        geom_polygon(data = gghole(fort_map)[[1]], 
                     aes(x=long, y=lat, group=group, fill=type),
                     color="grey30",size=.1)+
        geom_polygon(data = gghole(fort_map)[[2]], 
                     aes(x=long, y=lat, group=group, fill=type),
                     color="grey30",size=.1)+
        scale_fill_viridis("NEUJOBS\nclassification of\nNUTS-2 regions", 
                           option = "B", discrete = T, end = .8)+
        geom_path(data = bord, aes(x = long, y = lat, group = group),
                  color = "grey20",size = .5) + 
        
        annotation_custom(grob = ggplotGrob(typ_mosaic), 
                          xmin = 2500000, xmax = 4000000, 
                          ymin = 4450000, ymax = 5450000)+
        annotation_custom(grob = ggplotGrob(an_sub), 
                          xmin = 5400000, xmax = 6600000, 
                          ymin = 2950000, ymax = 4150000)+
        labs(title = "Urban / Rural classification of NUTS-2 regions of Europe\n",
             caption = paste(strwrap(caption, width = 95), collapse = '\n'))+
        theme(plot.title = element_text(size = 20),
              plot.caption = element_text(size = 12))

ggsave(filename = "map.png", typ, width = 6.5, height = 8, dpi = 300)
