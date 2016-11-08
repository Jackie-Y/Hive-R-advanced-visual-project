### STAT 480 Project
# package
inst_pkgs = load_pkgs =  c("ggplot2", "ggmap")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)
library(openintro)


## map of average depdelay per state
ave_orig <- read.csv("~/1A-STAT 480/Project/ave_orig.csv", header=TRUE)
colnames(ave_orig)= c('state','ave_delay','counts')
# transfer abbreviation to full names
ave_orig$region=tolower(abbr2state(ave_orig$state)) 
head(ave_orig[order(-ave_orig$counts),])

# merge with map data
map =map_data("state")
total= merge(ave_orig, map, by ='region',all.y=T)

plot.base=function(total, fillby, darkcol,title){
  retplot=ggplot(total)+
  geom_map(aes(map_id = region), map = total, fill = "black", color = "grey20", size = 0.25)+
  
  expand_limits(x = total$long, y = total$lat)+
  
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))+

  geom_polygon(data = total, aes(x = long, y = lat, group = group,
                                    fill = fillby, col =c(as.character(fillby)) ), color = "white") +
  scale_fill_continuous(low = "thistle2", high = darkcol, guide="colorbar") +
  coord_map()+
  ggtitle(title)
  return (retplot)
}

## plot for delayed flight pre state  
basemap_orig = plot.base(total, total$counts, "darkblue","Count of Depdelay per Origin State")
print(basemap_orig)



### airport
airport <- read.csv("~/1A-STAT 480/Project/ORIG_DEST1.csv")
head(airport)
colnames(airport)=c('depdelay','orig','dest','orig_lat','orig_long','dest_lat','dest_long')

plot(ecdf(airport$depdelay))

plotdata =



## some of the coding refer to the plot.tweet function written by James Balamuta, April 2015.

plot.route = function(basemap, points,linecol){
    retmap = basemap +
    # Add orig points
    # geom_point(data = points, aes(x = x_orig, y = y_orig), size = 1.5, alpha = 1/5, color = "darkblue") +
    # Add orig-dest segment
    geom_segment(data = points, aes(x = x_orig, y = y_orig, xend = x_dest, yend = y_dest),
                     col= linecol, size= 0.72,  arrow = arrow(length = unit(0.35,"cm")) ) +
    # Add dest airport infomation
    geom_text(data = points, aes(x = x_dest+0.35, y = y_dest+0.35, label= dest),col ='black', size =5)
  # Graph ggplot2 object
  return(retmap)
}

head(route_n)
route_n <- read.csv("~/1A-STAT 480/Project/ROUTE_n.csv")
colnames(route_n)=c('origin','dest','orig_lat','orig_long','dest_lat','dest_long','counts','ave_delay')
route_n = route_n[1:50,]
route_n_points = data.frame(x_orig = as.numeric(route_n$orig_long), y_orig = as.numeric(route_n$orig_lat),
                            x_dest = as.numeric(route_n$dest_long), y_dest = as.numeric(route_n$dest_lat),
                            orig = route_n$origin, dest = route_n$dest)

basemap_orig = plot.base(total, total$counts, "skyblue","50 Most Frequent and 25 worst delayed Air Routes")
map1=plot.route(basemap_orig, route_n_points, 'gray55')
print(map1)


# 25 worst delay route
route_ave =route_n[order(-route_n$ave_delay),]
route_ave = route_ave[1:25,]
route_ave_points = data.frame(x_orig = as.numeric(route_ave$orig_long), y_orig = as.numeric(route_ave$orig_lat),
                            x_dest = as.numeric(route_ave$dest_long), y_dest = as.numeric(route_ave$dest_lat),
                            orig = route_ave$origin, dest = route_ave$dest)

map2=plot.route(map1, route_ave_points, 'darkred')
print(map2)


