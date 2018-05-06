library(akima)
library(dplyr)
library(extrafont)
library(ggmap)
library(ggplot2)
library(lubridate)
library(maps)
library(RColorBrewer)
library(rgdal)
library(scales)
library(sp)
library(tidyr)
library(usmap)
library(tmap)


# Using Gill Sans Nova for plots and maps
# If you need to load your fonts in Windows, uncomment next line
# loadfonts(device = "win")

##################
# Auxiliar methods
##################
ExportPlot <- function(gplot, filename, width=4, height=3, transparent=FALSE, bg="#3d85c6", format="png") {
    # Export plot in PNG
    # Notice that A4: width=11.69, height=8.27
    ifelse(transparent, background <- NA, background <- bg)
    if (format == "tiff") {
      tiff(file = paste('./output/', filename, '_.png', sep=""), width = width, height = height, units="in", res=300, compression = 'lzw')
    }
    else{
      png(file = paste('./output/', filename, '_.png', sep=""), bg=background, width = width * 100, height = height * 100)
    }
    print(gplot)
    dev.off()
}

GetSizeFromQuant <- function(quant) {
    case_when(
        quant < 5 ~ 0.25,
        quant < 10 ~ 0.5,
        quant < 20 ~ 0.75,
        quant < 30 ~ 1,
        quant < 40 ~ 2,
        quant < 50 ~ 3,
        quant < 100 ~ 5,
        quant < 1000 ~ 6,
        quant > 1000  ~ 8,
        quant == NA ~ 0
    )
}


#######################
# Load and "scrub" data
#######################
samdi_df <- read.table(file="./data/samdi_latest_data_2011_2018.csv", header = TRUE, sep = ";", quote='"', fill=TRUE)
# get rid of rows with empty dates
samdi_df <- samdi_df[!(samdi_df$date == ''),]
# separate years and months (R originally reads date as factor)
samdi_df$year <- year(samdi_df$date)
samdi_df$month <- month(samdi_df$date)
# add new $grade colum, Useful for color scales and palettes
beautiful_brakes <-  c(1, 5, 10, 20, 30, 40, 50, 100, 1000, Inf)
quant_beautiful_brakes <-  c(1, 100, 200, 500, 1000, 5000, 20000, 50000, Inf)
beautiful_sizes <- c(0.25, 0.5, 0.75, 1, 2, 3, 5, 6, 8)
samdi_df$grade <- cut(samdi_df$quantity, breaks= beautiful_brakes, right = FALSE)
samdi_df$size <- with(samdi_df, ave(quantity, FUN=GetSizeFromQuant))

# exclude rows from outside continental North America bounding box -136.4,16.8,-59.1,49.6
bbox <- c(-136.4, -59.1,  16.8, 49.61)
us_samdi_df <- samdi_df[(samdi_df$longitude >= bbox[1]  & samdi_df$longitude <= bbox[2] & samdi_df$latitude >= bbox[3] & samdi_df$latitude <= bbox[4] ),]
us_samdi_df_plastic <- subset(us_samdi_df, description=="PLASTIC")
# Reduce plastic levels
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Cigarette lighters/tobacco packaging", "Cigarette or tobacco packaging", "Cigarettes")] <- "Cigarettes"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Aerosol cans", "Aluminum or tin cans")] <- "Cans"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in%  c("Buoys and floats", "Crab/Lobster/Fish trap parts", "Fishing nets", "Fishing lures and lines", "Rope or Net Pieces (non-nylon)")] <- "Fishing gear"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) == "Plastic Bags"] <- "Bags"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) == "Plastic or Foam Fragments"] <- "Fragments"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Plastic Bottle", "Plastic Bottle or Container Caps", "Straws", "Plastic Food Wrappers", "Foam or Plastic Cups", "Plastic Utensils", "Six-pack rings")] <- "Food&Drink"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Styrofoam packaging", "Balloons and/or string", "Personal care products", "Other Plastic Jugs or Containers", "Fireworks", "Toys (plastic)", "Non-food related plastic packaging", "Chemicals and chemical containers", "Rubber Gloves")] <- "Other"


#############
# Print Bar Plots
#############
# some handy vars
years <- levels(factor(samdi_df$year))
types <- levels(factor(samdi_df$description))
years_num <- as.numeric(years)
map_labels <- c("< 5","< 10","< 20","< 30","< 40", "< 50", "< 100", "< 1,000", "> 1,000")
quant_map_labels <- c("< 100","< 200","< 500","< 1000","< 5000", "< 20000", "< 50000", "> 100,000")

desc_palette = c( "#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#FFFF00", "#FF61CC", "#C77CFF")

PrintBarPlots <- function() {
    # count observations plot
    obs_plot <- ggplot(data=us_samdi_df, aes(x=reorder(description,description, function(x) - length(x)), fill=description)) +
        geom_bar(stat="count") +
        xlab("") + ylab("") +
        scale_fill_manual(values = desc_palette) +
        scale_y_continuous(labels = comma) + 
        theme(legend.position="none",
            axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
            axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank())
    
    obs_plastic_plot <- ggplot(data=us_samdi_df_plastic, aes(x=reorder(itemname,itemname, function(x) - length(x)), fill=itemname)) +
      geom_bar(stat="count") +
      xlab("") + ylab("") +
      scale_fill_manual(values = desc_palette) +
      scale_y_continuous(labels = comma) + 
      theme(legend.position="none",
            axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
            axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank())
    
    ExportPlot(obs_plot, "obs_by_type", width=11, height = 6)
    ExportPlot(obs_plastic_plot, "plastic_obs_by_type", width=11, height = 6)

    # quantity by type plot
    q_plot <- ggplot(data=us_samdi_df, aes(x=reorder(description,description, function(x) - length(x)), fill=description)) +
        geom_col(aes(y = quantity)) +
        xlab("") + ylab("") + 
        scale_fill_manual(values = desc_palette) +
        scale_y_continuous(labels = comma) +
        theme(legend.position="none",
          axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
          axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          rect = element_blank())
    plastic_q_plot <- ggplot(data=us_samdi_df_plastic, aes(x=reorder(itemname,itemname, function(x) - length(x)), fill=itemname)) +
      geom_col(aes(y = quantity)) +
      xlab("") + ylab("") + 
      scale_fill_manual(values = desc_palette) +
      scale_y_continuous(labels = comma) +
      theme(legend.position="none",
            axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
            axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank())
    ExportPlot(q_plot, "obs_by_quant", width=11, height = 6)
    ExportPlot(plastic_q_plot, "plastic_obs_by_quant", width=11, height = 6)

    # count observations, per year plot
    obs_plot_yearly <- ggplot(data=us_samdi_df, aes(x=year, fill=description)) +
        xlab("") + ylab("") +
        geom_bar(stat="count") +
        scale_y_continuous(labels = comma) +
        labs(fill="") +
        scale_fill_manual(values = desc_palette) +
        scale_x_continuous("", labels=years, breaks=years_num) +
        theme(axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
            axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank())
    ExportPlot(obs_plot_yearly, "obs_by_type_y", width=11, height = 6)
        
    # quantity by type, per year plot
    q_plot_yearly <- ggplot(data=us_samdi_df, aes(x=year, y=quantity, fill=description)) +
        xlab("") + ylab("") +
        geom_bar(stat="identity") +
        scale_y_continuous(labels = comma) +
        labs(fill="") +
        scale_fill_manual(values = desc_palette) +
        scale_x_continuous("", labels=years, breaks=years_num) +
        theme(axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
            axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank())
    ExportPlot(q_plot_yearly, "obs_by_quant_y", width=11, height = 6)
    
    # count observations, per year plot but in percentage
    obs_plot_yearly_percent <- ggplot(data=us_samdi_df, aes(x=year, fill=description)) +
        xlab("") + ylab("") +
        geom_bar(position="fill") +
        labs(fill="") +
        scale_fill_manual(values = desc_palette) +
        scale_x_continuous("", labels=years, breaks=years_num) +
        scale_y_continuous(labels = scales::percent) +
        theme(axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
            axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank())
    ExportPlot(obs_plot_yearly_percent, "obs_by_quan_y_percent", width=11, height = 6)
  
    # count observations, per year plot but in percentage
    plastic_obs_plot_yearly_percent <- ggplot(data=us_samdi_df_plastic, aes(x=year, fill=itemname)) +
      xlab("") + ylab("") +
      geom_bar(position="fill") +
      labs(fill="") +
      scale_fill_manual(values = desc_palette) +
      scale_x_continuous("", labels=years, breaks=years_num) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.title= element_text(colour = "white", size=22, family="Gill Sans Nova"),
            axis.text = element_text(colour = "white", size=17, family="Gill Sans Nova"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank())
    ExportPlot(plastic_obs_plot_yearly_percent, "plastic_obs_plot_yearly_percent", width=11, height = 6)
    
    
    remove(obs_plot, q_plot, obs_plot_yearly, q_plot_yearly, obs_plot_yearly_percent)

    # time series by type
    i <- 1
    for (type in types) {
        df_type <- subset(us_samdi_df, description == type)
        p <- ggplot(data = df_type, aes(x=as.Date(date), y=quantity)) +
            geom_line(size = 1, colour = desc_palette[i]) +
            scale_x_date(date_labels = "%Y") +
            xlab("") + ylab("") +
            #ggtitle(type) +
            ggtitle("") +
            theme(legend.position="none",
                plot.title = element_text(hjust = 0.5,colour = "white", size=22, family="Gill Sans Nova"),
                axis.title= element_text(colour = "white", size=20, family="Gill Sans Nova"),
                axis.text = element_text(colour = "white", size=16, family="Gill Sans Nova"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                rect = element_blank())
        ExportPlot(p, sprintf("quant_time_series_%s", type) , width = 8, height = 6)
        remove(p)
        i <- i + 1
    }

}

PrintBarPlots()

#############
# Print Maps
#############

PlotDebrisMap <- function(basemap, dataframe, legend_pos="right"){
    gg <- basemap +
        geom_point(data = dataframe, aes(x = longitude, y = latitude, color = description, size=size), alpha=0.9) +
        scale_color_manual(values=desc_palette) + 
        scale_size_area(max_size = 8, breaks=beautiful_sizes, labels=map_labels) +
        theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=22,  family="Gill Sans Nova", colour="white"),
            legend.position=legend_pos,
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            rect = element_blank()) +
        xlab('') +
        ylab('') +
        labs(colour="", size="") +
        guides(fill=guide_legend(ncol=1,bycol=TRUE), size=guide_legend(override.aes=list(colour="white")),
            color=guide_legend(override.aes=list(size=5)))
    return(gg)
}

# world map, full data
world <- map_data("world")
world <- world[world$region != "Antarctica",] # remove antarctica
base_map <- geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region), color=NA, fill="#7f7f7f", size=0.05, alpha=0.4)
base_map <- ggplot() + base_map
world_map <- PlotDebrisMap(base_map, samdi_df)
ExportPlot(world_map, "world_dist", width = 11, height = 8, format="png", transparent=TRUE)
remove(world_map)

states <- map_data("state")

wc_states = c("california", "oregon", "washington", "ne")
ec_states = c("louisiana", "mississippi", "alabama", "tennessee", "indiana",
                "kentucky", "ohio", "west virginia", "pennsylvania", 
                "maine", "new hampshire", "massachusetts", "rhode island", "connecticut",
                "new york", "new jersey", "delaware", "maryland", "virginia", "north carolina",
                "south carolina", "georgia", "florida")
gl_states = c("illinois", "indiana", "michigan", "minnesota", "new york", "ohio", "pennsylvania", "wisconsin")
counties <- map_data("county")

# Prepare counties with fips (needed for merges..)
c_fips <- county.fips
county_fips <- separate(data = c_fips, col = polyname, into = c("region", "subregion"), sep = "\\,")
merged_counties <- left_join(counties, county_fips, by=c("region", "subregion") )
merged_counties$state_fip <- sapply(merged_counties$region, fips)

# ec_counties <- subset(counties, region %in% ec_states)
# wc_counties <- subset(counties, region %in% wc_states)
# gl_counties <- subset(counties, region %in% gl_states)

# Great lakes states dist
gl <- subset(states, region %in% gl_states)
base_map <- ggplot(data = gl) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill="#7f7f7f", size=0.05, alpha=0.4, color="white") + 
    coord_fixed(1.3)
gl_samdi_df <- samdi_df[(samdi_df$longitude >= min(base_map$data$long) & samdi_df$longitude <= max(base_map$data$long) & samdi_df$latitude >= min(base_map$data$lat) & samdi_df$latitude <= max(base_map$data$lat)),]
gl_map <- PlotDebrisMap(base_map, gl_samdi_df, legend_pos = "bottom")
ExportPlot(gl_map, "gl_dist", width = 10, height = 8, format="png", transparent=TRUE)
remove(gl_map)

# West coast states dist
wc <- subset(states, region %in% wc_states)
base_map <- ggplot(data = wc) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill="#7f7f7f", size=0.05, alpha=0.4, color="white") + 
    coord_fixed(1.3)
wc_samdi_df <- samdi_df[(samdi_df$longitude >= min(base_map$data$long) & samdi_df$longitude <= max(base_map$data$long) & samdi_df$latitude >= min(base_map$data$lat) & samdi_df$latitude <= max(base_map$data$lat)),]
wc_map <- PlotDebrisMap(base_map, wc_samdi_df)
ExportPlot(wc_map, "wc_dist", width = 10, height = 8, format="png", transparent=TRUE)
remove(wc_map)

# East coast states dist
ec <- subset(states, region %in% ec_states)
base_map <- ggplot(data = ec) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill="#7f7f7f", size=0.05, alpha=0.4, color="white") + 
    coord_fixed(1.3)
ec_samdi_df <- samdi_df[(samdi_df$longitude >= min(base_map$data$long) & samdi_df$longitude <= max(base_map$data$long) & samdi_df$latitude >= min(base_map$data$lat) & samdi_df$latitude <= max(base_map$data$lat)),]
ec_map <- PlotDebrisMap(base_map, ec_samdi_df)
ExportPlot(ec_map, "ec_dist", width = 8, height = 10, format="png", transparent=TRUE)
remove(ec_map)

# Use tl_2009_us_county to match lat-long obs to county polygons.
# Want the total obs count and total quantity observed per county.
# Note we use tl_2009_us_county as county polygons hug water regions. Depending on the year
# this seems to change. See further details at https://gis.stackexchange.com/questions/9578/2010-census-tiger-county-shp-file-with-defined-coastal-land-area

tl_counties <- readOGR(dsn="data/tl_2009_us_county","tl_2009_us_county")
tl_counties@data$county_fip <- as.character(tl_counties@data$CNTYIDFP)
tl_counties@data$statefp <- as.character(tl_counties@data$STATEFP)
tl_counties@data$name <- as.character(tl_counties@data$NAME)
spdf <- SpatialPointsDataFrame(coords=subset(samdi_df, select=c("longitude", "latitude")), data = samdi_df) 
proj4string(spdf) <- proj4string(tl_counties)
spdf$county_name <- over(spdf, tl_counties)$name
spdf$county_fip <- over(spdf, tl_counties)$county_fip
by_county <- group_by(as.data.frame(spdf), county_fip)
counts <- summarise(by_county, count = n(), sum(quantity))
colnames(counts) <- c("county_fip", "count", "quantity")
tl_counties$quantity <- 0

# do counts and sum quantities using country fip as main id
for (row in 1:nrow(counts)) {
    c_ns <- counts[row, "county_fip"]
    q <- as.numeric(counts[row, "quantity"][1])
    tl_counties$quantity[as.character(c_ns) == as.character(tl_counties@data$county_fip)] <- q
}
counties_quantity <- data.frame(state_fip=tl_counties@data$statefp, 
                        county_name=tolower(tl_counties@data$name), 
                        fips=as.character(tl_counties@data$county_fip),
                        quantity=tl_counties@data$quantity, stringsAsFactors=FALSE)
counties_quantity$fips <- as.numeric(counties_quantity$fips)
# bring back all new info to merged_counties
merged_counties_q <- left_join(merged_counties, counties_quantity, by = "fips")

# GL quantity heatmap
gl_counties <- subset(merged_counties_q, region %in% gl_states)
gl_counties <- gl_counties %>% mutate(quantity = if_else(is.na(quantity), 0, quantity))
gl_counties$grade <- cut(gl_counties$quantity, breaks= quant_beautiful_brakes, right = FALSE)

gl_hm <- ggplot(data = gl) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill="#7f7f7f", size=0.05, alpha=0.4, color="white") +
    geom_polygon(data=gl_counties, aes(long, lat,group = group, fill=grade), alpha=0.7, color="white") + 
    scale_fill_brewer(palette="YlOrRd", labels=quant_map_labels, name="") +
    coord_fixed(1.4) +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
        legend.position="right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

ExportPlot(gl_hm, "gl_hm", width = 10, height = 6, format="png", transparent=FALSE)
remove(gl_hm)

# WC quantity heatmap
wc_counties <- subset(merged_counties_q, region %in% wc_states)
wc_counties <- wc_counties %>% mutate(quantity = if_else(is.na(quantity), 0, quantity))
wc_counties$grade <- cut(wc_counties$quantity, breaks= quant_beautiful_brakes, right = FALSE)

wc_hm <- ggplot(data = wc) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill="#7f7f7f", size=0.05, alpha=0.4, color="white") +
    geom_polygon(data=wc_counties, aes(long, lat,group = group, fill=grade), alpha=0.7, color="white") + 
    scale_fill_brewer(palette="YlOrRd", labels=quant_map_labels, name="") +
    coord_fixed(1.3) +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
        legend.position="right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

ExportPlot(wc_hm, "wc_hm", width = 6, height = 10, format="png", transparent=FALSE)
remove(wc_hm)

# EC quantity heatmap
ec_counties <- subset(merged_counties_q, region %in% ec_states)
ec_counties <- ec_counties %>% mutate(quantity = if_else(is.na(quantity), 0, quantity))
ec_counties$grade <- cut(ec_counties$quantity, breaks= quant_beautiful_brakes, right = FALSE)

ec_hm <- ggplot(data = ec) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill="#7f7f7f", size=0.05, alpha=0.4, color="white") +
    geom_polygon(data=ec_counties, aes(long, lat,group = group, fill=grade), alpha=0.7, color="white") + 
    scale_fill_brewer(palette="YlOrRd", labels=quant_map_labels, name="") +
    coord_fixed(1.3) +
    xlab("") +
    ylab("") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=20,  family="Gill Sans Nova", colour="white"),
        legend.position="right",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank())

ExportPlot(ec_hm, "ec_hm", width = 10, height = 6, format="png", transparent=FALSE)
remove(ec_hm)
