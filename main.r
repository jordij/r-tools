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

source(file="plots.r")

#######################
# Load and "scrub" data
#######################
samdi_df <- read.table(file="./data/samdi_latest_data_2011_2018.csv", header=TRUE, sep=";", quote='"', fill=TRUE)
# get rid of rows with empty dates
samdi_df <- samdi_df[!(samdi_df$date == ''),]
# separate years and months (R originally reads date as factor)
samdi_df$year <- year(samdi_df$date)
samdi_df$month <- month(samdi_df$date)
# add new $grade colum, Useful for color scales and palettes
beautiful_brakes <- c(1, 5, 10, 20, 30, 40, 50, 100, 1000, Inf)
quant_beautiful_brakes <- c(1, 100, 200, 500, 1000, 5000, 20000, 50000, Inf)
beautiful_sizes <- c(0.25, 0.5, 0.75, 1, 2, 3, 5, 6, 8)
samdi_df$grade <- cut(samdi_df$quantity, breaks= beautiful_brakes, right = FALSE)
samdi_df$size <- with(samdi_df, ave(quantity, FUN=GetSizeFromQuant))
# Change description levels to lowercase keeping initial capital letter
levels(samdi_df$description)[levels(samdi_df$description) == "PLASTIC"] <- "Plastic"
levels(samdi_df$description)[levels(samdi_df$description) == "CLOTH"] <- "Cloth"
levels(samdi_df$description)[levels(samdi_df$description) == "FISHING GEAR"] <- "Fishing Gear"
levels(samdi_df$description)[levels(samdi_df$description) == "GLASS"] <- "Glass"
levels(samdi_df$description)[levels(samdi_df$description) == "METAL"] <- "Metal"
levels(samdi_df$description)[levels(samdi_df$description) == "OTHER ITEMS"] <- "Other"
levels(samdi_df$description)[levels(samdi_df$description) == "PAPER & LUMBER"] <- "Paper&Lumber"
levels(samdi_df$description)[levels(samdi_df$description) == "RUBBER"] <- "Rubber"

# exclude rows from outside continental North America bounding box -136.4,16.8,-59.1,49.6
bbox <- c(-136.4, -59.1,  16.8, 49.61)
us_samdi_df <- samdi_df[(samdi_df$longitude >= bbox[1]  & samdi_df$longitude <= bbox[2] & samdi_df$latitude >= bbox[3] & samdi_df$latitude <= bbox[4] ),]
us_samdi_df_plastic <- subset(us_samdi_df, description=="Plastic")
# Reduce plastic levels
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Cigarette lighters/tobacco packaging", "Cigarette or tobacco packaging", "Cigarettes")] <- "Cigarettes"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Aerosol cans", "Aluminum or tin cans")] <- "Cans"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in%  c("Buoys and floats", "Crab/Lobster/Fish trap parts", "Fishing nets", "Fishing lures and lines", "Rope or Net Pieces (non-nylon)")] <- "Fishing Gear"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) == "Plastic Bags"] <- "Bags"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) == "Plastic or Foam Fragments"] <- "Fragments"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Plastic Bottle", "Plastic Bottle or Container Caps", "Straws", "Plastic Food Wrappers", "Foam or Plastic Cups", "Plastic Utensils", "Six-pack rings")] <- "Food&Drink"
levels(us_samdi_df_plastic$itemname)[levels(us_samdi_df_plastic$itemname) %in% c("Styrofoam packaging", "Balloons and/or string", "Personal care products", "Other Plastic Jugs or Containers", "Fireworks", "Toys (plastic)", "Non-food related plastic packaging", "Chemicals and chemical containers", "Rubber Gloves")] <- "Other"

########################
# Print some basic stats
########################

# global
nrow(samdi_df)
sum(samdi_df$quantity)
summarize(group_by(samdi_df, year),
    count=n(),
    mm= mean(quantity), 
    tot= sum(quantity), 
    max = max(quantity, na.rm = TRUE))
# us only
nrow(us_samdi_df)
sum(us_samdi_df$quantity)
summarize(group_by(us_samdi_df, year), 
    count=n(),
    mm= mean(quantity), 
    tot= sum(quantity), 
    max = max(quantity, na.rm = TRUE))
# plastic us only
nrow(us_samdi_df_plastic)
sum(us_samdi_df_plastic$quantity)
summarize(group_by(us_samdi_df_plastic, year),
    count=n(),
    mm= mean(quantity), 
    tot= sum(quantity), 
    max = max(quantity, na.rm = TRUE))

##################
# Print line plots
##################

# n observations, excluding 2018 as not completed year yet
PrintLinePlot(samdi_df[!(samdi_df$year == 2018),], "Years", "Observations", "world_observations", 
    ptitle="World Observations", pfamily="Gill Sans Nova", ptitlesize=30, ptextsize=24)
PrintLinePlot(us_samdi_df[!(us_samdi_df$year == 2018),], "Years", "Observations", "us_observations", 
    ptitle="Observations in the U.S.", pfamily="Gill Sans Nova", ptitlesize=30, ptextsize=24)
PrintLinePlot(us_samdi_df_plastic[!(us_samdi_df_plastic$year == 2018),], "Years", "Plastic Observations", 
    "us_plastic_observations", ptitle="Plastic Observations in the U.S.", plcolor="#FF61CC", pfamily="Gill Sans Nova", ptitlesize=30, ptextsize=24)

# quantity worldwide, excluding 2018 as not completed year yet
PrintLinePlot(samdi_df[!(samdi_df$year == 2018),], "Years", "Quantity", "world_quantity", 
    ptitle="World Quantity", pfamily="Gill Sans Nova", ptype="quantity", ptitlesize=30, ptextsize=24)
PrintLinePlot(us_samdi_df[!(us_samdi_df$year == 2018),], "Years", "Quantity", "us_quantity", 
    ptitle="Quantity in the U.S.", pfamily="Gill Sans Nova", ptype="quantity", ptitlesize=30, ptextsize=24)
PrintLinePlot(us_samdi_df_plastic[!(us_samdi_df_plastic$year == 2018),], "Years", "Plastic Quantity", 
    "us_plastic_quantity", ptitle="Plastic Quantity in the U.S.", plcolor="#FF61CC", pfamily="Gill Sans Nova", ptype="quantity", ptitlesize=30, ptextsize=24)

#################
# Print Bar Plots
#################

# some handy vars first
years <- levels(factor(samdi_df$year))
types <- levels(factor(samdi_df$description))
years_num <- as.numeric(years)
map_labels <- c("< 5","< 10","< 20","< 30","< 40", "< 50", "< 100", "< 1,000", "> 1,000")
quant_map_labels <- c("< 100","< 200","< 500","< 1,000","< 5,000", "< 20,000", "< 50,000", "> 100,000")
desc_palette = c( "#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#FFFF00", "#FF61CC", "#C77CFF")

PrintBarPlots("Observations by Type in the U.S.", "obs_by_type", 12, 6, us_samdi_df, "description", 
    "count", desc_palette, pfamily="Gill Sans Nova", pylab="Observations", ptitlesize=30, ptextsize=24)
PrintBarPlots("Plastic Observations by Type in the U.S.", "plastic_obs_by_type", 12, 6, us_samdi_df_plastic, 
    "itemname", "count", desc_palette, pfamily="Gill Sans Nova", pylab="Observations", ptitlesize=30, ptextsize=24)

PrintBarPlots("Quantity by Type in the U.S.", "obs_by_quant", 12, 6, us_samdi_df, "description", 
    "col", desc_palette, pfamily="Gill Sans Nova", pylab="Quantity", ptitlesize=30, ptextsize=24)
PrintBarPlots("Plastic Observations by Quantity in the U.S.", "plastic_obs_by_quant", 12, 6, us_samdi_df_plastic, 
    "itemname", "col", desc_palette, pfamily="Gill Sans Nova", pylab="Quantity", ptitlesize=30, ptextsize=24)

PrintBarPlots("Yearly Observations by Type in the U.S.", "obs_by_type_yearly", 12, 6, us_samdi_df, 
    "years", "count", desc_palette, pfamily="Gill Sans Nova", pylab="Observations", 
    pscalex="years", plegend="Types", pposlegend="right", ptitlesize=30, ptextsize=24)
PrintBarPlots("Yearly Quantity by Type in the U.S.", "obs_by_quant_yearly", 12, 6, us_samdi_df, 
    "quantyears", "identity", desc_palette, pfamily="Gill Sans Nova", pylab="Quantity", 
    pscalex="years", plegend="Types", pposlegend="right", ptitlesize=30, ptextsize=24)

PrintBarPlots("Yearly Observations by Type in the U.S.", "obs_by_type_yearly_percent", 12, 6, us_samdi_df, 
    "years", "fill", desc_palette, pfamily="Gill Sans Nova", pscalex="years", pscaley="percent",
    plegend="Types", pposlegend="right", ptitlesize=30, ptextsize=24)
PrintBarPlots("Plastic Yearly Observations by Type in the U.S.", "plastic_obs_by_quant_yearly_percent", 12, 6,
    us_samdi_df_plastic, "percentyears", "fill", desc_palette, pfamily="Gill Sans Nova", 
    pscalex="years", pscaley="percent", plegend="Plastic Types", pposlegend="right", ptitlesize=30, ptextsize=24)
    
# Yearly series by type
i <- 1
for (type in types) {
    df_type <- subset(us_samdi_df, description == type)
    PrintBarPlots(type, sprintf("quant_time_series_%s", type), 8, 6, df_type, "date", "line", desc_palette,
     pfamily="Gill Sans Nova", pscalex="date", pscaley="", mcolor=desc_palette[i], ptitlesize=30, ptextsize=24)
    i <- i + 1
}


############
# Print Maps
############

# world map, full dataset
world <- map_data("world")
world <- world[world$region != "Antarctica",] # remove antarctica
base_map <- geom_map(data=world, map=world, aes(x=long, y=lat, map_id=region), color=NA, fill="#3d85c6", size=0.05, alpha=0.5)
base_map <- ggplot() + base_map
world_map <- PlotDebrisMap(base_map, samdi_df, title="World Observations by Type and Quantity")

ExportPlot(world_map, filename="map_world_dist", width=11, height=6, bg="#ffffff", format="png")

# US east coast, west coast and great lakes
states <- map_data("state")
wc_states = c("california", "oregon", "washington", "ne")
ec_states = c("louisiana", "mississippi", "alabama", "tennessee", "indiana",
                "kentucky", "ohio", "west virginia", "pennsylvania", 
                "maine", "new hampshire", "massachusetts", "rhode island", "connecticut",
                "new york", "new jersey", "delaware", "maryland", "virginia", "north carolina",
                "south carolina", "georgia", "florida")
gl_states = c("illinois", "indiana", "michigan", "minnesota", "new york", "ohio", "pennsylvania", "wisconsin")

PrintCountiesOrStatesMap(states, gl_states, "great_lakes_distribution", 10, 8, 
    title="Great Lakes Observations by Type and Quantity")
PrintCountiesOrStatesMap(states, wc_states, "west_coast_distribution", 9, 8,
    title="West Coast Observations by Type and Quantity")
PrintCountiesOrStatesMap(states, ec_states, "east_coast_distribution", 10, 8,
    title="East Coast Observations by Type and Quantity", legendpos="right")

# Prepare counties with fips (needed for merges..)
counties <- map_data("county")
c_fips <- county.fips
county_fips <- separate(data = c_fips, col = polyname, into = c("region", "subregion"), sep = "\\,")
merged_counties <- left_join(counties, county_fips, by=c("region", "subregion") )
merged_counties$state_fip <- sapply(merged_counties$region, fips)


################
# Print HeatMaps
################

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
tl_counties$quantity <- 0  # set all to 0 and then populate

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

# GL counties
gl_counties <- subset(merged_counties_q, region %in% gl_states)
gl_counties <- gl_counties %>% mutate(quantity = if_else(is.na(quantity), 0, quantity))
gl_counties$grade <- cut(gl_counties$quantity, breaks= quant_beautiful_brakes, right = FALSE)
# WC counties
wc_counties <- subset(merged_counties_q, region %in% wc_states)
wc_counties <- wc_counties %>% mutate(quantity = if_else(is.na(quantity), 0, quantity))
wc_counties$grade <- cut(wc_counties$quantity, breaks= quant_beautiful_brakes, right = FALSE)
# EC counties
ec_counties <- subset(merged_counties_q, region %in% ec_states)
ec_counties <- ec_counties %>% mutate(quantity = if_else(is.na(quantity), 0, quantity))
ec_counties$grade <- cut(ec_counties$quantity, breaks= quant_beautiful_brakes, right = FALSE)

PrintHeatMap(states, gl_states, gl_counties, "great_lakes_heatmap", pwidth=12, pheight=6, ppalette="YlOrRd", 
    plabels=quant_map_labels, pfont="Gill Sans Nova", pfillcolor="#7f7f7f", ptitle="Great Lakes Counties by Quantity")

PrintHeatMap(states, wc_states, wc_counties, "west_coast_heatmap", pwidth=12, pheight=6, ppalette="YlOrRd", 
    plabels=quant_map_labels, pfont="Gill Sans Nova", pfillcolor="#7f7f7f", ptitle="West Coast Counties by Quantity")

PrintHeatMap(states, ec_states, ec_counties, "east_coast_heatmap", pwidth=12, pheight=6, ppalette="YlOrRd", 
    plabels=quant_map_labels, pfont="Gill Sans Nova", pfillcolor="#7f7f7f", ptitle="East Coast Counties by Quantity")
