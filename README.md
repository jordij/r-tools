# R-tools for Marine Debris Tracker data

## About

A set of scripts to generate useful plots and maps for data acquired from the [Marine Debris Tracker](http://www.marinedebris.engr.uga.edu/). Debris data can be downloaded [from the website](http://www.marinedebris.engr.uga.edu/newmap/) or if you're looking for long periods of data using my Python [SAMDI-get script](https://github.com/jordij/samdi-get).

![Observations by type](https://i.imgur.com/qhG4G3q.png)

## Instructions

Download [2009 US County boundaries dataset](ftp://ftp.census.gov/geo/tiger/TIGER2009/tl_2009_us_county.zip) and extract the file within the `data` folder. You should end up with the structure:

`./data/tl_2009_us_county/` and all the shp files etc in there.

Run the script and, hopefully, you'll end up with all the generated assets in the `output` folder.
