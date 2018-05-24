##################
# Auxiliar methods
##################
ExportPlot <- function(gplot, output="./output/", filename="untitled", width=4, height=3, transparent=FALSE, bg="#ffffff", format="png") {
    # Notice that A4: width=11.69, height=8.27
    ifelse(transparent, background <- NA, background <- bg)
    if (format == "tiff") {
      tiff(file = paste(output, filename, '_.tiff', sep=""), width = width, height = height, units="in", res=300, compression = 'lzw')
    }
    else{
      png(file = paste(output, filename, '_.png', sep=""), bg=background, width = width * 100, height = height * 100)
    }
    print(gplot)
    dev.off()
    remove(gplot)
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

#############
# Print Plots
#############

PrintLinePlot <- function(pdata, pxlab, pylab, pname, pwidth=10, pheight=6, ptitle="", 
        plcolor="#FF5733", pcolor="#000000", ptitlesize=22, ptextsize=17, pfamily="serif", 
        ptype="count", plegend="none", pbg="white", pformat="png") {
    if (ptype == "count") {
        plot <-  ggplot(pdata, aes(x=year, size=1)) + 
            geom_line(stat="count", colour=plcolor)
    } else if(ptype == "quantity") {
        plot <-  ggplot(pdata, aes(x=year, y=quantity, size=1)) + 
            stat_summary(fun.y=sum, na.rm=TRUE, color=plcolor, geom ='line')
    }
    plot <- plot +
        scale_x_continuous(breaks=c(2011:2017)) +
        ggtitle(ptitle)  +
        xlab(pxlab) + 
        ylab(pylab) +
        scale_y_continuous(labels=comma) +
        theme(legend.position=ifelse(plegend == "none", "none", "right"),
            plot.title=element_text(hjust = 0.5),
            axis.title.x=element_text(margin=margin(t=10, r=0, b=0, l=0), colour=pcolor, size=ptextsize, family=pfamily),
            axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0), colour=pcolor, size=ptextsize, family=pfamily),
            axis.text=element_text(colour=pcolor, size=ptextsize-2 , family=pfamily),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            text=element_text(size=ptitlesize, family=pfamily, colour=pcolor),
            axis.line=element_blank(),
            axis.ticks=element_blank(),
            rect=element_blank())
    # export to png
    ExportPlot(plot, filename=pname, width=pwidth, height=pheight, bg=pbg, format=pformat)
}

PrintBarPlots <- function(ptitle, pname, pwidth, pheight, pdata, pfill, ptype, ppalette, 
        ptitlesize=22, ptextsize=17, pfamily="serif", pxlab="", pylab="", pscalex="", 
        pscaley="comma", pcolor="#000000", plegend="none", pposlegend="none", mcolor=NA, pbg="#ffffff") {
    # type of main plot
    if (pfill == "description") {
        plot <- ggplot(data=pdata, aes(x=reorder(description, description, function(x) - length(x)), fill=description))
    } else if (pfill == "itemname") {
        plot <- ggplot(data=pdata, aes(x=reorder(itemname, itemname, function(x) - length(x)), fill=itemname))
    } else if (pfill == "years") {
        plot <- ggplot(data=pdata, aes(x=year, fill=description))
    } else if (pfill == "quantyears") {
        plot <- ggplot(data=pdata, aes(x=year, y=quantity, fill=description))
    } else if (pfill == "percentyears") {
        plot <- ggplot(data=pdata, aes(x=year, fill=itemname))
    } else if (pfill == "date") {
        plot <- ggplot(pdata, aes(x=as.Date(date), y=quantity))
    }
    # by number of obs (count) or quantity
    if (ptype == "col") {
        plot <- plot + geom_col(aes(y=quantity))
    } else if (ptype == "count") {
        plot <- plot + geom_bar(stat="count")
    } else if (ptype == "identity") {
        plot <- plot + geom_bar(stat="identity") 
    } else if (ptype == "fill") {
        plot <- plot + geom_bar(position="fill")
    } else if (ptype == "line") {
        plot <- plot + geom_line(size = 1, colour=mcolor)
    }

    # y scale format
    if (pscaley == "comma") {
        plot <- plot + scale_y_continuous(labels=comma)
    } else if (pscaley == "percent") {
        plot <- plot + scale_y_continuous(labels=scales::percent)
    }
    # x scale format
    if (pscalex == "years") {
        plot <- plot + scale_x_continuous("", labels=years, breaks=years_num)
    } else if (pscalex == "date") {
        plot <- plot + scale_x_date(date_labels = "%Y") 
    }
    # rest of stuff, fonts, background etc
    plot <- plot +
        ggtitle(ptitle) +
        xlab(pxlab) + 
        ylab(pylab) +
        scale_fill_manual(values = ppalette) + 
        theme(legend.position=ifelse(pposlegend == "none", "none", "right"),
            legend.spacing = unit(1,"cm"),
            plot.title=element_text(hjust = 0.5),
            axis.title.x=element_text(margin=margin(t=20, r=0, b=0, l=0), colour=pcolor, size=ptextsize, family=pfamily),
            axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0), colour=pcolor, size=ptextsize, family=pfamily),
            axis.text=element_text(colour=pcolor, size=ptextsize, family=pfamily),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            text=element_text(size=ptextsize, family="Gill Sans Nova", colour=pcolor),
            axis.line=element_blank(),
            axis.ticks=element_blank(),
            rect=element_blank())
    if (plegend != "none") {
        plot <- plot + labs(fill=plegend)
    } 
    # export to png
    ExportPlot(plot, filename=pname, width=pwidth, height=pheight, bg=pbg, format="png")
}


#############
# Print Maps
#############

PlotDebrisMap <- function(basemap, dataframe, title="", legend_pos="right", pcolor="#000000"){
    gg <- basemap +
        geom_point(data=dataframe, aes(x=longitude, y=latitude, color=description, size=size), alpha=0.6) +
        scale_color_manual(values=desc_palette) + 
        scale_size_area(max_size=8, breaks=beautiful_sizes, labels=map_labels) +
        theme(plot.title=element_text(hjust = 0.5),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            text=element_text(size=22,  family="Gill Sans Nova", colour=pcolor),
            legend.position=legend_pos,
            legend.spacing = unit(1,"cm"),
            axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            rect=element_blank()) +
        xlab('') +
        ylab('') +
        ggtitle(title) +
        labs(colour="", size="") +
        guides(fill=guide_legend(ncol=1, bycol=TRUE), 
            size=guide_legend(override.aes=list(colour=pcolor)),
            color=guide_legend(override.aes=list(size=5)))
    return(gg)
}

PrintCountiesOrStatesMap <- function (us_states, states, name, width, height, bg="#ffffff", title="", legendpos="bottom") {
    subset_counties <- subset(us_states, region %in% states)
    gg <- ggplot(data=subset_counties) + 
        geom_polygon(aes(x = long, y = lat, group = group), fill="#7f7f7f", size=0.05, alpha=0.4, color="white") + 
        coord_fixed(1.3)
    subset_samdi_df <- samdi_df[(samdi_df$longitude >= min(gg$data$long) & samdi_df$longitude <= max(gg$data$long) & samdi_df$latitude >= min(gg$data$lat) & samdi_df$latitude <= max(gg$data$lat)),]
    gg_map <- PlotDebrisMap(gg, subset_samdi_df, title=title, legend_pos=legendpos)
    ExportPlot(gg_map, filename=name, width=width, height=height, format="png", bg=bg)
}

PrintHeatMap <- function(states, sub_states, counties, pfilename, pwidth, pheight, ppalette, plabels,
                    pfont="serif", pfillcolor="#7f7f7f", ptextcolor="#000000",
                    pcolor="#ffffff", pposlegend="right", pbg="#ffffff", ptitle="") {
    subset_counties <- subset(states, region %in% sub_states)
    gg <- ggplot(data=subset_counties) + 
        geom_polygon(aes(x=long, y=lat, group=group), fill="#7f7f7f", size=0.05, alpha=0.7, color=pcolor) +
        geom_polygon(data=counties, aes(long, lat, group=group, fill=grade), alpha=0.8, color=pcolor) + 
        scale_fill_brewer(palette=ppalette, labels=plabels, name="", drop=FALSE, na.value=pfillcolor) +
        coord_fixed(1.4) +
        xlab("") +
        ylab("") +
        ggtitle(ptitle) +
        theme(plot.title=element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            text=element_text(size=20,  family=pfont, colour=ptextcolor),
            legend.position=pposlegend,
            axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            rect=element_blank())
    ExportPlot(gg, filename=pfilename, width=pwidth, height=pheight, bg=pbg, format="png")
}