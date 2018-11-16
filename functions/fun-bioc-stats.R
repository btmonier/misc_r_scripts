#------------------------------------------------------------------------------
# Title:         Function - Bioconductor Stats
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-04-20 17:12:35 CDT
# Last Modified: 2018-05-17 09:07:27 CDT
#------------------------------------------------------------------------------

# Necessary packages
# library(ggplot2)
# library(reshape2)
# library(RCurl)

# Function
getBiocStats <- function(pkg) {
    # Contact Bioconductor server
    message("Contacting server...")
    
    # Retrieve data
    url <- "http://bioconductor.org/packages/stats/bioc/%s/%s_stats.tab"
    url <- gsub("%s", pkg, url, fixed = TRUE)

    if (RCurl::url.exists(url)) {
        df <- read.delim(url, header = TRUE)
    } else {
        stop("This Bioconductor package does not exist.")
    }

    # Data munging
    df$Year <- as.factor(df$Year)
    df <- df[!(df$Month == "all"), ]
    df$Month <- factor(df$Month)
    df$Month <- factor(
        df$Month, levels = c(
            "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
        )
    )
    df.m <- reshape2::melt(
        data = df,
        id.vars = c(
            "Year",
            "Month"
        ),
        measure.vars = c(
            "Nb_of_distinct_IPs",
            "Nb_of_downloads"
        )
    )
    df.m <- df.m[order(as.Date(df.m$Month, format="%d/%m/%Y")), ]
    df.m$variable <- gsub("_", " ", df.m$variable)
    df.m$variable <- gsub("Nb", "No.", df.m$variable)

    # Visualization
    ggplot2::ggplot(
        data = df.m, 
        ggplot2::aes(x = Month, y = value, fill = variable)
    ) +
        ggplot2::geom_bar(
            stat = "identity", 
            position = ggplot2::position_dodge()
        ) +
        ggplot2::scale_fill_brewer(palette = "Paired") +
        ggplot2::labs(
            x = "",
            y = "",
            title = paste(pkg, "downloads:", Sys.time())
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "")) +
        ggplot2::facet_grid(Year ~ .) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
    
    # Save image
    message("Saving plot...")
    
    year.l <- length(levels(df$Year))
    if (year.l == 1 | year.l == 2) {
        height <- 5
    } else if (year.l == 3) {
        height <- 7
    } else {
        height <- 8
    }

    ggplot2::ggsave(
        filename = paste0(pkg, "-stats-", Sys.Date(), ".pdf"),
        width = 7,
        height = height,
        units = "in"
    )
    
    message(
        paste0(
            "Plot saved at: ",
            getwd()
        )
    )
}
