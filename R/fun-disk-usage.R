#------------------------------------------------------------------------------
# Title:         Function - Disk Usage Monitor
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-05-07 12:29:45 CDT
# Last Modified: 2018-05-18 18:26:13 CDT
#------------------------------------------------------------------------------

# source("D:\\Box Sync\\misc-scripts-r\\misc-prop-bar.R")

getDiskUsage <- function() {
    call <- "wmic logicaldisk get size,freespace,caption"
    disks <- system(call, inter = TRUE)

    disks <- read.fwf(
        textConnection(disks[1:(length(disks)-1)]), 
        widths = c(9, 13, 13), 
        strip.white = TRUE, 
        stringsAsFactors = FALSE
    )

    colnames(disks) <- disks[1, ]
    disks <- disks[-1, ]
    rownames(disks) <- disks[, 1]
    disks <- disks[, -1]

    disks[disks == ""] <- NA
    disks <- disks[complete.cases(disks), ]
    disks$UsedSpace <- as.numeric(disks$Size) - as.numeric(disks$FreeSpace)
    disks <- data.matrix(disks)

    bars <- lapply(seq_len(nrow(disks)), function(i) {
        makeTextBar(disks[i, 3], disks[i, 2], char.rem = "-")
    })

    message(
        paste0(
            "\n",
            "Operating system (OS)... ", Sys.info()["sysname"],  "\n",
            "OS release.............. ", Sys.info()["release"],  "\n",
            "OS version.............. ", Sys.info()["version"],  "\n",
            "User.................... ", Sys.info()["user"],     "\n",
            "\n",
            "Disk usage for machine:  ", Sys.info()["nodename"], "\n"
        )
    )
    for (i in seq_len(nrow(disks))) {
        message(
            paste(
                rownames(disks)[i], bars[i]
            )
        )
    }
    message("\n")
}