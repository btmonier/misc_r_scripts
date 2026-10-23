# library(cli)


#' @return A data.frame
getCommitMetaData <- function() {
    urlMaster <- "https://api.bitbucket.org/2.0/repositories/tasseladmin/tassel-5-standalone/src/master/"
    pageLen <- 100
    urlLib <- paste0(urlMaster, "/lib?pagelen=", pageLen)

    cli::cli_progress_step("Downloading JSON info")
    jsonTop <- jsonlite::fromJSON(urlMaster)
    jsonLib <- jsonlite::fromJSON(urlLib)

    jsonTop$values$attributes[sapply(jsonTop$values$attributes, is.null)] <- NA
    jsonLib$values$attributes[sapply(jsonLib$values$attributes, is.null)] <- NA


    cli::cli_progress_step("Collecting metdata")
    urlLibDF <- data.frame(
        jar        = jsonLib$values$path,
        attribute  = unlist(jsonLib$values$attributes),
        commitMeta = jsonLib$values$commit$links$self$href,
        downLink   = jsonLib$values$links$self$href
    )
    urlTopDF <- data.frame(
        jar        = jsonTop$values$path,
        attribute  = unlist(jsonTop$values$attributes),
        commitMeta = jsonTop$values$commit$links$self$href,
        downLink   = jsonTop$values$links$self$href
    )

    cli::cli_progress_step("Cleaning up data")
    urlLibDF <- urlLibDF[!grepl(".ini$|.dylib$", urlLibDF$jar), ]
    urlTopDF <- urlTopDF[urlTopDF$jar == "sTASSEL.jar", ]
    urlMasterDf <- rbind(urlTopDF, urlLibDF)
    urlMasterDf$commitDate <- as.Date("")

    cli::cli_progress_step("Collecting commit history")
    j <- 1
    for (i in urlMasterDf$commitMeta) {
        tmpJson <- jsonlite::fromJSON(i)
        urlMasterDf$commitDate[j] <- as.Date(tmpJson$date)
        j <- j + 1
    }

    rownames(urlMasterDf) <- NULL

    return(urlMasterDf)
}


initializeJars <- function(root = "inst/") {

    md <- getCommitMetaData()

    j <- 1
    cli::cli_progress_bar("Downloading jar files", total = nrow(md), type = "download")
    for (i in md$downLink) {
        dest <- paste0(root, md$jar[j])
        download.file(i, dest, quiet = TRUE)
        cli::cli_progress_update()
        j <- j + 1
    }
    cli::cli_alert_success("Downloaded {nrow(md)} jar files.")
}















