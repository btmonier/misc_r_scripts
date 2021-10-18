


urlMaster <- "https://api.bitbucket.org/2.0/repositories/tasseladmin/tassel-5-standalone/src/master/"
pageLen <- 100
urlLib <- paste0(urlMaster, "/lib?pagelen=", pageLen)


jsonMaster <- jsonlite::fromJSON(urlMaster)
jsonLib <- jsonlite::fromJSON(urlLib)


urlLibDF <- jsonLib$values[, c("path", "size", "attributes")]
urlMasterDF <- jsonMaster$values[, c("path", "size", "attributes")]


urlReturn <- rbind(
    urlLibDF,
    urlMasterDF[urlMasterDF$path == "sTASSEL.jar", ]
)
