#------------------------------------------------------------------------------
# Title:         Function - Caesar Cipher
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-05-18 12:55:20 CDT
# Last Modified: 2018-05-18 19:55:14 CDT
#------------------------------------------------------------------------------

encryptCaesar <- function(x, shift = 3) {

    if (shift > 26) {
        stop("Cipher shifts can only be less than or equal to 26.")
    } else if (length(x) > 1) {
        x <- paste(x, collapse = "")
    } else if (!grepl("^[ A-Za-z]+$", x)) {
        message("Removing special characters and numbers...")
        x <- gsub("[[:punct:][:digit:]]+", "", x)
    } 

    # Convert letters to numerical representations
    x1 <- toupper(x)
    x1 <- unlist(strsplit(x1, split = ""))
    x1 <- match(x1, LETTERS)

    # The Caesar shift
    x1 <- (x1 + shift) %% 27

    # Attribute back to letters
    x1 <- LETTERS[x1]
    x1[is.na(x1)] <- " "
    x1 <- paste(x1, collapse = "")

    return(x1)
}


decryptCaesar <- function(x, key = NULL) {
    if (length(x) > 1) {
        stop("Please only use vector of length 1.")
    } else if (!grepl("^[ A-Za-z]+$", x)) {
        message("Removing special characters...\n\n")
        x <- gsub("[[:punct:][:digit:]]+", "", x)
    } 

    # Convert letters to numerical representations
    x1 <- toupper(x)
    x1 <- unlist(strsplit(x1, split = ""))
    x1 <- match(x1, LETTERS)

    # Decrypt
    if (is.null(key)) {
        if (length(x1) >= 100) {
            x1 <- x1[1:30]
            message("Brute force decrypting...")
            pboptions(style = 3, char = ":")
            decrypt_list <- pbapply::pblapply(seq_len(26), function(i) {
                x1 <- (x1 - i) %% 27
                x1 <- LETTERS[x1]
                x1[is.na(x1)] <- " "
                x1 <- paste(x1, collapse = "")
                x1 <- paste0(x1, "...")
            })
            message("")
            names(decrypt_list) <- paste0(
                "decryption_shift_", 
                sprintf("%02d", 1:length(decrypt_list))
            )
            x1 <- decrypt_list[nchar(decrypt_list) == length(x1) + 3]
        } else {
            message("Brute force decrypting...")
            pboptions(style = 3, char = ":")
            decrypt_list <- pbapply::pblapply(seq_len(26), function(i) {
                x1 <- (x1 - i) %% 27
                x1 <- LETTERS[x1]
                x1[is.na(x1)] <- " "
                x1 <- paste(x1, collapse = "")
            })
            names(x1) <- paste0(
                "decryption_shift_", 
                sprintf("%02d", 1:length(x1))
            )
            x1 <- decrypt_list[nchar(decrypt_list) == nchar(x)]            
        }
        return(x1)
    } else if (!is.numeric(key)) {
        stop("Cipher key can only be numeric.")
    } else {
        x1 <- (x1 - key) %% 27
        x1 <- LETTERS[x1]
        x1[is.na(x1)] <- " "
        x1 <- paste(x1, collapse = "")
        return(x1)
    }
}