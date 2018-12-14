#------------------------------------------------------------------------------
# Title:         Make a BUNCH of Avatars...
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2018-03-02 23:21:09 CST
# Last Modified: 2018-03-03 01:04:56 CST
#------------------------------------------------------------------------------

# Source and create directory
setwd("C:/Users/Brandon Monier/Box Sync/misc-scripts-r")
source("misc-avatars-02.R")
dir.create(path = "test-output", showWarnings = FALSE)
setwd("test-output/")

# Generate avatars
av.iter <- function(reps) {
    n <- nchar(reps)
    pb <- progress::progress_bar$new(
        format = "  Generating avatars [:bar] :percent in :elapsed",
        total = reps, clear = FALSE, width= 70
    )
    num <- paste0("%0", n, "d")
    for (i in 1:reps) {
        pb$tick()
        pad <- sprintf(num, as.numeric(i))
        main <- paste("Output", pad)
        file <- paste0("av-out-", pad, ".png")
        Sys.sleep(0.5/reps)
        png(filename = file)
        ran.av(main = main)
        dev.off()
    }
}

i <- 1
# tmp <- c("\\", "|", "/", "-")
tmp <- c(
    "[                 ]",
    "[=                ]",
    "[==               ]",
    "[===              ]",
    "[====             ]",
    "[=====            ]",
    "[======           ]",
    "[=======          ]",
    "[========         ]",
    "[=========        ]",
    "[==========       ]",
    "[===========      ]",
    "[============     ]",
    "[=============    ]",
    "[==============   ]",
    "[===============  ]",
    "[================ ]",
    "[=================]",
    "[================ ]",
    "[===============  ]",
    "[==============   ]",
    "[=============    ]",
    "[============     ]",
    "[===========      ]",
    "[==========       ]",
    "[=========        ]",
    "[========         ]",
    "[=======          ]",
    "[======           ]",
    "[=====            ]",
    "[====             ]",
    "[===              ]",
    "[==               ]",
    "[=                ]",
    "[                 ]"
)


while (i != 0) {
    cat("  ", tmp[i])
    if (i > 34) {
        i = i - 33
    } else {
        i = i+1
    }
    Sys.sleep(0.1)
    cat("\014\n")
}



