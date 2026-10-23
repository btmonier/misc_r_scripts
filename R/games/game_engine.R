#------------------------------------------------------------------------------
# Title:   Console Game Engine
# Author:  Brandon Monier
#
# A tiny roguelike-style navigator. Move the '@' around a walled map, pick up
# gold ('$'), and take the stairs ('>') down to the next level.
#
# Run in a terminal for real-time controls (requires `keypress`):
#     Rscript R/games/game_engine.R
# Anywhere else (e.g. RStudio), type commands and press Enter. Several moves
# can be chained on one line, e.g. "dddsss".
#------------------------------------------------------------------------------

# === Config ========================================================

config <- list(
    width = 40,
    height = 15,
    goldPerLevel = 5,
    wallDensity = 0.18,
    maxWallDensity = 0.35
)

tiles <- list(
    wall = "#",
    floor = ".",
    gold = "$",
    stairs = ">",
    player = "@"
)

dirs <- list(
    up = c(-1, 0),
    down = c(1, 0),
    left = c(0, -1),
    right = c(0, 1)
)

keyMap <- c(
    up = "up", down = "down", left = "left", right = "right",
    w = "up", s = "down", a = "left", d = "right",
    k = "up", j = "down", h = "left", l = "right",
    q = "quit", escape = "quit", `ctrl-c` = "quit"
)


# === Map generation ================================================

## Flood fill from a cell; returns linear indices of reachable cells ----
reachable <- function(map, start) {
    h <- nrow(map)
    seen <- logical(length(map))
    seen[start] <- TRUE
    queue <- start

    while (length(queue)) {
        cur <- queue[1]
        queue <- queue[-1]
        r <- (cur - 1) %% h + 1

        nb <- c(cur - 1, cur + 1, cur - h, cur + h)
        nb <- nb[c(r > 1, r < h, TRUE, TRUE)]
        nb <- nb[nb >= 1 & nb <= length(map)]
        nb <- nb[!seen[nb] & map[nb] != tiles$wall]

        seen[nb] <- TRUE
        queue <- c(queue, nb)
    }

    which(seen)
}


## Build a level with gold and stairs reachable from the start ----
newLevel <- function(level, cfg = config) {
    density <- min(cfg$wallDensity + 0.02 * (level - 1), cfg$maxWallDensity)

    repeat {
        map <- matrix(tiles$floor, cfg$height, cfg$width)
        map[c(1, cfg$height), ] <- tiles$wall
        map[, c(1, cfg$width)] <- tiles$wall

        interior <- which(map == tiles$floor)
        map[interior[runif(length(interior)) < density]] <- tiles$wall

        open <- which(map == tiles$floor)
        start <- open[sample.int(length(open), 1)]
        reach <- setdiff(reachable(map, start), start)

        if (length(reach) >= cfg$goldPerLevel + 1) break
    }

    picks <- reach[sample.int(length(reach), cfg$goldPerLevel + 1)]
    map[picks[1]] <- tiles$stairs
    map[picks[-1]] <- tiles$gold

    pos <- arrayInd(start, dim(map))
    list(map = map, py = pos[1], px = pos[2])
}


# === Game state ====================================================

enterLevel <- function(state, level) {
    lvl <- newLevel(level)
    state$level <- level
    state$map <- lvl$map
    state$py <- lvl$py
    state$px <- lvl$px
    state
}

newGame <- function() {
    state <- list(
        gold = 0,
        turns = 0,
        msg = "Collect the gold ($) and find the stairs (>).",
        running = TRUE
    )
    enterLevel(state, 1)
}


# === Rendering =====================================================

clearScreen <- function() {
    if (Sys.getenv("RSTUDIO") == "1") {
        cat("\014")
    } else if (isatty(stdout())) {
        cat("\033[2J\033[H")
    }
}

render <- function(state) {
    clearScreen()
    view <- state$map
    view[state$py, state$px] <- tiles$player

    cat(apply(view, 1, paste, collapse = ""), sep = "\n")
    cat(sprintf(
        "Level: %d   Gold: %d   Turns: %d\n",
        state$level, state$gold, state$turns
    ))
    cat("Move: arrows / wasd / hjkl   Quit: q\n")
    cat(state$msg, "\n", sep = "")
}


# === Input =========================================================

## Pick the best available input method ----
makeReader <- function() {
    hasKeypress <- requireNamespace("keypress", quietly = TRUE) &&
        keypress::has_keypress_support()

    if (hasKeypress) {
        return(list(read = function() keypress::keypress(), realtime = TRUE))
    }

    if (interactive()) {
        return(list(read = function() readline("> "), realtime = FALSE))
    }

    con <- file("stdin")
    open(con)
    read <- function() {
        cat("> ")
        line <- readLines(con, n = 1)
        if (length(line) == 0) "q" else line
    }
    list(read = read, realtime = FALSE)
}


## Translate a key (or a line of typed keys) into commands ----
parseKey <- function(key) {
    key <- tolower(trimws(key))
    if (key %in% names(keyMap)) return(unname(keyMap[key]))

    chars <- strsplit(key, "")[[1]]
    if (!length(chars)) return("none")

    cmds <- unname(keyMap[chars])
    cmds[is.na(cmds)] <- "none"
    cmds
}


# === Game logic ====================================================

step <- function(state, cmd) {
    if (cmd == "quit") {
        state$running <- FALSE
        return(state)
    }
    if (cmd == "none") {
        state$msg <- "Unknown key. Use arrows / wasd / hjkl, or q to quit."
        return(state)
    }

    d <- dirs[[cmd]]
    ny <- state$py + d[1]
    nx <- state$px + d[2]
    tile <- state$map[ny, nx]

    if (tile == tiles$wall) {
        state$msg <- "You bump into a wall."
        return(state)
    }

    state$py <- ny
    state$px <- nx
    state$turns <- state$turns + 1
    state$msg <- ""

    if (tile == tiles$gold) {
        amount <- sample(5:15, 1) * state$level
        state$gold <- state$gold + amount
        state$map[ny, nx] <- tiles$floor
        state$msg <- sprintf("You pick up %d gold.", amount)
    } else if (tile == tiles$stairs) {
        state <- enterLevel(state, state$level + 1)
        state$msg <- sprintf("You descend to level %d.", state$level)
    }

    state
}


# === Main ==========================================================

titleScreen <- function(reader) {
    clearScreen()
    cat(
        "+---+---+---+---+---+---+---+\n",
        "| R |   | R | O | G | U | E |\n",
        "+---+---+---+---+---+---+---+\n",
        "\n by Brandon Monier\n\n",
        sep = ""
    )
    if (reader$realtime) {
        cat("Press any key to begin...\n")
    } else {
        cat("Press Enter to begin...\n")
    }
    reader$read()
}

main <- function() {
    reader <- makeReader()
    titleScreen(reader)
    state <- newGame()

    while (state$running) {
        render(state)
        for (cmd in parseKey(reader$read())) {
            state <- step(state, cmd)
            if (!state$running) break
        }
    }

    clearScreen()
    cat(sprintf(
        "Thanks for playing!\n\nDeepest level: %d\nGold: %d\nTurns: %d\n",
        state$level, state$gold, state$turns
    ))
    invisible(state)
}

main()
