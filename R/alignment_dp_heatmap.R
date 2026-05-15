#------------------------------------------------------------------------------
# Title:  Pairwise Alignment Wavefront (WFA) Heatmaps
# Author: Brandon Monier (brandon.monier@sdstate.edu)
# Date:   05.15.26
#
# Generates two side-by-side heatmaps of a pairwise sequence alignment using
# the Wavefront Alignment (WFA) algorithm of Marco-Sola et al. (2020) with
# gap-affine penalties (Gotoh-style three-state wavefronts: M, I, D).
#
# Unlike full Needleman-Wunsch, WFA only computes "wavefronts" of furthest-
# reaching offsets per diagonal at each increasing penalty score s, expanding
# outward until the lower-right corner is reached. The cells actually visited
# by any wavefront form a sparse, band-like region that hugs the optimal
# alignment trace. Each visited cell is annotated with the minimum penalty s
# at which a wavefront first reached it.
#
#   - Left  : every cell touched by some wavefront, colored by penalty s
#   - Right : early wavefronts only (penalty <= threshold), showing the
#             initial expansion before any mismatch / indel is accepted
#------------------------------------------------------------------------------

# Preamble ----

## Load packages
require(ggplot2)
require(patchwork)


# Functions ----

#' Compute a pairwise alignment cell-coverage matrix using gap-affine WFA.
#'
#' Implements the gap-affine Wavefront Alignment algorithm (Marco-Sola, Moure,
#' Moreto & Espinosa, Bioinformatics, 2020) with three per-score wavefronts:
#'   M[s, k]  -- best offset on diagonal k reachable with score s ending in
#'               a match / mismatch state
#'   I[s, k]  -- best offset ending in an insertion (gap in seq_row)
#'   D[s, k]  -- best offset ending in a deletion  (gap in seq_col)
#'
#' Recurrences (penalties: mismatch x, gap_open o, gap_extend e):
#'   I[s, k] = max(M[s - o - e, k - 1], I[s - e, k - 1]) + 1
#'   D[s, k] = max(M[s - o - e, k + 1], D[s - e, k + 1])
#'   M[s, k] = max(M[s - x, k] + 1, I[s, k], D[s, k])
#' followed by an "extend" step that greedily walks matches forward on
#' diagonal k.
#'
#' @param seq_row    reference sequence drawn down the rows
#' @param seq_col    query sequence drawn across the columns
#' @param mismatch   penalty for a mismatched base (default 4)
#' @param gap_open   penalty for opening a new gap   (default 6)
#' @param gap_extend penalty for extending a gap     (default 2)
#'
#' @return integer matrix of size (nchar(seq_row) + 1) x (nchar(seq_col) + 1).
#'         Cells reached by some wavefront contain the minimum penalty s that
#'         first touched them; unvisited cells are NA. The total alignment
#'         penalty is stored in attribute "alignment_score".
compute_wfa_matrix <- function(
    seq_row,
    seq_col,
    mismatch   = 4,
    gap_open   = 6,
    gap_extend = 2
) {
    s1 <- strsplit(seq_row, "")[[1]]
    s2 <- strsplit(seq_col, "")[[1]]
    m  <- length(s1)
    n  <- length(s2)

    target_k      <- n - m
    target_offset <- n

    ## Wavefront storage: list keyed by (s + 1), each entry is a named integer
    ## vector keyed by diagonal index k (character names).
    M_wf <- list()
    I_wf <- list()
    D_wf <- list()

    ## Cell-coverage matrix: minimum penalty at which each cell was reached.
    cells <- matrix(NA_integer_, nrow = m + 1, ncol = n + 1)
    cells[1, 1] <- 0L

    get_wf <- function(wf, s, k) {
        if (s < 0L || s + 1L > length(wf)) return(NA_integer_)
        v <- wf[[s + 1L]]
        if (is.null(v) || length(v) == 0L) return(NA_integer_)
        out <- v[as.character(k)]
        if (is.na(out)) NA_integer_ else as.integer(out)
    }

    set_wf <- function(wf, s, k, val) {
        while (length(wf) < s + 1L) wf[[length(wf) + 1L]] <- integer(0)
        wf[[s + 1L]][as.character(k)] <- as.integer(val)
        wf
    }

    record_cell <- function(i, j, s) {
        if (i < 0L || i > m || j < 0L || j > n) return(invisible())
        cur <- cells[i + 1L, j + 1L]
        if (is.na(cur) || cur > s) {
            cells[i + 1L, j + 1L] <<- as.integer(s)
        }
    }

    ## Greedy match extension on diagonal k starting from offset h; returns
    ## the new (possibly advanced) offset and records every cell traversed.
    extend <- function(h, k, s) {
        i <- h - k
        j <- h
        record_cell(i, j, s)
        while (i < m && j < n && s1[i + 1L] == s2[j + 1L]) {
            i <- i + 1L
            j <- j + 1L
            record_cell(i, j, s)
        }
        j
    }

    safe_max <- function(...) {
        v <- c(...)
        v <- v[!is.na(v)]
        if (length(v) == 0L) NA_integer_ else as.integer(max(v))
    }

    ## Seed wavefront: s = 0, diagonal 0, offset 0 -- extend matches.
    M_wf <- set_wf(M_wf, 0L, 0L, extend(0L, 0L, 0L))

    s          <- 0L
    max_score  <- (mismatch + gap_open + gap_extend) * (m + n) + 10L

    repeat {
        cur <- get_wf(M_wf, s, target_k)
        if (!is.na(cur) && cur >= target_offset) break
        if (s >= max_score) stop("WFA: alignment score exceeded upper bound")
        s <- s + 1L

        ## Diagonal index k can grow by at most 1 per unit score; (-s):s is
        ## a safe (loose) bound.
        for (k in (-s):s) {
            ## I[s, k] = max(M[s - o - e, k - 1], I[s - e, k - 1]) + 1
            iv <- safe_max(
                get_wf(M_wf, s - gap_open - gap_extend, k - 1L),
                get_wf(I_wf, s - gap_extend,            k - 1L)
            )
            if (!is.na(iv)) {
                iv <- iv + 1L
                I_wf <- set_wf(I_wf, s, k, iv)
                record_cell(iv - k, iv, s)
            }

            ## D[s, k] = max(M[s - o - e, k + 1], D[s - e, k + 1])
            dv <- safe_max(
                get_wf(M_wf, s - gap_open - gap_extend, k + 1L),
                get_wf(D_wf, s - gap_extend,            k + 1L)
            )
            if (!is.na(dv)) {
                D_wf <- set_wf(D_wf, s, k, dv)
                record_cell(dv - k, dv, s)
            }

            ## M[s, k] = max(M[s - x, k] + 1, I[s, k], D[s, k]); then extend.
            v_x <- get_wf(M_wf, s - mismatch, k)
            if (!is.na(v_x)) v_x <- v_x + 1L
            mv <- safe_max(v_x, get_wf(I_wf, s, k), get_wf(D_wf, s, k))
            if (!is.na(mv)) {
                M_wf <- set_wf(M_wf, s, k, extend(mv, k, s))
            }
        }
    }

    rownames(cells) <- c("", s1)
    colnames(cells) <- c("", s2)
    attr(cells, "alignment_score") <- s
    cells
}


#' Convert a (possibly sparse) cell-coverage matrix into long format for
#' ggplot2. NA cells are dropped so they render as the panel background.
matrix_to_long <- function(mat) {
    df <- expand.grid(
        row = seq_len(nrow(mat)),
        col = seq_len(ncol(mat))
    )
    df$value     <- as.vector(mat)
    df$row_label <- rownames(mat)[df$row]
    df$col_label <- colnames(mat)[df$col]
    df[!is.na(df$value), , drop = FALSE]
}


#' Plot a wavefront coverage matrix as a heatmap. Visited cells get their
#' penalty score printed inside the tile; unvisited cells stay blank.
#'
#' @param mat        matrix returned from `compute_wfa_matrix`
#' @param threshold  if non-NULL, only cells with penalty <= threshold are drawn
#' @param title      optional plot title
#' @param low_color  fill for low penalty scores
#' @param mid_color  fill for mid-range penalty scores
#' @param high_color fill for high penalty scores
plot_dp_heatmap <- function(
    mat,
    threshold  = NULL,
    title      = NULL,
    low_color  = "#3CC54E",
    mid_color  = "#F2E63D",
    high_color = "#E83A3A"
) {
    df <- matrix_to_long(mat)

    if (!is.null(threshold)) {
        df <- df[df$value <= threshold, , drop = FALSE]
    }

    n_rows  <- nrow(mat)
    n_cols  <- ncol(mat)
    max_val <- max(mat, na.rm = TRUE)
    if (!is.finite(max_val) || max_val <= 0) max_val <- 1

    ggplot(df, aes(x = col, y = row, fill = value)) +
        geom_tile(color = "white", linewidth = 0.4) +
        geom_text(
            aes(label = value),
            size   = 2.4,
            color  = "black",
            family = "mono"
        ) +
        scale_y_reverse(
            breaks = seq_len(n_rows),
            labels = rownames(mat),
            limits = c(n_rows + 0.5, 0.5),
            expand = c(0, 0)
        ) +
        scale_x_continuous(
            breaks   = seq_len(n_cols),
            labels   = colnames(mat),
            limits   = c(0.5, n_cols + 0.5),
            expand   = c(0, 0),
            position = "top"
        ) +
        scale_fill_gradientn(
            colors = c(low_color, mid_color, high_color),
            values = scales::rescale(c(0, max_val / 2, max_val)),
            limits = c(0, max_val)
        ) +
        coord_equal() +
        labs(title = title) +
        theme_void() +
        theme(
            plot.title       = element_text(hjust = 0.5, face = "bold"),
            legend.position  = "none",
            axis.text.x.top  = element_text(
                family = "mono",
                face   = "bold",
                size   = 9,
                margin = margin(b = 2)
            ),
            axis.text.y      = element_text(
                family = "mono",
                face   = "bold",
                size   = 9,
                margin = margin(r = 2),
                hjust  = 0.5
            ),
            axis.ticks       = element_blank(),
            plot.margin      = margin(6, 6, 6, 6)
        )
}


# Workflow ----

## Sequences (top = columns / query, left = rows / reference)
seq_query <- "TCTATACTGCGCGTTTGGAGAAATAAAATAGT"
seq_ref   <- "TCTTACTCGGGGGTGAGAAATCTATTAGT"

## Run WFA and build the cell-coverage matrix
wfa_mat <- compute_wfa_matrix(
    seq_row    = seq_ref,
    seq_col    = seq_query,
    mismatch   = 4,
    gap_open   = 6,
    gap_extend = 2
)

message("WFA alignment penalty: ", attr(wfa_mat, "alignment_score"))

## Full WFA exploration vs. only the early wavefronts
final_score <- attr(wfa_mat, "alignment_score")
p_full   <- plot_dp_heatmap(wfa_mat, threshold = NULL)
p_banded <- plot_dp_heatmap(wfa_mat, threshold = max(2L, final_score %/% 2L))

## Side-by-side layout
combined <- p_full + p_banded + patchwork::plot_layout(widths = c(1, 1))


# Output ----

## Display interactively
print(combined)

## Save to disk (uncomment to write)
# ggsave(
#     filename = "alignment_wfa_heatmap.png",
#     plot     = combined,
#     width    = 14,
#     height   = 7,
#     dpi      = 150,
#     bg       = "white"
# )
