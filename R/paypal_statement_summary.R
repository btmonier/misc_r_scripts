#------------------------------------------------------------------------------
# Title:         PayPal PDF Statement Summary
# Author:        Brandon Monier (brandon.monier@sdstate.edu)
# Created:       2026-06-02
# Description:   Read a directory of PayPal account-activity PDF statements
#                and print (optionally export) summary metrics.
# Usage:         Rscript paypal_statement_summary.R --dir "C:/path/to/pdfs"
# Dependencies:  pdftools (and stringr recommended)
#------------------------------------------------------------------------------

# === Preamble ================================================================

suppressPackageStartupMessages({
    if (!requireNamespace("pdftools", quietly = TRUE)) {
        stop(
            "Package 'pdftools' is required. Install with: install.packages('pdftools')",
            call. = FALSE
        )
    }
    if (!requireNamespace("stringr", quietly = TRUE)) {
        stop(
            "Package 'stringr' is required. Install with: install.packages('stringr')",
            call. = FALSE
        )
    }
})

library(pdftools)
library(stringr)

# === Parsing helpers =========================================================

#' Lines that end with currency + amount + fees + total (transaction row).
TX_AMOUNT_LINE <- paste0(
    "\\s+(", "[A-Z]{3}", ")\\s+",
    "([-+]?\\d+\\.\\d{2})\\s+",
    "([-+]?\\d+\\.\\d{2})\\s+",
    "([-+]?\\d+\\.\\d{2})\\s*$"
)

DATE_PREFIX <- "^\\s*(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s*(.*)$"
STATEMENT_PERIOD <- paste0(
    "Statement Period\\s+PayPal Account ID\\s+",
    "([\\s\\S]*?)\\s+",
    "([\\w.+-]+@[\\w.-]+)"
)
ID_LINE <- "^\\s*ID:\\s*([A-Z0-9]+)\\s*$"

#' PayPal PDF lines for linked bank/card funding, not transaction parties.
is_funding_detail_line <- function(x) {
    str_detect(
        x,
        paste0(
            "Checking\\s+x-", "|",
            "^\\s*USD\\s*$", "|",
            "^\\s*\\d+\\.\\d{2}\\s+USD\\s*$"
        )
    )
}

normalize_pdf_lines <- function(text) {
    str_trim(str_split(text, "\n")[[1]])
}

parse_statement_header <- function(lines) {
    full <- paste(lines, collapse = "\n")

    account_holder <- NA_character_
    holder_idx <- which(str_detect(lines, "^ACCOUNT STATEMENTS$"))
    if (length(holder_idx) == 1L && holder_idx < length(lines)) {
        account_holder <- lines[holder_idx + 1L]
    }

    period <- NA_character_
    account_id <- NA_character_
    m <- str_match(full, STATEMENT_PERIOD)
    if (!is.na(m[1, 1])) {
        period <- str_squish(m[1, 2])
        account_id <- m[1, 3]
    }

    period_start <- NA_character_
    period_end <- NA_character_
    if (!is.na(period)) {
        period_bits <- str_match(
            period,
            "^([A-Za-z]+\\s+\\d{1,2},\\s+\\d{4})\\s*-\\s*(.+)$"
        )
        if (!is.na(period_bits[1, 1])) {
            period_start <- period_bits[1, 2]
            period_end <- str_squish(period_bits[1, 3])
        }
    }

    list(
        account_holder = account_holder,
        period_label = period,
        period_start = period_start,
        period_end = period_end,
        account_id = account_id
    )
}

#' Merge PDF line-wrap artifacts (split dates / descriptions).
repair_wrapped_lines <- function(lines) {
    if (length(lines) == 0L) {
        return(lines)
    }

    activity_idx <- which(str_detect(lines, "^ACCOUNT ACTIVITY$"))
    if (length(activity_idx) != 1L) {
        return(lines)
    }

    body <- lines[(activity_idx + 1L):length(lines)]
    footer_idx <- which(str_detect(body, "^\\*For each transaction"))
    if (length(footer_idx) >= 1L) {
        body <- body[seq_len(footer_idx[1L] - 1L)]
    }

    # Drop column header row
    body <- body[!str_detect(body, "^DATE\\s+DESCRIPTION")]
    body <- body[nzchar(body)]

    out <- character()
    i <- 1L
    while (i <= length(body)) {
        line <- body[i]
        # Date year split onto next line only when this line is not already a full row.
        if (
            str_detect(line, "\\d{2}/\\d{2}/\\d{3}\\s") &&
            !str_detect(line, TX_AMOUNT_LINE) &&
            i < length(body)
        ) {
            next_line <- body[i + 1L]
            year_fix <- str_match(next_line, "^\\s*(\\d)\\s+(.*)$")
            if (!is.na(year_fix[1, 1])) {
                line <- str_squish(paste0(
                    str_trim(str_remove(line, "\\s+$")),
                    year_fix[1, 2],
                    " ",
                    year_fix[1, 3]
                ))
                i <- i + 2L
                out <- c(out, line)
                next
            }
        }
        out <- c(out, line)
        i <- i + 1L
    }
    out
}

parse_transaction_amount_line <- function(line) {
    m <- str_match(line, TX_AMOUNT_LINE)
    if (is.na(m[1, 1])) {
        return(NULL)
    }

    left <- str_trim(str_remove(line, TX_AMOUNT_LINE))
    date <- NA_character_
    description <- left

    dm <- str_match(left, DATE_PREFIX)
    if (!is.na(dm[1, 1])) {
        date <- dm[1, 2]
        description <- str_trim(dm[1, 3])
    }

    list(
        date = date,
        description = description,
        currency = m[1, 2],
        amount = as.numeric(m[1, 3]),
        fees = as.numeric(m[1, 4]),
        total = as.numeric(m[1, 5])
    )
}

parse_transactions <- function(lines) {
    lines <- repair_wrapped_lines(lines)
    amount_line_idx <- which(str_detect(lines, TX_AMOUNT_LINE))

    if (length(amount_line_idx) == 0L) {
        return(data.frame(
            date = character(),
            description = character(),
            merchant = character(),
            transaction_id = character(),
            currency = character(),
            amount = numeric(),
            fees = numeric(),
            total = numeric(),
            stringsAsFactors = FALSE
        ))
    }

    rows <- lapply(seq_along(amount_line_idx), function(j) {
        idx <- amount_line_idx[j]
        core <- parse_transaction_amount_line(lines[idx])
        if (is.null(core)) {
            return(NULL)
        }

        next_idx <- if (j < length(amount_line_idx)) {
            amount_line_idx[j + 1L] - 1L
        } else {
            length(lines)
        }
        detail_lines <- if (idx < next_idx) lines[(idx + 1L):next_idx] else character()

        # Year digit wrapped to next line: "04/24/202" + "6  Depot"
        if (
            !is.na(core$date) &&
            str_detect(core$date, "\\d{2}/\\d{2}/\\d{3}$") &&
            length(detail_lines) > 0L
        ) {
            year_cont <- str_match(detail_lines[1L], "^\\s*(\\d)\\s+(.*)$")
            if (!is.na(year_cont[1, 1])) {
                core$date <- paste0(core$date, year_cont[1, 2])
                detail_lines[1L] <- str_trim(year_cont[1, 3])
                if (!nzchar(detail_lines[1L])) {
                    detail_lines <- detail_lines[-1L]
                }
            }
        }

        merchant <- NA_character_
        txn_id <- NA_character_
        for (dl in detail_lines) {
            idm <- str_match(dl, ID_LINE)
            if (!is.na(idm[1, 1])) {
                txn_id <- idm[1, 2]
            } else if (!is_funding_detail_line(dl)) {
                if (is.na(merchant)) {
                    merchant <- str_trim(dl)
                } else {
                    merchant <- paste(merchant, str_trim(dl))
                }
            }
        }

        if (is.na(merchant) || !nzchar(merchant)) {
            cand <- detail_lines[!str_detect(detail_lines, ID_LINE)]
            cand <- cand[!str_detect(cand, "SIBLEY STATE BANK|Checking x-")]
            cand <- cand[!str_detect(cand, "^\\d+\\.\\d{2}\\s+USD$")]
            if (length(cand) > 0L) {
                merchant <- str_trim(cand[1])
            }
        }

        # Description often includes a truncated merchant name before the wrap.
        desc_suffix <- str_match(core$description, "Payment:\\s*(.+)$")[, 2]
        if (!is.na(desc_suffix) && nzchar(str_trim(desc_suffix)) && !is.na(merchant)) {
            merchant <- str_squish(paste(str_trim(desc_suffix), merchant))
        }

        payment_type <- str_match(core$description, "^([^:]+):")[, 2]
        if (is.na(payment_type)) {
            payment_type <- core$description
        }

        data.frame(
            date = core$date,
            payment_type = str_trim(payment_type),
            description = core$description,
            merchant = merchant,
            transaction_id = txn_id,
            currency = core$currency,
            amount = core$amount,
            fees = core$fees,
            total = core$total,
            stringsAsFactors = FALSE
        )
    })

    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (length(rows) == 0L) {
        return(parse_transactions(character()))
    }
    do.call(rbind, rows)
}

read_paypal_statement <- function(pdf_path) {
    text <- pdf_text(pdf_path)
    lines <- normalize_pdf_lines(paste(text, collapse = "\n"))
    header <- parse_statement_header(lines)
    transactions <- parse_transactions(lines)

    c(
        list(
            source_file = basename(pdf_path),
            source_path = normalizePath(pdf_path, winslash = "/", mustWork = TRUE)
        ),
        header,
        list(transactions = transactions)
    )
}

# === Summaries ===============================================================

summarize_transactions <- function(tx, statement_meta) {
    if (nrow(tx) == 0L) {
        return(data.frame(
            source_file = statement_meta$source_file,
            period_label = statement_meta$period_label,
            account_id = statement_meta$account_id,
            n_transactions = 0L,
            total_credits = 0,
            total_debits = 0,
            total_fees = 0,
            net_total = 0,
            stringsAsFactors = FALSE
        ))
    }

    credits <- tx$total[tx$total > 0]
    debits <- tx$total[tx$total < 0]

    data.frame(
        source_file = statement_meta$source_file,
        period_label = statement_meta$period_label,
        account_id = statement_meta$account_id,
        n_transactions = nrow(tx),
        total_credits = sum(credits, na.rm = TRUE),
        total_debits = sum(debits, na.rm = TRUE),
        total_fees = sum(tx$fees, na.rm = TRUE),
        net_total = sum(tx$total, na.rm = TRUE),
        stringsAsFactors = FALSE
    )
}

summarize_by_payment_type <- function(tx) {
    if (nrow(tx) == 0L) {
        return(data.frame())
    }
    agg <- aggregate(
        list(total = tx$total, fees = tx$fees),
        by = list(payment_type = tx$payment_type),
        FUN = sum
    )
    agg$n_transactions <- as.integer(tabulate(match(tx$payment_type, agg$payment_type)))
    agg[order(-abs(agg$total)), c("payment_type", "n_transactions", "total", "fees")]
}

summarize_by_merchant <- function(tx, top_n = 10L) {
    if (nrow(tx) == 0L) {
        return(data.frame())
    }
    tx$merchant_key <- ifelse(
        is.na(tx$merchant) | !nzchar(tx$merchant),
        "(unknown)",
        tx$merchant
    )
    agg <- aggregate(
        list(total = tx$total),
        by = list(merchant = tx$merchant_key),
        FUN = sum
    )
    agg$n_transactions <- as.integer(tabulate(match(tx$merchant_key, agg$merchant)))
    agg <- agg[order(agg$total), , drop = FALSE]
    if (nrow(agg) > top_n) {
        agg <- agg[seq_len(top_n), , drop = FALSE]
    }
    agg
}

find_statement_pdfs <- function(dir_path, recursive = FALSE) {
    if (!dir.exists(dir_path)) {
        stop("Directory not found: ", dir_path, call. = FALSE)
    }
    pattern <- "\\.pdf$"
    files <- list.files(
        dir_path,
        pattern = pattern,
        ignore.case = TRUE,
        full.names = TRUE,
        recursive = recursive
    )
    files <- files[file.size(files) > 0]
    sort(files)
}

print_summary_report <- function(statements) {
    file_summaries <- do.call(
        rbind,
        lapply(statements, function(st) {
            summarize_transactions(st$transactions, st)
        })
    )

    all_tx <- do.call(rbind, lapply(statements, function(st) {
        if (nrow(st$transactions) == 0L) {
            return(NULL)
        }
        cbind(
            source_file = st$source_file,
            period_label = st$period_label,
            st$transactions
        )
    }))

    cat("\n=== PayPal statement summary ===\n\n")
    cat("Files parsed:", length(statements), "\n")
    if (nrow(file_summaries) > 0L) {
        cat("\n--- Per statement ---\n")
        print(file_summaries, row.names = FALSE)
    }

    if (!is.null(all_tx) && nrow(all_tx) > 0L) {
        cat("\n--- Combined ---\n")
        cat(
            "Transactions:", nrow(all_tx),
            "| Credits:", sprintf("%.2f", sum(all_tx$total[all_tx$total > 0], na.rm = TRUE)),
            "| Debits:", sprintf("%.2f", sum(all_tx$total[all_tx$total < 0], na.rm = TRUE)),
            "| Fees:", sprintf("%.2f", sum(all_tx$fees, na.rm = TRUE)),
            "| Net:", sprintf("%.2f", sum(all_tx$total, na.rm = TRUE)),
            "\n"
        )

        by_type <- summarize_by_payment_type(all_tx)
        if (nrow(by_type) > 0L) {
            cat("\n--- By payment type ---\n")
            print(by_type, row.names = FALSE)
        }

        by_merchant <- summarize_by_merchant(all_tx)
        if (nrow(by_merchant) > 0L) {
            cat("\n--- Top merchants (by spend) ---\n")
            print(by_merchant, row.names = FALSE)
        }
    }

    invisible(list(file_summaries = file_summaries, transactions = all_tx))
}

# === CLI =====================================================================

parse_cli_args <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    dir_path <- "C:/Users/brand/dl/test"
    recursive <- FALSE
    export_csv <- NULL

    i <- 1L
    while (i <= length(args)) {
        if (args[i] %in% c("-d", "--dir") && i < length(args)) {
            dir_path <- args[i + 1L]
            i <- i + 2L
        } else if (args[i] %in% c("-r", "--recursive")) {
            recursive <- TRUE
            i <- i + 1L
        } else if (args[i] %in% c("-o", "--export") && i < length(args)) {
            export_csv <- args[i + 1L]
            i <- i + 2L
        } else if (args[i] %in% c("-h", "--help")) {
            cat(paste0(
                "Usage: Rscript paypal_statement_summary.R [options]\n\n",
                "  -d, --dir PATH      Directory of PayPal PDF statements ",
                "(default: C:/Users/brand/dl/test)\n",
                "  -r, --recursive     Include PDFs in subdirectories\n",
                "  -o, --export PATH   Write combined transactions to CSV\n",
                "  -h, --help          Show this message\n"
            ))
            quit(save = "no", status = 0)
        } else {
            # Bare path argument
            dir_path <- args[i]
            i <- i + 1L
        }
    }

    list(dir = dir_path, recursive = recursive, export = export_csv)
}

# === Main ====================================================================

# Making this more like python's scripting approach
main <- function() {
    opts <- parse_cli_args()
    pdfs <- find_statement_pdfs(opts$dir, recursive = opts$recursive)

    if (length(pdfs) == 0L) {
        stop("No PDF files found in: ", opts$dir, call. = FALSE)
    }

    message("Found ", length(pdfs), " PDF(s) in ", opts$dir)

    statements <- lapply(pdfs, function(f) {
        message("Parsing: ", basename(f))
        read_paypal_statement(f)
    })

    result <- print_summary_report(statements)

    if (!is.null(opts$export) && !is.null(result$transactions)) {
        utils::write.csv(result$transactions, opts$export, row.names = FALSE)
        message("Wrote transactions to: ", opts$export)
    }

    invisible(result)
}


if (sys.nframe() == 0L) {
    main()
}
