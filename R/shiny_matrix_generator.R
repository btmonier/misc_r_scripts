#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   shiny_matrix_generator.R
# Description:   Generate binary matrices in Shiny
# Author:        Brandon Monier
# Created:       2020-05-30 at 11:52:29
# Last Modified: 2020-05-31 at 15:41:28
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to create a binary matrix
#    (i.e. a matrix of 1s and 0s) using interactive command with the
#    Shiny platform
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load packages ----
library(ggplot2)
library(magrittr)
library(shiny)



# === Shiny application (WIP) =======================================

## User interface ----
ui <- basicPage(
    shiny::numericInput(
        inputId = "num_row",
        label = "Number of rows",
        value = 3
    ),
    numericInput(
        inputId = "num_col",
        label = "Number of columns",
        value = 3
    ),
    actionButton("go", "Generate"),
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info")
)


## Server logic ----
server <- function(input, output) {
    val <- reactiveValues(clickx = NULL, clicky = NULL, data = NULL)

    observeEvent(input$go, {
        val$data <- matrix(
            rep(0, input$num_row * input$num_col),
            nrow = input$num_row,
            ncol = input$num_col
        )
    })

    observeEvent(input$plot_click, {
        val$clickx <- c(val$clickx, input$plot_click$x)
        val$clicky <- c(val$clicky, input$plot_click$y)

        if (is.null(input$plot_click$x)) {
            click_x <- NULL
        } else {
            click_x <- round(input$plot_click$x)
        }

        if (is.null(input$plot_click$y)) {
            click_y <- NULL
        } else {
            click_y <- round(input$plot_click$y)
        }

        if (val$data[click_y, click_x] == 0) {
            val$data[click_y, click_x] <- 1
        } else {
            val$data[click_y, click_x] <- 0
        }
    })

    output$plot1 <- renderPlot({
        if (input$go == 0) {
            return(NULL)
        } else {
            val$data %>%
                reshape2::melt() %>%
                ggplot() +
                aes(x = Var2, y = Var1, fill = as.factor(value)) +
                geom_tile(width = 0.9, height = 0.9) +
                scale_y_reverse() +
                scale_fill_manual(values = c("#a1a1a1", "#ff4f4f")) +
                coord_equal() +
                theme(
                    axis.text = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks = element_blank(),
                    legend.position = "none",
                    panel.background = element_blank()
                )

        }
    })

    output$info <- renderPrint({

        if (input$go == 0) {
            cat("## Click 'generate' to make data!")
        } else {
            tmp <- paste(as.vector(val$data), collapse = ", ")
            cat("## Copy and paste me to an R script ----\n")
            cat("test_mat <- matrix(\n")
            cat("    data = c(\n")
            cat(stringr::str_wrap(tmp, width = 60, indent = 8, exdent = 8), "\n", collapse = "")
            cat("   ),\n")
            cat(paste0("    nrow = ", input$num_row, ",\n"))
            cat(paste0("    ncol = ", input$num_col, "\n"))
            cat(")")

        }
    })

}


## Run application ----
shinyApp(ui, server)


