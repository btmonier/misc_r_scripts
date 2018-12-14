# la croix  flavors

croix_palettes <- list(
	pure = c(
		"#B5E0EC", "#98C6D5", "#5DC5DC", "#43A2BA",
		"#0E266B", "#80A3B2", "#0991C4", "#CBC8C9"	
	),
	berry = c(
		"#C691B6", "#B37CA3", "#D4AAC7", "#E5C0D8",
		"#A1648E", "#172664", "#1784AD", "#65689C"	
	),
	cran_raspberry = c(
		"#E48887", "#EDA8A6", "#C56869", "#BC8586",
		"#182764", "#73627C", "#1985A4", "#D4CACA"
	),
	lemon = c(
		"#F0E086", "#CBB864", "#EFC805", "#CEA407",
		"#142265", "#80875C", "#1689B0", "#BFBFBB"	
	),
	lime = c(
		"#9CC26D", "#7FAA4E", "#B7D68F", "#0C1F69",
		"#D2DEB9", "#209E13", "#088BB3", "#52857A"	
	),
	orange = c(
		"#F2B855", "#EE9916", "#C97912", "#CB973E",
		"#16265E", "#767250", "#128992", "#C9C3BE"	
	),
	pamplemousse = c(
		"#EFC38E", "#CA9C68", "#D8848A", "#F3A5A8",
		"#1F3172", "#837985", "#2293AF", "#C1BCBC"	
	),
	peach_pear = c(
		"#DABC86", "#E6E3A5", "#DC9067", "#9F9674",
		"#1F3070", "#2695AD", "#E54613", "#BFC4BE"	
	),
	coconut = c(
		"#E5E4E6", "#E0CFBE", "#CDB296", "#19296E",
		"#B0B7BC", "#2299BB", "#978580", "#96461E"	
	),
	apricot = c(
		"#EC9E0A", "#E4E0E1", "#D06D0F", "#C1BCBF",
		"#151F59", "#7F868E", "#E6A26D", "#20A4A4"	
	),
	passionfruit = c(
		"#EC6883", "#C84163", "#E7E3E8", "#CEC3C9",
		"#15215E", "#507F9B", "#AF99AA", "#AEBF1A"	
	),
	key_lime = c(
		"#0B8733", "#91D30C", "#65AB10", "#11205D",
		"#CBD8BF", "#8CBF70", "#ECED05", "#BE5416"	
	),
	mango = c(
		"#86BF0C", "#E8E8E9", "#121E5A", "#CECECD",
		"#379A53", "#B3B8A6", "#7F8B96", "#E64313"	
	),
	tangerine = c(
		"#ECA024", "#F3B854", "#D5701F", "#101D5D",
		"#4F4A61", "#978955", "#D0C9C3", "#28ACA0"	
	)
)


croix_palette <- function(name, n = NULL, type = c("discrete", "continuous")) {
	type <- match.arg(type)
	
	pal <- croix_palettes[[name]]
	if (is.null(pal)) {
		stop("This palette could not be found.")
	}

	if (is.null(n)) {
		n <- length(pal)
	} else if (type == "discrete" && n > length(pal)) {
		warning(
			paste0(
				"n too large, allowed maximum for palette ", 
				"\"", name, "\"", " is ", length(pal), ".\n",
				"Displaying the palette you asked for with that many colors.",
				"\n"
			)
		)
		n <- length(pal)
	}

	if (type == "discrete") {
		pal <- pal[seq_len(n)]
	} else if (type == "continuous") {
		pal <- grDevices::colorRampPalette(pal)(n)
	}

	structure(pal, class = "croix", name = name, type = type)
}

syntax <- function(x) {
	if (grepl("_", x)) {
		x <- gsub("_", " ", x)
	}

	x <- strsplit(x, " ")[[1]]
  	x <- paste(
  		toupper(
  			substring(x, 1, 1)
  		), 
  		substring(x, 2),
      	sep = "", collapse = " "
  	)
  	return(x)
}


print.croix <- function(x, ...) {
	n <- length(x)
	old <- par(pty = "m", mar = c(3, 1, 3, 1))
	on.exit(par(old))

	image(
		x = 0:n, 
		y = c(0, 2.5), 
		z = as.matrix(1:n), 
		col = x,
		ylab = "",
		xlab = "", 
		xaxt = "n", 
		yaxt = "n", 
		bty = "n"
	)

	t_lab <- syntax(attr(x, "name"))
	b_lab <- paste("Colors:", n)

	rect(
		xleft = 0, 
		ybottom = 0, 
		xright = n, 
		ytop = 0.5, 
		col = rgb(1, 1, 1), 
		border = NA
	)

	rect(
		xleft = 0, 
		ybottom = 2, 
		xright = n, 
		ytop = 2.5, 
		col = rgb(1, 1, 1), 
		border = NA
	)	
	text(n * 0.5, 2.25, labels = t_lab, cex = 1, font = 2)
	text(n * 0.5, 0.25, labels = b_lab, cex = 1, font = 1)
}

summary.croix <- function(x, ...) {
	cat("La Croix, so tasty...\n")
	cat("  Class.....", class(x), "\n")
	cat("  Length....", length(x), "\n")
	cat("  Palette...", attr(x, "name"), "\n")
	cat("  Type......", attr(x, "type"), "\n")
}






croix_all <- function(...) {
	names <- names(croix_palettes)
	max_len <- 8
	par(mar = c(1, 1, 2, 1), pty = "s")

	plot(
		x = c(0, max_len),
		y = c(0, length(names)),
		type = "n",
		axes = FALSE,
		xlab = "",
		ylab = "",
		main = "All palettes"
	)

	for (i in seq_len(max_len)) {
		for (j in seq_along(names)) {
			rect(
				xleft = i - 1, 
				ybottom = j - 1, 
				xright = i, 
				ytop = j - 0.2, 
				col = croix_palette(names[j])[i], 
				border = "white",
				lwd = 5
			)	
		}
	}

	text(
		rep(-0.1, length(names)), 
		seq_along(names) - 0.6, 
		labels = names, 
		xpd = TRUE, 
		adj = 1
	)	
}

