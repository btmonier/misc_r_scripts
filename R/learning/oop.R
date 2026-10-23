#---------------------------------------------------------------------
# Title:         S4 Function Tests
# Author:        Brandon Monier
# Created:       2018-02-27 at 20:46:48
# Last Modified: 2018-02-27 at 20:46:48
#---------------------------------------------------------------------

# Define an S4 Class

## Let's make something dumb...
setClass(
	Class = "rhizome",
	representation(
		area = "numeric",
		units = "character",
		structure = "character",
		ingredients = "data.frame"
	)
)

rhizome <- function(area, units, structure) {
	new("rhizome", area = area, units = units, structure = structure)
}

# Define S4 methods and generics

## Show (no generic needed)
setMethod(
	f = "show",
	signature = "rhizome",
	definition = function(object) {
		cat("Your gingR metadata...\n")
		cat("  Class     :", class(object), "\n")
		cat("  Area      :", object@area, "\n")
		cat("  Units     :", object@units, "\n")
		cat("  Structure :", object@structure, "\n")
	}
)

## Ingredients
setGeneric(
	name = "materials",
	def = function(object) {
		standardGeneric("materials")
	}
)

setMethod(
	f = "materials",
	signature = "rhizome",
	definition = function(object) {
		tmp.amount <- c(
			0.007191188, 0.005708062, 0.004651828, 
			0.006346201, 0.004224936, 0.023360364, 
			0.007824928, 0.008674314, 0.003382148
		)
		tmp.material <- c(
			"baking soda", "brown sugar", "butter", "cinnamon", "eggs", 
			"flour", "ginger", "ground cloves", "molasses"
		)
		tmp.unit <- c(
			"tsp", "cup", "cup", "tblsp", "egg", "cup", "tblsp", 
			"tsp", "cup"
		)
		ar <- object@area
		units <- object@units

		message("Calculating measurements...")

		if(units == "in") {
			n.area = ar * tmp.amount * 0.5
		} else if (units == "ft") {
			n.area = ar * 12
			n.area = n.area * tmp.amount * 0.5
		} else if (units == "cm") {
			n.area = ar * 0.39370079
			n.area = n.area * tmp.amount * 0.5
		} else if (units == "m") {
			n.area = ar * 39.37
			n.area = n.area * tmp.amount * 0.5
		}

		tmp.df <- data.frame(
			ingredient = tmp.material,
			amount = round(n.area, 2),
			unit = tmp.unit
		)
		object@ingredients <- tmp.df
		return(object)
	}
)


