#------------------------------------------------------------------------------
# Title:         Function - revealJS compiler
# Author:        Brandon Monier
# Created:       2018-05-24 10:55:12 CDT
# Last Modified: 2018-05-24 11:07:02 CDT
#------------------------------------------------------------------------------

revealer <- function(presentation, ...) {
	rmarkdown::render(
		presentation,
		revealjs::revealjs_presentation(
			incremental           = FALSE,
			center                = FALSE,
			slide_level           = 2,
			fig_width             = 8,
			fig_height            = 3,
			fig_caption           = FALSE,
			smart                 = TRUE,
			self_contained        = TRUE,
			theme                 = "simple",
			transition            = "fade",
			background_transition = "default",
			reveal_options        = NULL,
			reveal_plugins        = NULL,
			highlight             = "default",
			mathjax               = "default",
			template              = "default",
			includes              = NULL,
			keep_md               = FALSE,
			lib_dir               = NULL,
			pandoc_args           = NULL,
			extra_dependencies    = NULL,
			...
		)
	)
}