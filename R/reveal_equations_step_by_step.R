#' @title step_by_step_eq
#' @description
#'  A function to allow display equations one line at a time in a revealjs
#'  presentation when the presentation is created using either quarto or
#'  xaringan.  The function ultimately creates individual slides for each
#'  equation and only increases the slide count for the first of these slides.
#'  The behavior mimics pausing in Beamer between lines of an equation.
#'
#' @param eqlist The list of equations.  A list of equations in latex format.
#'  The equations should be formatted as a character vector (see example below)
#'  Backslashes need to be escaped and the results are otherwise printed asis.
#'  Basically, this should just be a list of latex equations that are wrapped in
#'  $$\begin{aligned}...\end{aligned}$$, so whatever is provided in this list is
#'  just printed out.
#' @param before Optional text that goes before the equations.  This can be
#'  formatted exactly the same way as they would be with stepping through the
#'  equations line by line.
#' @param after Optional text that goes after the equations.
#' @param title Optional title for the slide.
#' @param presentation_type The type of presentation that the function is being
#'  used for.  The default choice is `quarto`.  The other option is `xaringan`.
#'
#' @return NULL.  The function returns nothing but prints the output for
#'  generating slides with equation appearing one after another.
#'
#' @examples
#' eqlist <- list("E[Y] &= E[Y|X=1]P(X=1) + E[Y|X=0]P(X=0) \\",
#'                "&= 1 \times 0.5 + 0 \times 0.5 \\",
#'                "&= 0.5")
#' before <- "Suppose that we know that $E[Y|X=1]=1$, $E[Y|X=0]=0$, $P(X=1)=0.5$,
#'  and $P(X=0) = 0.5$, then the law of iterated expecations implies that"
#' after <- "concluding text"
#' title <- "Law of Iterated Expectations Example"
#' step_by_step_eq(eqlist, before, after, title)
#'
#' @export
step_by_step_eq <- function(eqlist, before="", after="", title=" ",
                            presentation_type="quarto") {

  if (presentation_type == "quarto") {
    # drop slide pauses in before content
    before_inner <- gsub(". . .", "", before, fixed=TRUE)

    for (i in 2:length(eqlist)) {
      eqlist[i] <- paste0(eqlist[i-1],"\\\\\n",eqlist[i])
    }

    out <- ""
    for (i in 1:length(eqlist)) {
      out <- paste0(out, "## ", title) # print title
      if (i > 1) out <- paste0(out, " {visibility=\"uncounted\"} \n") else out <- paste0(out, " \n")
      # print before content
      if (i == 1) out <- paste0(out, before, "\n") else out <- paste0(out, before_inner, "\n")
      out <- paste0(out, "$$\n\\begin{aligned}\n",eqlist[[i]],"\n\\end{aligned}\n$$\n\n") # print equation
    }
    out <- paste0(out, after, "\n") # print after content
    cat(out)

  } else if (presentation_type == "xaringan") {

     # drop slide pauses in before content
    before_inner <- gsub("--", "", before)

    for (i in 2:length(eqlist)) {
      eqlist[i] <- paste0(eqlist[i-1],"\\\\\n",eqlist[i])
    }

    out <- ""
    for (i in 1:length(eqlist)) {
      if (i > 1) out <- paste0(out, "count:false", "\n")
      out <- paste0(out, "# ", title, "\n") # print title
      # print before content
      if (i == 1) out <- paste0(out, before, "\n") else out <- paste0(out, before_inner, "\n")
      out <- paste0(out, "$$\n\\begin{aligned}\n",eqlist[[i]],"\n\\end{aligned}\n$$\n\n") # print equation
      if (i < length(eqlist)) out <- paste0(out, "---\n\n")
    }
    out <- paste0(out, after, "\n") # print after content
    out <- paste0(out, "---\n")
    cat(out)

  } else {
    stop(paste0("Presentation type: ", presentation_type, " is not supported"))
  }

}

