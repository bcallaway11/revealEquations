
<!-- README.md is generated from README.Rmd. Please edit that file -->

# revealEquations

This is some code that can be used to reveal equations line by line when
making quarto or xaringan presentations, similar to “pausing” in Beamer.
It’s possible this functionality exists somewhere else, but I had
trouble finding it.

Here is an example of some code (for quarto) to produce slides that step
through equations:

```` r
---
title: "revealEquations Demo"
format: revealjs
---

## Slide 1

Some info

```{r results="asis"}
eqlist <- list("E[Y] &= E[Y|X=1]P(X=1) + E[Y|X=0]P(X=0) \\",
               "&= 1 \\times 0.5 + 0 \\times 0.5 \\",
               "&= 0.5")
before <- "Suppose that we know that $E[Y|X=1]=1$, $E[Y|X=0]=0$, $P(X=1)=0.5$,
 and $P(X=0) = 0.5$, then the law of iterated expecations implies that"
after <- "concluding text"
title <- "Law of Iterated Expectations Example"
revealEquations::step_by_step_eq(eqlist, before, after, title)
``` 
````

and you can [see the slides themselves
here](https://bcallaway11.github.io/revealEquations/revealEquations_demo.html).
