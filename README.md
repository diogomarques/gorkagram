
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gorkagram

<!-- badges: start -->

<!-- badges: end -->

This R package draws gorkagrams.

A `gorkagram` is a graph depicting a value of a variable in a
qualitative semantic differential with labelled polarities. The
depiction of the value is not supposed to be exact, but instead, in
conjunction with it’s label, to be evocative of a degree of proximity to
a polarity.

The original diagram appears as “Diagram Nine” in

> Gorka, S. (2007) Content and end-state-based alteration in the
> practice of political violence since the end of cold war: the
> difference between the terrorism of the cold war and the terrorism of
> al Qaeda: the rise of the “transcendental terrorist”. PhD
> Dissertation, Corvinus University, 166.

## Installation

This is a toy package and you should absolutely not install it ever,
lest you bring upon us the demise of Western Civilization.

If you insist, you can install it with:

``` r
# install.packages("devtools")
devtools::install_github("diogomarques/gorkagram")
```

But please don’t.

## Example

To draw a gorkagram, specify the polarities (`from`, `to`), a label for
the variable of interest (`what`), and it’s placement (`what`) between
polarities, from 0 to 1.

``` r
library(gorkagram)
gorkagram(from = "Spread", 
          to = "Dip",
          what = "Hummus", 
          where = 35/100)
```

<img src="man/figures/README-example-1.png" width="100%" />
