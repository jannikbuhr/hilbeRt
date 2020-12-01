
# hilbeRt

The goal of hilbeRt is to draw some pretty Hilbert curves. It is
inspired by the Coding Train’s episode on Hilbert curves
([link](https://www.youtube.com/watch?v=dSK-MW-zuAc)) and the code is
largely just a port of the original java/processing code to R. This in
turn is an implementation of [this
algorithm](https://marcin-chwedczuk.github.io/iterative-algorithm-for-drawing-hilbert-curve)
by Marcin Chwedczuk.

## Installation

You can install `hilbeRt` from github with:

``` r
# install.packages("remotes")
remotes::install_github("jmbuhr/hilbeRt")
```

## Example

``` r
library(hilbeRt)
get_hilbert_paths(1:7) %>%
  animate_hilbert_morph()
```

<img src="man/figures/README-example-1.gif" width="100%" />
