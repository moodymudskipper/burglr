
<!-- README.md is generated from README.Rmd. Please edit that file -->

# burglr <img src='man/figures/logo.png' align="right" height="138" />

Experimental\!

{burglr} provides a way to copy functions from other packages without
adding them as dependencies.

It fetches the code of the required function along with all other
functions that it depends on, whether they’re in the same package or
not.

Use ethically\! (see notes)

## Installation

Install with :

``` r
remotes::install_github("moodymudskipper/burglr")
```

## Example

If you run `burglr::burgle(Hmisc::ballocation, Hmisc::as.discrete)`, a
`"burgled.R"` file will be created in the `"R"` folder of your package.

It will contain everything necessary to make the functions work as in
the original package.

``` r
burglr::burgle(Hmisc::ballocation, Hmisc::as.discrete)
#> Copying Hmisc:::ballocation and its dependencies
#> Hmisc:::ballocation
#> Hmisc:::bpower
#> scales:::alpha
#> Copying Hmisc:::as.discrete and its dependencies
#> Hmisc:::as.discrete
#> Hmisc:::as.discrete.default
#> Hmisc:::is.discrete
#> Hmisc:::discrete
#> Please consider giving credit to the authors by adding them as contributors in your package's DESCRIPTION file!
```

Content of `"R/burgled.R"` :

``` r
# generated by {burglr}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ballocation (copied from Hmisc:::ballocation)

# from Hmisc 4.4-2
`ballocation` <- function(p1, p2, n, alpha = 0.05) {
  q1 <- 1 - p1
  q2 <- 1 - p2
  f.minvar.diff <- 1 / (1 + sqrt(p2 * q2 / (p1 * q1)))
  f.minvar.ratio <- 1 / (1 + sqrt(p1 * q2 / p2 / q1))
  z <- c(
    fraction.group1.min.var.diff = f.minvar.diff, fraction.group1.min.var.ratio = f.minvar.ratio,
    fraction.group1.min.var.logodds = 1 - f.minvar.diff
  )
  if (!missing(n)) {
    possf <- seq(0.001, 0.999, length = 1000)
    pow <- bpower(p1, p2,
      n1 = n * possf, n2 = n * (1 - possf),
      alpha = alpha
    )
    f <- possf[pow == max(pow)]
    f <- f[abs(f - 0.5) == min(abs(f - 0.5))]
    z <- c(z, fraction.group1.max.power = f[1])
  }
  z
}

# from Hmisc 4.4-2
`bpower` <- function(p1, p2, odds.ratio, percent.reduction, n, n1, n2, alpha = 0.05) {
  if (!missing(odds.ratio)) {
    p2 <- p1 * odds.ratio / (1 - p1 + p1 * odds.ratio)
  } else if (!missing(percent.reduction)) {
    p2 <- p1 * (1 - percent.reduction / 100)
  }
  if (!missing(n)) {
    n1 <- n2 <- n / 2
  }
  z <- qnorm(1 - alpha / 2)
  q1 <- 1 - p1
  q2 <- 1 - p2
  pm <- (n1 * p1 + n2 * p2) / (n1 + n2)
  ds <- z * sqrt((1 / n1 + 1 / n2) * pm * (1 - pm))
  ex <- abs(p1 - p2)
  sd <- sqrt(p1 * q1 / n1 + p2 * q2 / n2)
  c(Power = 1 - pnorm((ds - ex) / sd) + pnorm((-ds - ex) / sd))
}

# from scales 1.1.1
`alpha` <- function(colour, alpha = NA) {
  if (length(colour) != length(alpha)) {
    if (length(colour) > 1 && length(alpha) > 1) {
      stop("Only one of colour and alpha can be vectorised")
    }
    if (length(colour) > 1) {
      alpha <- rep(alpha, length.out = length(colour))
    }
    else {
      colour <- rep(colour, length.out = length(alpha))
    }
  }
  rgb <- farver::decode_colour(colour, alpha = TRUE)
  rgb[!is.na(alpha), 4] <- alpha[!is.na(alpha)]
  farver::encode_colour(rgb, rgb[, 4])
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# as.discrete (copied from Hmisc:::as.discrete)

# from Hmisc 4.4-2
`as.discrete` <- function(x, ...) {
  UseMethod("as.discrete")
}

# from Hmisc 4.4-2
#' @export
`as.discrete.default` <- function(x, ...) {
  if (is.discrete(x)) {
    x
  } else {
    discrete(x)
  }
}

# from Hmisc 4.4-2
`is.discrete` <- function(x) {
  inherits(x, "discrete")
}

# from Hmisc 4.4-2
`discrete` <- function(x, levels = sort(unique.default(x), na.last = TRUE),
                       exclude = NA) {
  if (!is.numeric(x)) {
    stop("x must be a numeric vairable")
  }
  exclude <- as.vector(exclude, typeof(x))
  levels <- levels[is.na(match(levels, exclude))]
  f <- x[!(x %in% exclude)]
  attr(f, "levels") <- levels
  class(f) <- "discrete"
  f
}
```

We see that :

  - the package and its version are mentioned.
  - When importing S3 generics, all available methods are imported too,
    and are tagged with `#' @export` so they can be registered when you
    `devtools::document()`

## Technical notes

  - `burgle()` makes sure to check which packages are imported so we
    avoid unnecessary copies of functions, so for instance in the
    example above if your package imports {scales} `scales::alpha` won’t
    be copied
  - You can name arguments if you wish to rename the function in your
    package. This is not recommended in general as recursive functions
    and other corner cases won’t work.
  - You cannot (yet ?) copy functions that use code in C, C++ or
    Fortran, in their own body or through their unimported dependencies.
  - A convenient use case is to use {burgler} on your own packages of
    misc functions.
  - This is not 100% robust but seems to work ok in general. Please
    report issues.

## Ethical and legal notes

  - Use dependencies when you can\!
  - The name is tongue in cheek and should serve as a reminder that
    copying a substantial amount of code without giving credit is
    unethical and possibly illegal.
  - If you copy functions from packages you don’t own, consider editing
    the DESCRIPTION file to give credit where it’s due.
  - Pay attention to licenses
