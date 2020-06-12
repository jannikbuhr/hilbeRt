#' Get one Hilbert path
#'
#' Gets the Hilbert path for one particular order
#'
#' @param order, integer. Order of the Hilbert curve.
#'
#' @return a tibble of coordinates
get_hilbert_path <- function(order) {
  N <- 2^order
  total = N * N
  len <- 1 / N
  path <- rep(list(c(0,0)), total)

  hilbert <- function(i) {
    base_points <- list(
      c(0, 0),
      c(0, 1),
      c(1, 1),
      c(1, 0)
    )
    index = bitwAnd(i, 3)
    v = base_points[[index + 1]]
    for (j in 1:(order - 1)) {
      i = bitwShiftR(i, 2)
      index = bitwAnd(i, 3)
      len = 2^j
      if (index == 0) {
        temp = v[1];
        v[1] = v[2];
        v[2] = temp;
      } else if (index == 1) {
        v[2] = v[2] + len;
      } else if (index == 2) {
        v = v + len;
      } else if (index == 3) {
        temp = len - 1 - v[1];
        v[1] = len - 1 - v[2];
        v[2] = temp;
        v[1] = v[1] + len;
      }
    }
    return(v)
  }

  for (i in 0:(total - 1)) {
    path[[i + 1]] <- hilbert(i)
    path[[i + 1]] <- path[[i + 1]] * len + len / 2
  }
  path %>%
    lapply(setNames, c("x", "y")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(index = 1:dplyr::n())
}


#' Get Hilbert Paths
#'
#' Get the Hilbert paths for one multiple orders
#'
#' @param orders, integer vector. Orders of the Hilbert curves. E.g. \code{1:3}
#'
#' @return a tibble of coordinates
#' @export
#'
#' @examples
#' get_hilbert_paths(1:2)
get_hilbert_paths <- function(orders) {
  purrr::map_dfr(orders, get_hilbert_path, .id = "order")
}

#' Animate a smooth transition between Hilbert curve orders
#'
#' This function uses gganimate to produce an animation
#' of a morphing between the paths of the Hilbert curves
#' produced by \code{get_hilbert_paths(1:n)}
#'
#' @param paths of the Hilbert curves
#' produced by \code{get_hilbert_paths(1:n)}
#'
#' @return a gganimate object
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' get_hilbert_paths(1:3) %>%
#'   animate_hilbert_morph()
animate_hilbert_morph <- function(paths) {
  ggplot(paths, aes(x, y, group = 1, col = index)) +
    geom_path(size = 1.2) +
    guides(color = "none") +
    theme_void() +
    coord_equal() +
    scale_color_viridis_c() +
    gganimate::transition_states(order)
}



