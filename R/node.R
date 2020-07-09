#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name vctrs_treenode
NULL


#' new_treenode
#'
#' @param parent an integer vector
#' @param id an integer vector
#' @param xmin a double vector
#' @param xmax a double vector
#' @param ymin a double vector
#' @param ymax a double vector
#'
#' @export
new_treenode <- function(parent = integer(), id = integer(),
                         xmin = double(), xmax = double(),
                         ymin = double(), ymax = double()) {

  vec_assert(parent, integer())
  vec_assert(id, integer())
  vec_assert(xmin, double())
  vec_assert(xmax, double())
  vec_assert(ymin, double())
  vec_assert(xmax, double())

  # single root?

  # validate ids

  # validate_parents

  # non-overlapping squares


  new_rcrd(
    list(parent = parent, id = id, xmin = xmin,
         xmax = xmax, ymin = ymin, ymax = ymax),
    class = "vctrs_treenode"
  )
}


#' `treenode` vector
#'
#' This creates a vector of treenodes, each encoding a square in the Cartesian
#' space.
#'
#' @param parent an integer vector
#' @param id an integer vector
#' @param xmin a double vector
#' @param xmax a double vector
#' @param ymin a double vector
#' @param ymax a double vector
#'
#' @return As S3 vector of class `vctrs_treenode`
#' @export
#'
#' @examples
#' # An example tree with:
#' # - 1 root (box = (0,100,0,100)),
#' # - 2 childs (box1 = (0,50,0,50), box2 = (50,100,50,100))
#'
#' treenode(
#'   parent = c(0,0,0),
#'   id = 0:2,
#'   xmin = c(0,0,50),
#'   xmax = c(100,50,100),
#'   ymin = c(0,0,50),
#'   ymax = c(100,50,100)
#' )
treenode <- function(parent, id, xmin, xmax, ymin, ymax) {
  parent <- as.integer(parent)
  id <- as.integer(id)
  xmin <- as.double(xmin)
  xmax <- as.double(xmax)
  ymin <- as.double(ymin)
  ymax <- as.double(ymax)

  if(!all(xmin < xmax)) {
    stop("xmin must be smaller than xmax, for all x")
  }
  if(!all(ymin < ymax)) {
    stop("ymin must be smaller than ymax, for all y")
  }

  new_treenode(parent, id, xmin, xmax, ymin, ymax)
}

#' is_node
#'
#' @param x a `treenode`
#'
#' @export
is_treenode <- function(x) {
  inherits(x, "vctrs_treenode")
}

#' @export
format.vctrs_treenode <- function(x,...) {
  id <- field(x,"id")
  parent <- field(x, "parent")
  xmin <- field(x, "xmin")
  xmax <- field(x, "xmax")
  ymin <- field(x, "ymin")
  ymax <- field(x, "ymax")

  glue::glue("node {id}, son of {parent}, lord of box ",
             "[{xmin}, {xmax}, {ymin}, {ymax}]")
}

#' @export
vec_ptype_abbr.vctrs_treenode <- function(x, ...) {
  "v"
}

#' @export
vec_ptype_full.vctrs_treenode <- function(x, ...) {
  "treenode"
}

#' @export
vec_ptype2.vctrs_node.vctrs_treenode <- function(x, y, ...) new_treenode()

