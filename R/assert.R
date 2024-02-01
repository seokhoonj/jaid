#' Assert type
#'
#' Assert object type.
#'
#' @param obj object
#' @param type object type
#' @return no return
#'
#' @examples
#' # assert object type
#' \donttest{assert_type("x", "character")}
#'
#' @export
assert_type <- function(obj, type) {
  obj_name <- deparse(substitute(obj))
  if (typeof(obj) != type) {
    stop(obj_name, " is not an object of type: '",
         paste0(type, collapse = ", "), "'",
         call. = FALSE)
  }
}

#' Assert class
#'
#' Assert object class.
#'
#' @param obj object
#' @param class object class
#' @return no return
#'
#' @examples
#' # assert object class
#' \donttest{assert_class(cars, "data.frame")}
#'
#' @export
assert_class <- function(obj, class) {
  obj_name <- deparse(substitute(obj))
  if (!inherits(obj, class)) {
    stop(obj_name, " is not an object of class: '",
         paste0(class, collapse = ", "), "'",
         call. = FALSE)
  }
}
