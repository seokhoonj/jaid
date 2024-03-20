#' Assert class
#'
#' Assert object class.
#'
#' @param obj an object
#' @param class an object class
#' @return no return
#'
#' @examples
#' # assert object class
#' \donttest{assert_class(cars, "data.frame")}
#'
#' @export
assert_class <- function(obj, class) {
  obj_name <- desub(obj)
  if (!inherits(obj, class)) {
    stop(obj_name, " is not an object of class: '",
         paste(class, collapse = ", "), "'",
         call. = FALSE)
  }
}
