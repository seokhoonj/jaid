#' @title assert type
#' @description
#' Check the type of object.
#'
#' @param obj object
#' @param type A type string
#' @return no return
#'
#' @export
assert_type <- function(obj, type) {
  obj_name <- devars(obj)
  if (typeof(obj) != type) {
    stop(obj_name, " is not an object of type: '",
         paste(type, collapse = ", "), "'",
         call. = FALSE)
  }
}

#' @title assert class
#' @description
#' Check the class of object.
#'
#' @param obj object
#' @param class A class string
#' @return no return
#'
#' @export
assert_class <- function(obj, class) {
  obj_name <- devars(obj)
  if (!inherits(obj, class)) {
    stop(obj_name, " is not an object of class: '",
         paste(class, collapse = ", "), "'",
         call. = FALSE)
  }
}
