#' Capture and normalize column specifications into names
#'
#' This helper function accepts flexible column specifications and resolves
#' them into a character vector of valid column names from a data frame.
#'
#' @description
#' Handles both *standard evaluation* (character vectors, numeric indices,
#' variables containing them) and *non-standard evaluation* (bare symbols,
#' calls like `c(a, b)` or `.(a, b)`). It ensures that only valid column
#' names are returned.
#'
#' @param data A data.frame (or tibble/data.table) whose column names are
#'   used for validation.
#' @param cols Column specification. Can be:
#'   - A character vector of column names (e.g. `c("x", "y")`).
#'   - A numeric vector of indices (e.g. `c(1, 3)`).
#'   - A variable holding one of the above.
#'   - A non-standard expression such as `c(x, y)` or `.(x, y)`.
#'
#' @return
#' A character vector of column names (guaranteed to exist in `data`).
#' Returns `character(0)` if the input is `NULL` or not supplied.
#'
#' @details
#' - When used **inside another function**, make sure to capture and unquote
#'   the argument with `!!rlang::enquo(x)` before passing to `capture_names()`.
#'   For example:
#'   ```
#'   f <- function(data, cols) {
#'     capture_names(data, !!rlang::enquo(cols))
#'   }
#'   ```
#' @examples
#' \donttest{
#' # Character vector
#' capture_names(iris, c("Sepal.Length", "Petal.Width"))
#'
#' # Numeric indices
#' capture_names(iris, c(1, 4))
#'
#' # Variable holding names
#' cols_vec <- c("Sepal.Length", "Petal.Width")
#' capture_names(iris, cols_vec)
#'
#' # Bare symbols
#' capture_names(iris, c(Sepal.Length, Petal.Width))
#'
#' # data.table style
#' capture_names(iris, .(Sepal.Length, Petal.Width))
#'
#' # Wrappers: safe through multiple layers of NSE
#' f1 <- function(data, x) {x <- rlang::enquo(x); capture_names(data, !!x)}
#' f2 <- function(data, y) {y <- rlang::enquo(y); f1(data, !!y)}
#' f3 <- function(data, z) {z <- rlang::enquo(z); f2(data, !!z)}
#'
#' cols_vec <- c("Sepal.Length", "Petal.Width")
#'
#' f1(iris, c("Sepal.Length", "Petal.Width"))
#' f1(iris, c(Sepal.Length, Petal.Width))
#' f1(iris, cols_vec)
#' f2(iris, c("Sepal.Length", "Petal.Width"))
#' f2(iris, c(Sepal.Length, Petal.Width))
#' f2(iris, cols_vec)
#' f3(iris, c("Sepal.Length", "Petal.Width"))
#' f3(iris, c(Sepal.Length, Petal.Width))
#' f3(iris, cols_vec)
#' }
#'
#' @export
capture_names <- function(data, cols) {
  quo <- rlang::enquo(cols)
  if (rlang::quo_is_missing(quo) || rlang::quo_is_null(quo)) {
    return(character(0))
  }

  # 1) Single symbol: if it matches a column in `data`, return it directly
  expr <- rlang::get_expr(quo)
  if (rlang::is_symbol(expr)) {
    nm <- rlang::as_name(expr)
    if (nm %in% names(data)) return(nm)
  }

  # 2) Try to evaluate in the caller environment:
  #    - if character, return directly (after validation)
  #    - if numeric, convert to names (after bounds check)
  val <- tryCatch(
    rlang::eval_tidy(quo, env = rlang::caller_env()),
    error = function(e) e
  )

  if (!inherits(val, "error")) {
    if (is.character(val)) {
      miss <- setdiff(val, names(data))
      if (length(miss)) stop(
        "Unknown column(s): ", paste(miss, collapse = ", "),
        call. = FALSE
      )
      return(val)
    }
    if (is.numeric(val)) {
      if (any(val < 1L | val > ncol(data)))
        stop("Some indices are out of bounds for `data`.", call. = FALSE)
      return(names(data)[val])
    }
    # if other types, fall back to parsing
  }

  # 3) Fallback: non-standard expressions like c(a,b), list(a,b), .(a,b)
  nms <- capture_chr(!!quo)
  miss <- setdiff(nms, names(data))
  if (length(miss))
    stop("Unknown column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  nms
}

#' Convert expression input into a character vector
#'
#' Public wrapper that captures a user-supplied argument via
#' [rlang::enquo()] and normalizes it into a character vector of names.
#'
#' This is the **user-facing helper**: it works with expressions typed
#' directly at the call site, such as `c(a, b)` (bare symbols) or
#' `c("a", "b")` (character literals).
#'
#' For processing quosures or language objects **as values** (not typed
#' directly), see the internal helpers `.quo_to_chr()` and `.lang_to_chr()`.
#'
#' @param x An expression typed at the call site, e.g. `c(a, b)` or
#'   `c("a", "b")`. Single names like `a` are also supported.
#'
#' @return A character vector of names.
#'
#' @details
#' - When used **inside another function**, make sure to capture and unquote
#'   the argument with `!!rlang::enquo(x)` before passing to `capture_chr()`.
#'   For example:
#'   ```
#'   f <- function(x) {
#'     capture_chr(!!rlang::enquo(x))
#'   }
#'   ```
#'
#' @examples
#' \donttest{
#' # Character input
#' capture_chr(c("a", "b", "c"))
#'
#' # Bare symbols
#' capture_chr(c(a, b, c))
#'
#' # Wrappers: safe through multiple layers of NSE
#' f1 <- function(x) {x <- rlang::enquo(x); capture_chr(!!x)}
#' f2 <- function(y) {y <- rlang::enquo(y); f1(!!y)}
#' f3 <- function(z) {z <- rlang::enquo(z); f2(!!z)}
#'
#' f1(c("a", "b", "c"))
#' f1(c(a, b, c))
#' f2(c("a", "b", "c"))
#' f2(c(a, b, c))
#' f3(c("a", "b", "c"))
#' f3(c(a, b, c))
#' }
#'
#' @seealso [rlang::enquo()]
#'
#' @export
capture_chr <- function(x) {
  return(.quo_to_chr(rlang::enquo(x))) # rlang::enquo(x) must be outside .quo_to_chr()
}


# Internal helper functions -----------------------------------------------

# Internal helper: normalize quosure, symbol, call, or character into a char vector
#' @keywords internal
#' @noRd
.quo_to_chr <- function(x) {
  if (rlang::is_quosure(x))
    return(.lang_to_chr(rlang::get_expr(x)))

  if (rlang::is_symbol(x) || rlang::is_call(x))
    return(.lang_to_chr(x))

  if (is.character(x)) # check here as well as in .lang_to_chr()
    return(x)

  stop("Provide a quosure (enquo/quo), symbol/call, or a character vector.",
       call. = FALSE)
}

#' Normalize a raw language object (symbol or call) into a character vector
#'
#' - If `x` is a symbol → returns its name.
#' - If `x` is a call to `c()`, `list()`, or `.(...)` → recursively
#'   convert arguments and flatten to a character vector.
#' - If `x` is already a character vector → returns it unchanged.
#' - Otherwise → throws an error.
#'
#' @examples
#' \donttest{
#' # Symbol as a value
#' .lang_to_chr(as.name("a"))   # "a"
#'
#' # Call objects as values
#' .lang_to_chr(call("c", quote(a), quote(b)))    # c("a","b")
#' .lang_to_chr(call("list", quote(a), quote(b)))
#' .lang_to_chr(call(".", quote(a), quote(b)))    # data.table style
#' }
#'
#' @keywords internal
#' @noRd
.lang_to_chr <- function(x) {
  if (rlang::is_symbol(x))
    return(rlang::as_name(x))

  # c() / list() / .(...)
  if (rlang::is_call(x)) {
    fn <- tryCatch(
      rlang::as_string(rlang::call_name(x)),
      error = function(...) ""
    )
    if (fn %in% c("c", "list", ".")) {
      args <- rlang::call_args(x)
      return(unlist(lapply(args, .lang_to_chr), use.names = FALSE))
    }
  }

  if (is.character(x)) # check here as well as in .quo_to_chr()
    return(x)

  stop("`x` must be a symbol or a call like c(a, b) / list(a, b) / .(a, b).",
       call. = FALSE)
}
