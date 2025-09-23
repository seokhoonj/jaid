# Trace argument expression -----------------------------------------------

#' Retrieve the Original Expression Passed to an Argument
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Retrieves the original expression or symbol name passed to `x`,
#' similar to `deparse(substitute(x))`, but designed to work
#' through multiple layers of nested function calls.
#'
#' Unlike `substitute()`, this function walks the call stack frame-by-frame
#' to reconstruct the original argument, which allows it to work even after
#' many nested calls or wrappers.
#'
#' In **Shiny reactive or promise contexts**, the altered call stack may make
#' exact recovery of the symbol name unreliable. This implementation improves
#' stability by:
#' \itemize{
#'   \item Using `stop_at` to halt the search at a known caller function
#'         (avoids traversing reactive internals)
#'   \item Handling evaluation errors safely with `tryCatch()`
#'   \item Preserving verbose diagnostic output without causing format errors
#' }
#'
#' For critical Shiny logic, still treat the recovered name as **best-effort**
#' and provide fallback behavior for ambiguous or missing results.
#'
#' @param x An expression or object to inspect.
#'   Should be a symbol, a call, or an evaluable expression.
#'   Character literals (e.g., `"x"`) are not traceable and will return `"unknown"`.
#' @param stop_at Character vector of function names.
#'   If a matching function name is encountered in the call stack,
#'   the search stops and that frame's `x` argument is returned.
#'   This is useful for limiting the search to a known entry point
#'   (e.g., "assert_class") to avoid traversing unrelated frames.
#' @param verbose Logical; if `TRUE`, prints detailed inspection steps. Default is `FALSE`.
#' @param skip_shiny Logical; if `TRUE`, and a Shiny context is detected
#'   (via [.is_shiny_running()]), the function immediately returns `"x"`
#'   instead of traversing the altered Shiny call stack.
#'   This provides a safe fallback for reactive/promise environments where
#'   the original symbol name cannot be reliably recovered.
#' @param max_depth Maximum number of frames to inspect.
#'   Optional; defaults to the current stack depth.
#'
#' @return A character string or character vector representing the original expression.
#'
#' @note
#' - In normal R execution, `trace_arg_expr()` can reliably retrieve original symbol names
#'   even after deep nesting.
#' - In Shiny/reactive contexts, `stop_at` is **strongly recommended** to improve
#'   reproducibility and performance.
#' - If no symbol is found, returns `NULL`.
#'
#' @examples
#' \donttest{
#' # Console
#' f1 <- function(a) trace_arg_expr(x = a)
#' f2 <- function(b) f1(b)
#' f3 <- function(c) f2(c)
#' f4 <- function(d) f3(d)
#' f5 <- function(e) f4(e)
#'
#' trace_arg_expr(iris) # "iris"
#' f1(iris) # "iris"
#' f2(iris) # "iris"
#' f3(iris) # "iris"
#' f4(iris) # "iris"
#' f5(iris) # "iris"
#' }
#'
#' \dontrun{
#' # Shiny
#' f1 <- function(a, stop_at, verbose) trace_arg_expr(x = a, stop_at = stop_at,
#'   verbose = verbose)
#' f2 <- function(b, stop_at, verbose) f1(b, stop_at, verbose)
#' f3 <- function(c, stop_at, verbose) f2(c, stop_at, verbose)
#' f4 <- function(d, stop_at, verbose) f3(d, stop_at, verbose)
#' f5 <- function(e, stop_at, verbose) f4(e, stop_at, verbose)
#
#' ui <- shiny::fluidPage(
#'   shiny::titlePanel("Test f5() for trace_arg_expr() in Shiny"),
#'   shiny::actionButton("run", "Run f5(iris, stop_at = 'f5', verbose = TRUE)"),
#'   shiny::verbatimTextOutput("output")
#' )
#'
#' server <- function(input, output) {
#'   result <- shiny::reactiveVal("")
#'
#'   shiny::observeEvent(input$run, {
#'     res <- utils::capture.output({
#'       out <- f5(iris, stop_at = "f5", verbose = TRUE)
#'       cat(sprintf("Result from f5: '%s'\n", out))
#'     })
#'     result(paste(res, collapse = "\n"))
#'   })
#'
#'   output$output <- shiny::renderText(result())
#' }
#'
#' shiny::shinyApp(ui, server)
#'
#' # Exception
#' f1 <- function(a, stop_at, verbose) trace_arg_expr(x = a, stop_at = stop_at,
#'   verbose = verbose)
#' f2 <- function(b, stop_at, verbose) f1(b, stop_at, verbose)
#' f3 <- function(c, stop_at, verbose) f2(c, stop_at, verbose)
#' f4 <- function(d, stop_at, verbose) f3(d, stop_at, verbose)
#' f5 <- function(e, stop_at, verbose) f4(e, stop_at, verbose)
#'
#' trace_arg_expr("x") # "\"x\""
#' do.call("trace_arg_expr", list(x = iris)) # "x"
#' do.call(trace_arg_expr, list(x = iris)) # "x"
#' do.call("f5", list(e = iris, stop_at = "f5", verbose = FALSE)) # "e"
#' do.call(f5, list(e = iris, stop_at = "f5", verbose = FALSE)) # "a"
#' rlang::exec("trace_arg_expr", x = iris) # structure(list(Sepal.Length = c(...
#' rlang::exec(trace_arg_expr, x = iris) # structure(list(Sepal.Length = c(...
#' rlang::exec("f5", e = iris, stop_at = "f5", verbose = FALSE) # "e"
#' rlang::exec(f5, e = iris, stop_at = "f5", verbose = FALSE) # "a"
#' }
#'
#' @export
trace_arg_expr <- function(x, stop_at, verbose = FALSE, skip_shiny = TRUE,
                           max_depth) {
  lifecycle::signal_stage("experimental", "trace_arg_expr()")

  # starting expression (argument as seen in the current frame)
  expr <- substitute(x)
  fallback <- .safe_deparse(expr)

  # return fallback immediately in Shiny contexts
  if (skip_shiny && .is_shiny_running())
    return(fallback)

  # if the very first expr is not a language object, fallback immediately
  if (!(is.symbol(expr) || is.call(expr))) {
    if (verbose) cat(sprintf("initial expr is a literal/value \u2192 fallback '%s'\n", fallback))
    return(fallback)
  }

  # cap maximum depth safely
  if (missing(stop_at))
    stop_at <- NULL
  if (missing(max_depth)) {
    max_depth <- sys.nframe()
  } else {
    max_depth <- min(max_depth, sys.nframe())
  }
  if (verbose)
    cat(sprintf("Checking...\nMax depth (capped): %s\n", max_depth))

  prefix <- paste0("Frame %", nchar(max_depth), "d:")

  # traverse frames from inner to outer
  for (i in seq(from = max_depth, to = 1L)) {
    call <- tryCatch(sys.call(i),     error = function(e) NULL)
    fn   <- tryCatch(sys.function(i), error = function(e) NULL)
    if (is.null(call) || is.null(fn)) {
      if (verbose) cat(sprintf(prefix, i), "skipped - no call/fn\n")
      next
    }

    mc <- tryCatch(match.call(fn, call, expand.dots = FALSE),
                   error = function(e) NULL)

    if (!is.call(mc) || length(mc) < 2L) {
      if (verbose) cat(sprintf(prefix, i), "skipped - not enough arguments\n")
      next
    }

    # function name (as called)
    fn_name <- tryCatch(.norm_fn_name(mc[[1L]], fn), error = function(e) "")

    # if the frame is primitive or skip list -> return fallback_symbol ("x")
    if (.is_skip_fn(fn_name) || is.primitive(fn)) {
      if (verbose) {
        cat(sprintf(prefix, i),
            sprintf("skipped - wrapper/primitive (%s) \u2192 fallback '%s'\n",
                    fn_name, fallback))
      }
      return(fallback)
    }

    expr_new <- .rewrite_expr_via_call(expr, mc, fn)

    # verbose: print the "after-rewrite" expression for this frame
    if (verbose) {
      # if expr is a symbol or a call, print it; otherwise, print a skip message
      if (is.symbol(expr) || is.call(expr)) {
        cat(sprintf(prefix, i),
            sprintf("'%s' (from call: %s)\n",
                    .safe_deparse(expr_new),
                    .safe_deparse(mc)))
      } else {
        cat(sprintf(prefix, i), "skipped - unsupported argument type\n")
      }
    }

    # stop_at: if this frame matches, stop and return the current expr
    if (!is.null(stop_at) && fn_name %in% stop_at) {
      return(.safe_deparse(expr))
    }

    # carry the rewritten expr upward
    expr <- expr_new

    # if this is the last frame (i == 1) and not skipped, fallback.
    if (i == 1L && .is_skip_fn(fn_name)) {
      if (verbose)
        cat(sprintf("expression not traceable at top frame \u2192 fallback '%s'\n",
                    fallback))
      return(fallback)
    }
  }

  .safe_deparse(expr)
}


# Internal helper functions -----------------------------------------------

.norm_fn_name <- function(head, fn = NULL) {
  if (is.symbol(head)) {
    nm <- as.character(head)
    # If it's a bare symbol, try to prepend the package namespace if available
    if (!grepl("::", nm) && !is.null(fn)) {
      pkg <- tryCatch(getNamespaceName(environment(fn)),
                      error = function(e) NA_character_)
      if (!is.na(pkg)) nm <- paste0(pkg, "::", nm)
    }
    return(nm)
  }
  if (is.call(head)) {
    op <- as.character(head[[1L]])[1L]
    if (op %in% c("::", ":::") && length(head) >= 3L) {
      pkg <- as.character(head[[2L]])[1L]
      fun <- as.character(head[[3L]])[1L]
      return(paste0(pkg, op, fun))
    }
    if (op == "(" && length(head) >= 2L)
      return(.norm_fn_name(head[[2L]], fn))
    return(op)
  }
  ""
}

.is_skip_fn <- function(fn_name) {
  skip_fns <- c(
    # wrappers
    "do.call", "base::do.call",
    "exec", "rlang::exec", "rlang:::exec_impl",

    # base apply family / HOF
    "apply", "lapply", "sapply", "vapply", "mapply",
    "Map", "Reduce", "Filter", "Find",
    "by", "tapply", "aggregate",
    "replicate",
    "Vectorize", "outer",

    # purrr family
    "purrr::map", "purrr::imap", "purrr::pmap",
    "purrr::walk", "purrr::pwalk",
    "purrr::map2", "purrr::imap_dfr", "purrr::imap_dfc",

    # future/furrr
    "future.apply::future_lapply",
    "furrr::future_map", "furrr::future_pmap", "furrr::future_walk",

    # parallel/snow
    "parallel::mclapply", "parallel::parLapply",
    "parallel::parSapply", "parallel::clusterApply",
    "parallel::clusterApplyLB",

    # etc
    "UseMethod", "NextMethod",
    "tryCatch", "withCallingHandlers"
  )

  if (!nzchar(fn_name)) return(FALSE)
  any(vapply(skip_fns, function(pfx) grepl(paste0("^", pfx), fn_name), logical(1L)))
}

# recursively substitute symbols in expr based on match.call(mc) and the function's formals
.rewrite_expr_via_call <- function(expr, mc, fn) {
  fmls <- names(formals(fn))
  if (is.null(fmls)) fmls <- character(0)

  rewrite_sym <- function(sym) {
    nm <- as.character(sym)
    repl <- mc[[nm]]
    # only rewrite to language objects; keep symbol if replacement is a literal
    if (nm %in% fmls && !is.null(repl) && (is.symbol(repl) || is.call(repl))) {
      repl
    } else {
      sym
    }
  }

  if (is.symbol(expr)) {
    return(rewrite_sym(expr))
  }

  if (is.call(expr)) {
    # leave the head (function name) as is, recursively substitute the arguments
    parts <- as.list(expr)
    head  <- parts[[1L]]
    args  <- parts[-1L]
    args2 <- lapply(args, function(a) .rewrite_expr_via_call(a, mc, fn))
    return(as.call(c(list(head), args2)))
  }
  expr
}

.safe_deparse <- function(x, max_chars = 300L) {
  out <- tryCatch(paste(deparse(x), collapse = " "),
                  error = function(e) "<deparse-error>")
  if (!is.character(out) || length(out) != 1L || is.na(out))
    out <- as.character(out)[1L]
  if (nchar(out) > max_chars)
    out <- paste0(substr(out, 1L, max_chars), "...")
  return(out)
}

#' Check if a Shiny session is running
#'
#' This utility inspects whether Shiny is currently running.
#'
#' - If the `shiny` namespace is already loaded, it looks up and safely
#'   calls `shiny::isRunning()`.
#' - If `shiny` is not loaded, it simply returns `FALSE`.
#' - Any errors from `shiny::isRunning()` are caught and treated as `FALSE`.
#'
#' @return A logical scalar: `TRUE` if `shiny` is running, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' # Returns FALSE in non-Shiny environments
#' # In a Shiny app, will return TRUE once the session is active
#' .is_shiny_running()
#' }
#'
#' @keywords internal
.is_shiny_running <- function() {
  if ("shiny" %in% loadedNamespaces()) {
    fun <- tryCatch(get("isRunning", envir = asNamespace("shiny"), inherits = FALSE),
                    error = function(e) NULL)
    if (is.function(fun)) {
      return(isTRUE(tryCatch(fun(), error = function(e) FALSE)))
    }
  }
  FALSE
}

# Deprecated functions ----------------------------------------------------

# desub <- function(x) {
#   substitute(x) |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     eval(envir = parent.frame(n =  1)) |>
#     eval(envir = parent.frame(n =  2)) |>
#     eval(envir = parent.frame(n =  3)) |>
#     eval(envir = parent.frame(n =  4)) |>
#     eval(envir = parent.frame(n =  5)) |>
#     eval(envir = parent.frame(n =  6)) |>
#     eval(envir = parent.frame(n =  7)) |>
#     eval(envir = parent.frame(n =  8)) |>
#     eval(envir = parent.frame(n =  9)) |>
#     eval(envir = parent.frame(n = 10)) |>
#     eval(envir = parent.frame(n = 11)) |>
#     eval(envir = parent.frame(n = 12)) |>
#     eval(envir = parent.frame(n = 13)) |>
#     deparse()
# }

# desubs <- function(x) {
#   substitute(x) |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     substitute() |>
#     eval(envir = parent.frame(n =  1)) |>
#     eval(envir = parent.frame(n =  2)) |>
#     eval(envir = parent.frame(n =  3)) |>
#     eval(envir = parent.frame(n =  4)) |>
#     eval(envir = parent.frame(n =  5)) |>
#     eval(envir = parent.frame(n =  6)) |>
#     eval(envir = parent.frame(n =  7)) |>
#     eval(envir = parent.frame(n =  8)) |>
#     eval(envir = parent.frame(n =  9)) |>
#     eval(envir = parent.frame(n = 10)) |>
#     eval(envir = parent.frame(n = 11)) |>
#     eval(envir = parent.frame(n = 12)) |>
#     eval(envir = parent.frame(n = 13)) |>
#     vapply(FUN = deparse, FUN.VALUE = "character")
# }
