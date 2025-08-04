#' @title dolock, unlock
#'
#' @description
#' lock and unlock data files for security.
#'
#' @param obj An object to save
#' @param key A string specifying password
#'
#' @examples
#' ## dolock
#' \dontrun{dolock("your data", "your password")}
#'
#' ## unlock
#' \dontrun{unlock("your data", "your password")}
#'
#' @export
dolock <- function(obj, key) UseMethod("dolock")

#' @export
#' @rdname dolock
unlock <- function(obj, key) UseMethod("unlock")

#' @method dolock default
#' @export
dolock.default <- function(obj, key) {
  openssl::aes_cbc_encrypt(
    serialize(obj, NULL), key = openssl::sha256(charToRaw(key))
  )
}

#' @method unlock default
#' @export
unlock.default <- function(obj, key) {
  unserialize(
    openssl::aes_cbc_decrypt(obj, key = openssl::sha256(charToRaw(key)))
  )
}

#' @method dolock list
#' @export
dolock.list <- function(obj, key) lapply(obj, function(x) dolock(x, key))

#' @method unlock list
#' @export
unlock.list <- function(obj, key) lapply(obj, function(x) unlock(x, key))
