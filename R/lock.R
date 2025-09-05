#' Lock and unlock data objects
#'
#' `dolock()` encrypts and locks an object using a password key.
#' `unlock()` decrypts and restores a locked object with the same key.
#'
#' @param obj An R object to be locked or unlocked.
#' @param key A character string specifying the password used for locking/unlocking.
#'
#' @return The locked (encrypted) object for `dolock()`, or the unlocked
#'   (decrypted) object for `unlock()`.
#'
#' @examples
#' \dontrun{
#' ## Lock an object
#' locked <- dolock("secret", "password")
#'
#' ## Unlock it
#' original <- unlock(locked, "password")
#' }
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
