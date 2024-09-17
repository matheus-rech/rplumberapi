# R/authentication.R

library(jose)
library(openssl)

# Generate a secret key (in a real-world scenario, this should be stored securely)
secret_key <- random_bytes(32)

generate_token <- function(user_id) {
  token <- jwt_encode(list(user_id = user_id, exp = as.numeric(Sys.time()) + 3600), secret_key)
  return(token)
}

verify_token <- function(token) {
  tryCatch({
    decoded <- jwt_decode(token, secret_key)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

is_authenticated <- function(req) {
  auth_header <- req$HTTP_AUTHORIZATION
  if (is.null(auth_header)) {
    return(FALSE)
  }
  
  token <- sub("^Bearer ", "", auth_header)
  return(verify_token(token))
}
