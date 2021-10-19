
#' add_genders_variables
#'
#' @param data 
add_genders_variables <- function(data) {
  ## This code will check if you need to install the gender and genderdata 
  ##   packages and then download if not.
  if (!"gender" %in% installed.packages()) {
    install.packages("gender")
  }
  if (!"genderdata" %in% installed.packages()) {
    install.packages(
      "genderdata", type = "source",
      repos = "http://packages.ropensci.org"
    )
  }
  ## validate users object
  is_users_data <- function(x) {
    all(c("name", "description", "verified") %in% names(x))
  }
  ## validate tweets object
  is_tweets_data <- function(x) {
    all(c("status_id", "created_at", "is_retweet") %in% names(x))
  }
  ## depends on whether its users or tweets data
  if (is_users_data(data)) {
    users <- TRUE
    usr <- data
    tw <- rtweet::tweets_data(data)
  } else {
    users <- FALSE
    usr <- rtweet::users_data(data)
    tw <- data
    ## throw error if users data doesn't exist
    if (nrow(usr) == 0L) {
      stop(
        "Users data not found.", call. = FALSE
      )
    }
  }
  ## get first names (in lower case)
  nms <- trimws(tolower(usr$name))
  nms[nms == ""] <- " "
  nms <- sapply(strsplit(nms, " "), "[[", 1L)
  usr$ssa_search_name <- nms
  ## lookup gender estimates
  g <- gender::gender(unique(nms), years = c(1930, 2012), method = "ssa")
  ## only keep columns 1-4
  g <- g[, 1:4]
  ## rename the "name" variable
  names(g)[1] <- "ssa_search_name"
  ## left_join usr data with gender data
  usr <- dplyr::left_join(
    usr, g, by = "ssa_search_name"
  )
  ## match user IDs between data sets
  userpos <- match(usr$user_id, tw$user_id)
  ## bind columns
  tw <- dplyr::bind_cols(
    tw[, !duplicated(names(tw))], 
    usr[userpos, c("user_id", "proportion_male", "proportion_female", "gender")]
  )
  ## restore attributes and return data
  if (users) {
    attr(usr, "tweets") <- tw
    return(usr)
  } else {
    attr(tw, "users") <- usr
    return(tw)
  }
}
