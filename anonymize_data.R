
## generate random ids
random_string <- function(n) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

## read file paths
files <- list.files("PoliticalTweetsGenderData", full.names = T)

## use fake ids to replace real user ids and remove all identifiers
for (i in seq_along(files)) {
  rt <- readRDS(files[i])
  id <- data.frame(user_id = unique(rt$user_id))
  id$fake_id <- random_string(nrow(id))
  rt <- dplyr::left_join(rt, id, by = "user_id")
  rt <- dplyr::select(rt, -status_id, -user_id, -screen_name, -reply_to_status_id, 
                      -reply_to_screen_name, -reply_to_user_id, -retweet_user_id, 
                      -retweet_screen_name, -retweet_status_id, -reply_user_id,
                      -reply_screen_name, -name, -ssa_search_name, -mentions_screen_name, 
                      -user_id1, -mentions_user_id, -quoted_status_id)
  saveRDS(rt, files[i])
  
}



