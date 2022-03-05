# Función para descargar tweets

get_timeline_unlimited <- function(users, n, since_id = NULL) {
  
  if (length(users) == 0) {
    return(NULL)
  }
  
  rl <- rate_limit(token = twitter_token,
                   query = "get_timeline")
  
  if (length(users) <= rl$remaining) {
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, 
                           n, 
                           check = FALSE,
                           token = twitter_token,
                           retryonratelimit = TRUE,
                           since_id = since_id)  
  } else {
    
    if (rl$remaining > 0) {
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, 
                                   n, 
                                   check = FALSE,
                                   token = twitter_token,
                                   retryonratelimit = TRUE,
                                   since_id = since_id)
      rl <- rate_limit(token = twitter_token,
                       query = "get_timeline")
    } else {
      
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + .1
    print(glue("Waiting for {round(wait, 2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n, since_id)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}


# Concerned with iterating over a vector of users
# and handling API timeouts
get_followers_or_wait <- function(user_id) {
  
  rl <- rate_limit(token = twitter_token,
                   query = "get_followers")
  
  if (rl$remaining == 0) {
    
    wait <- rl$reset + .1
    print(glue("Waiting for {round(wait, 2)} minutes"))
    Sys.sleep(wait * 60)
    
  } else {
    
    get_followers(user_id)
  }
}

get_retweeters_or_wait <- function(status_id) {
  
  rl <- rate_limit(token = twitter_token,
                   query = "get_retweeters")
  
  if (rl$remaining == 0) {
    wait <- rl$reset + .1
    print(glue("Waiting for {round(wait, 2)} minutes"))
    Sys.sleep(wait * 60)
    
  } else {
    
    get_retweeters(status_id)
    
  }
}


