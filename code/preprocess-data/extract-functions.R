library(stringr)

#http://stackoverflow.com/questions/18164839/get-twitter-username-with-regex-in-r
extract_user <- function(x){
  x <- tolower(x)  
  x <- unlist(strsplit(x, " "))
  x <- unlist(strsplit(x, "[.]"))
  regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
  regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
  users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
  return(users)
}

extract_user_mention <- function(x1){
  x1 <- tolower(x1)    
  
  x <- x1
  x <- unlist(strsplit(x, " rt"))
  if(length(x) > 1){
    x <- x[1]
    x <- unlist(strsplit(x, " "))
    x <- unlist(strsplit(x, "[.]"))
    regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
    regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
    users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
    
    return(users)
  }else{
    
    if(grepl("retweet",x1)){
      x <- x1
      x <- unlist(strsplit(x, "retweet"))
      x <- x[1]
      x <- unlist(strsplit(x, " "))
      x <- unlist(strsplit(x, "[.]"))
      regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
      regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
      users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])    
      return(users)
    }else{
      return(c())
    }
  }
}
extract_user_retweet <- function(x1){
  x1 <- tolower(x1)    
  
  x <- x1
  x <- unlist(strsplit(x, "rt"))
  x <- x[2]
  x <- unlist(strsplit(x, " "))
  x <- unlist(strsplit(x, "[.]"))
  regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
  regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
  users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])
  
  #if(grepl(value, chars)
  if(length(users) == 0 && grepl("retweet",x1)){
    x <- x1
    x <- unlist(strsplit(x, "retweet"))
    x <- x[2]
    x <- unlist(strsplit(x, " "))
    x <- unlist(strsplit(x, "[.]"))
    regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
    regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @  
    users <- gsub(regex2, "", x[grep(regex1, x, perl = T)])    
  }
  return(users)    
  
}