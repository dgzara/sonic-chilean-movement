folder <- paste0(getwd(),"/code/preprocess-data")
setwd(folder)
source(file='../libraries.R')
source(file='../dbConnect.R')
source(file='extract-functions.R')

# Definitions
retweet_definition <- " text LIKE '%retweet @%' OR  text LIKE'%_ rt @%' OR  text LIKE'%RT @%' OR text LIKE'rt @%' OR  text LIKE'%Rt @%' OR  text LIKE'%rT @%' OR  text LIKE'%thx @%' OR  text LIKE'%MT @%' OR text LIKE'%retweeting @%'"
retweet_definition_nested <- " text NOT LIKE '%retweet @%' AND text NOT LIKE '%_ rt @%' AND text NOT LIKE '%RT @%' AND text NOT LIKE 'rt @%' AND text NOT LIKE '%Rt @%' AND text NOT LIKE '%rT @%' AND text NOT LIKE '%thx @%' AND text NOT LIKE '%MT @%' AND text NOT LIKE '%retweeting @%'"

# List of hashtags
sql <- paste("SELECT DISTINCT hashtag FROM hashtags")
hashtags <- dbGetQuery(mydb, sql)
hashtags <- sort(unique(unlist(hashtags$hashtag, use.names = FALSE)))

# Arreglamos UTF-8
dbSendQuery(mydb, "SET NAMES utf8")

# Borramos la base de datos
q <- "TRUNCATE TABLE hashtags_network;"
dbSendQuery(mydb, q)

# We check for each hashtag
for(i in 1:length(hashtags))
{
  print(hashtags[i])
  
  # First Get the mention networks in reply
  sql <- paste("SELECT id, hashtag, user_screen_name as user, text, created_at, user_profile_location, user_followers_count, user_time_zone   
               FROM hashtags
               WHERE text LIKE '@%' AND (",retweet_definition_nested,") AND hashtag = '",hashtags[i],"';",sep="")
  text_reply <-dbGetQuery(mydb, sql)
  
  # Get the mention and retweet in retweet networks
  sql <- paste("SELECT id, hashtag, user_screen_name as user, text, created_at, user_profile_location, user_followers_count, user_time_zone 
               FROM hashtags
               WHERE (",retweet_definition,") AND hashtag = '",hashtags[i],"';",sep="")
  text_reply_retweet <- dbGetQuery(mydb, sql)
  
  # Get the mentions in mention network
  sql <- paste("SELECT id, hashtag, user_screen_name as user, text, created_at, user_profile_location, user_followers_count, user_time_zone  
               FROM hashtags
               WHERE text LIKE '%@%' AND hashtag = '",hashtags[i],"' AND text NOT LIKE '@%' AND (",retweet_definition_nested,");",sep="")
  text_reply_mention <- dbGetQuery(mydb, sql)
  
  # Build replies network
  n <- nrow(text_reply)
  if(n>0){
    for(k in 1:n)
    {
      source <- text_reply[k,]$user
      content <- text_reply[k,]$text
      
      users_list <- extract_user(content)
      if(length(users_list) > 0){
        for(l in 1:length(users_list)){
          user <- users_list[l]
          if(l == 1) {type <- "reply"}
          else {type <- "mention"}
          user <- gsub("'s",'',user)
          user <- gsub("@",'',user)
          if(str_length(user)>0){
            sql_reply <- sprintf('INSERT INTO hashtags_network (source, target, type, hashtag, text, datetime, source_profile_location, source_followers_count, source_time_zone, tweet_id) VALUES ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s")', source, user, type, text_reply[k,]$hashtag, gsub("\"", "'", text_reply[k,]$text), text_reply[k,]$created_at, gsub("\"", "'", text_reply[k,]$user_profile_location), text_reply[k,]$user_followers_count, text_reply[k,]$user_time_zone, text_reply[k,]$id)
            dbSendQuery(mydb, sql_reply)          
          }
        } 
      }
      l == 0
    }
  }
  
  # Build retweets network
  n <- nrow(text_reply_retweet)
  if(n>0)
  {
    for(k in 1:n)
    {
      source <- text_reply_retweet[k,]$user
      content <- text_reply_retweet[k,]$text
      
      #mention in retweet networks
      users_list_mention <- extract_user_mention(content)
      if(length(users_list_mention)>0){
        for(l in 1:length(users_list_mention)){
          user_mention <- users_list_mention[l]
          type <- "mention"
          user_mention <- gsub("'s",'',user_mention)
          user_mention <- gsub("@",'',user_mention)
          if(str_length(user_mention) > 0){
            sql_mention <- sprintf('INSERT INTO hashtags_network (source, target, type, hashtag, text, datetime, source_profile_location, source_followers_count, source_time_zone, tweet_id) VALUES ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s")', source, user, type, text_reply_retweet[k,]$hashtag, gsub("\"", "'", text_reply_retweet[k,]$text), text_reply_retweet[k,]$created_at, gsub("\"", "'", text_reply_retweet[k,]$user_profile_location), text_reply_retweet[k,]$user_followers_count, text_reply_retweet[k,]$user_time_zone, text_reply_retweet[k,]$id)
            dbSendQuery(mydb, sql_mention)      
          }
        }    
      }
      l == 0
      
      #retweet in retweet networks
      users_list <- extract_user_retweet(content)
      if(length(users_list) > 0){
        user <- users_list[1]
        type <- "retweet"
        user <- gsub("'s",'',user)
        user <- gsub("@",'',user)
        if(str_length(user) > 0){
          sql_retweet <- sprintf('INSERT INTO hashtags_network (source, target, type, hashtag, text, datetime, source_profile_location, source_followers_count, source_time_zone, tweet_id) VALUES ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s")', source, user, type, text_reply_retweet[k,]$hashtag, gsub("\"", "'", text_reply_retweet[k,]$text), text_reply_retweet[k,]$created_at, gsub("\"", "'", text_reply_retweet[k,]$user_profile_location), text_reply_retweet[k,]$user_followers_count, text_reply_retweet[k,]$user_time_zone, text_reply_retweet[k,]$id)
          dbSendQuery(mydb, sql_retweet)
        }
      }
    }
  }
  
  # Build mentions network
  n <- nrow(text_reply_mention)
  if(n>0){
    for(k in 1:n)
    {
      source <- text_reply_mention[k,]$user
      content <- text_reply_mention[k,]$text
      
      users_list <- extract_user(content)
      type <- "mention"
      if(length(users_list) > 0){
        for(l in 1:length(users_list)){
          user <- users_list[l]
          user <- gsub("'s",'',user)
          user <- gsub("@",'',user)
          if(str_length(user) > 0){
            sql_mention_single <- sprintf('INSERT INTO hashtags_network (source, target, type, hashtag, text, datetime, source_profile_location, source_followers_count, source_time_zone, tweet_id) VALUES ("%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s", "%s")', source, user, type, text_reply_mention[k,]$hashtag, gsub("\"", "'", text_reply_mention[k,]$text), text_reply_mention[k,]$created_at, gsub("\"", "'", text_reply_mention[k,]$user_profile_location), text_reply_mention[k,]$user_followers_count, text_reply_mention[k,]$user_time_zone, text_reply_mention[k,]$id)
            dbSendQuery(mydb, sql_mention_single)        
          }
        }   
      }
      l == 0
    }
  }
  
}

# We disconnect
dbDisconnect(mydb)

