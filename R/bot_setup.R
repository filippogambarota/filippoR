bot_setup <- function(){
  library(telegram.bot)
  bot <<- Bot(token = bot_token("awesome_notifieR_bot"))
  updates <<- bot$getUpdates()
  chat_id <<- updates[[1L]]$from_chat_id()
}