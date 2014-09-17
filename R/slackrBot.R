#' main function, will loop and watch for messages
#'
#' Needs some updates here.
#'
#' @param latest the datetime (as numeric) from which to start looking at messages.
#' @param sleep the amount of time between checks in seconds
#' @param channel slack channel
#' @param username slack username to respond as
#' @param icon_emoji slack emoji to use
#' @param api_token slack api token
#' @export
checkslack <- function(latest=as.numeric(Sys.time()), sleep=2,
                       channel=Sys.getenv("SLACK_CHANNEL"),
                       username=Sys.getenv("SLACK_USERNAME"),
                       icon_emoji=Sys.getenv("SLACK_ICON_EMOJI"),
                       api_token=Sys.getenv("SLACK_API_TOKEN")) {
  if (grepl("^\\#", channel)) {
    channel <- slackrChTrans(channel)
  }
  for (i in seq(600)) {
    # cat("checking", latest, "\n")
    cat("Checking channel", channel, "\n")
    latest <- checknew(latest, channel, username, icon_emoji, api_token)
    # cat("Back, latest is now:", latest, "\n")
    Sys.sleep(sleep)
  }
}


#' Checks a specific slack channel for updated messages.
#'
#' Needs some updates here.
#'
#' @param oldest the datetime (as numeric) from which to start looking at messages.
#' @param channel slack channel
#' @param username slack username to respond as
#' @param icon_emoji slack emoji to use
#' @param api_token slack api token
#' @export
checknew <- function(oldest=as.numeric(Sys.time()),
                     channel=Sys.getenv("SLACK_CHANNEL"),
                     username=Sys.getenv("SLACK_USERNAME"),
                     icon_emoji=Sys.getenv("SLACK_ICON_EMOJI"),
                     api_token=Sys.getenv("SLACK_API_TOKEN")) {

  tryCatch({
    resp <- POST(url="https://slack.com/api/channels.history",
               body=list(token=api_token, channel=channel,
                         oldest=oldest))
  },
  error=function(cond) {
    message("Here's the original error message:")
    message(cond)
    return(NA)
  },
  warning=function(cond) {
    message("Here's the original warning message:")
    message(cond)
    return(NULL)
  },
  finally={
  })

  fullret <- list()
  try(fullret <- content(resp))
  latest <- oldest
  for(i in seq_along(fullret$messages)) {
    msg <- fullret$messages[[i]]
    msg$text <- tolower(msg$text)
    if("text" %in% names(msg)) {
      msg$targeted <- FALSE
      if(grepl("^jaybot", msg$text)) {
        msg$targeted <- TRUE
        msg$text <- sub("^jaybot[:, ]+", "", msg$text, perl=T)
      }
      cat("Found message:", msg$text, "\n")
      msg$name <- lookupUser(msg$user)
      respond(msg=msg, channel, username, icon_emoji, api_token)
    } else {
      print(msg)
    }
    # cat("ts:", msg$ts, "\n")
    # cat("latest:", latest, "\n")
    if("ts" %in% names(msg)) if(msg$ts > latest) latest <- msg$ts
  }
  # cat("latest <-", latest, "\n")
  latest
}

#' Figure out what response is necessary and send it.
#'
#' Needs some updates here.
#'
#' @param msg the slack message (as a list)
#' @param channel slack channel
#' @param username slack username to respond as
#' @param icon_emoji slack emoji to use
#' @param api_token slack api token
#' @export
respond <- function(msg,
                    channel=Sys.getenv("SLACK_CHANNEL"),
                    username=Sys.getenv("SLACK_USERNAME"),
                    icon_emoji=Sys.getenv("SLACK_ICON_EMOJI"),
                    api_token=Sys.getenv("SLACK_API_TOKEN")) {
  savetime <- proc.time()

  base <- "https://slack.com/api/chat.postMessage?token=xoxp-2616035440-2615222145-2615646207-b24ab3&"
  vname <- c("channel", "text", "username")

  words <- unlist(strsplit(msg$text, " "))
  response <- NULL
  if (msg$targeted) {
    if (words[1] == "insult") {
      response <- insult(paste(words[2:length(words)], collapse=" "), msg$name)
    } else if (grepl("how (the hell )?(are )?(ya|you)( doin\\'?g?)?\\?*$", msg$text, perl=T)) {
      howAreYa = c("just great", "peachy", "mas o menos",
                   "you know how it is", "eh, ok", "pretty good. how about you");
      response <- sample(howAreYa, 1)
    } else if (grepl("thank|thanx|thx", msg$text)) {
      response <- thanks(msg$name)
    } else if (grepl("excuse", msg$text)) {
      response <- paste0(msg$name, ": _",  sample(bofh, 1), "_")
    } else if (words[1]=="hello") {
      response <- paste0(msg$name, ": Hey yourself")
    } else {
      response <- paste0("I hear you, but you make no sense: `", msg, "`")
    }
  } else if (grepl("thank|thanx|thx", msg$text)) {
    lastmsg <- Sys.getenv("SLACK_LASTMSG")
    recent <- as.numeric(Sys.time())-10
    if (lastmsg > recent) {
      response <- thanks(msg$name)
    }
  }
  # response <- paste(response, paste0("_Responded in ", round((proc.time() - savetime)[3], 2), " seconds._"), sep="\n")
  # vval <- c(channel, response, "jaybot")
  # url <- paste0(base, paste(vname, vval, sep="=", collapse="&"))
  if (!is.null(response)) {
    cat("response: ", response, "\n")
    resp <- POST(url="https://slack.com/api/chat.postMessage",
                 body=list(token=api_token, channel=channel,
                           username=username, icon_emoji=icon_emoji,
                           text=response, link_names=1))
    Sys.setenv(SLACK_LASTMSG=as.numeric(Sys.time()))
  }
}

#' given username, returns the api ID
#'
#' Needs some updates here.
#'
#' @param user the slack username (must be preceded by "at" symbol)
#' @param api_token slack api token
#' @export
lookupUser <- function(user, api_token=Sys.getenv("SLACK_API_TOKEN")) {
  users <- slackrUsers(api_token)
  users$name <- paste0("@", users$name)
  ifelse(grepl("^@", user), users$id[users$name==user], users$name[users$id==user])
}

#' Multiple ways to respond to a thank you.
#'
#' Needs some updates here.
#'
#' @param from the text name of the person thanking us
#' @export
thanks <- function(from) {
  welcome <- c("your welcome", "da nada", "Anytime!", "'twas nothing", "My pleasure!",
               "Anything for my good friend, $name.", paste0("I like helping you, ", from, "."))
  sample(welcome, 1)
}

#' random insult generator
#'
#' Needs some updates here.
#'
#' @param user the name of the entity to insult
#' @param from the person asking for the insult
#' @export
insult <- function(user, from) {
  # https://github.com/akatrevorjay/lucy/blob/master/lib/Acme/Scurvy/Whoreson/BilgeRat/Backend/insultserver.pm
  srcadj <- c("acidic", "antique", "contemptible", "culturally-unsound", "despicable", "evil", "fermented",
           "festering", "foul", "fulminating", "humid", "impure", "inept", "inferior", "industrial",
           "left-over", "low-quality", "malodorous", "off-color", "penguin-molesting",
           "petrified", "pointy-nosed", "salty", "sausage-snorfling", "tastless", "tempestuous",
           "tepid", "tofu-nibbling", "unintelligent", "unoriginal", "uninspiring", "weasel-smelling",
           "wretched", "spam-sucking", "egg-sucking", "decayed", "halfbaked", "infected", "squishy",
           "porous", "pickled", "coughed-up", "thick", "vapid", "hacked-up",
           "unmuzzled", "bawdy", "vain", "lumpish", "churlish", "fobbing", "rank", "craven", "puking",
           "jarring", "fly-bitten", "pox-marked", "fen-sucked", "spongy", "droning", "gleeking", "warped",
           "currish", "milk-livered", "surly", "mammering", "ill-borne", "beef-witted", "tickle-brained",
           "half-faced", "headless", "wayward", "rump-fed", "onion-eyed", "beslubbering", "villainous",
           "lewd-minded", "cockered", "full-gorged", "rude-snouted", "crook-pated", "pribbling",
           "dread-bolted", "fool-born", "puny", "fawning", "sheep-biting", "dankish", "goatish",
           "weather-bitten", "knotty-pated", "malt-wormy", "saucyspleened", "motley-mind",
           "it-fowling", "vassal-willed", "loggerheaded", "clapper-clawed", "frothy", "ruttish",
           "clouted", "common-kissing", "pignutted", "folly-fallen", "plume-plucked", "flap-mouthed",
           "swag-bellied", "dizzy-eyed", "gorbellied", "weedy", "reeky", "measled", "spur-galled", "mangled",
           "impertinent", "bootless", "toad-spotted", "hasty-witted", "horn-beat", "yeasty",
           "imp-bladdereddle-headed", "boil-brained", "tottering", "hedge-born", "hugger-muggered",
           "elf-skinned")

  srcamt <- c("accumulation", "bucket", "coagulation", "enema-bucketful", "gob", "half-mouthful",
           "heap", "mass", "mound", "petrification", "pile", "puddle", "stack", "thimbleful", "tongueful",
           "ooze", "quart", "bag", "plate", "trunkfull", "buttload")

  srcnoun <- c("bat toenails", "bug spit", "cat hair", "chicken pee", "dog vomit", "dung",
             "stomach-bile", "fish heads", "guano", "gunk", "pond scum", "rat retch",
             "red dye number-9", "git manuals", "waffle-house grits", "yoo-hoo",
             "dog drool", "seagull puke", "cat bladders", "pus", "urine samples",
             "squirrel guts", "snake butts", "snake bait", "buzzard gizzards",
             "cat hairballs", "rat farts", "pods", "armadillo snouts", "entrails",
             "snake snot", "eel ooze", "slurpee backwash", "toxic waste", "Stimpy-drool",
             "craptacular carpet droppings", "cold sores", "warts")
  target <- user
  post <- NULL
  if(runif(1)<0.05) {
    target <- from
    post <- paste0("(", user, " was rubber, you were glue).")
  }

  srcsetup <- c(paste(target, "is nothing but"), paste("I heard", target, "is"),
                paste(target, "is"), paste(target, "is the embodiment of"))
  setup <- sample(srcsetup, 1)
  adj <- sample(srcadj, 2)
  amt <- sample(srcamt, 1)
  noun <- sample(srcnoun, 1)
  preposition <- ifelse(grepl("^[aeiou]+", adj[1], perl=T), "an", "a")
  paste(setup, preposition, adj[1], amt, "of", adj[2], noun, post);
}

#' List of BOFH excuses
#'
#' Full list of BOFH excuses taken from http://pages.cs.wisc.edu/~ballard/bofh/excuses
#'
#' @docType data
#' @keywords datasets
#' @format text list
#' @name bofh
NULL