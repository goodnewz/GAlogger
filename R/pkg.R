# ### INITIALIZE ####
# create the working environment and set everything in it
# galog$settings$tracking_id
# galog$settings$hostname
# galog$settings$consent
# galog$user_id
# galog$client_id
# galog$url
# galog$message
galog <- new.env(parent = emptyenv())

# ### END INITIALIZE ####

#' Save settings specified by user to file
#'
#' @param path location of the file to use
#'
#' @return the settings in a JSON format
#' @export
#' @examples ge_get_settings()
ga_load_settings <- function(path){
  if(missing(path)){
    path="~/galog_settings.json"
  }
  if (!file.exists(path)) {
    stop("No settings file found. Specify the correct path or use ?ga_save_settings to save them first")
  } else{
    # TODO check if settings are in env and get them from there. Otherwise read from disk
    settings <- jsonlite::read_json(path,flatten = TRUE,simplifyVector=TRUE)
  }
  # TODO CHECK IF SETTING NAMES ARE VALID
  print("Settings have been loaded")
  invisible(settings)
}

#' Set user defined settings to file
#'
#' @param path location of file to use
#' @param ... settings to save. Must be a list with name-value pair (e.g. tracking_id="UA-XXXXXXXX-x")
#'
#' @return a helpful message
#' @export
#'
#' @examples ga_set_settings(tracking_id="UA-XXXXXXXX-x",hostname="www.Foo.com",consent=TRUE)
ga_save_settings <- function(path=NULL,...){
  
  if(is.null(path)){
    path <- "~/galog_settings.json"
  }
  new_settings <- list(...)
  
  # TODO CHECK IF SETTING NAMES ARE VALID
  
  if (file.exists(path)) {
    current_settings <- jsonlite::read_json(path,flatten = TRUE,simplifyVector=TRUE)
    ## remove duplicates
    current_settings<- current_settings[!names(current_settings)==names(new_settings)]
    new_settings<- append(current_settings,new_settings)
  }
  jsonlite::write_json(new_settings, path)
  print("Setting have been saved")
  
}

ga_delete_settings <- function(path=NULL,...){
  if(is.null(path)){
    path <- "~/galog_settings.json"
  }
  
  if (file.exists(path)) {
    file.remove(path)
    print("Setting have been deleted")
  } else {print("No settings file to delete")}

}

#' @title Provide the Google Analytics tracking ID where all user interactions will be logged to
#' @description The Google Analytics tracking ID looks something like UA-XXXXX-Y. For example UA-25938715-4.
#' Get your own tracking ID at the Google Analytics website.
#' All collected user interactions will be logged with that tracking ID.
#' @param x a character string with the Google Analytics tracking ID
#' @return invisibly a list all general settings used to send data to Google Analytics
#' @export
#' @examples
#' ga_set_tracking_id("UA-XXXXXXXX-X")
ga_set_tracking_id <- function(tracking_id = NULL){
  if(is.null(tracking_id) || !is.character(tracking_id)){
    stop("You forgot to set up a proper tracking id. You currently set: ",tracking_id)
  }
  invisible(tracking_id)
}

#' @title Provide the identifier which will be used to identify a visitor/user
#' @description Set the identifier of a visitor as it is known by you. 
#' The user_id identifier is the identifier of the visitor/user as it is know by you.
#' By default for every new R session, a new client_id is generated.
#' @param user_id a character string with the visitor/user known to you. Defaults to a randomly generated UUID.
#' @return invisibly a list all general settings used to send data to Google Analytics
#' @export
#' @examples
#' ga_set_user_id()
#' ga_set_user_id("root")
#' ga_set_user_id("team-datascientists")
#' ga_set_user_id("shiny-server")
#'
#' x <- sprintf("%s-%s", Sys.getpid(), tolower(Sys.getenv("USERNAME", unset = "default")))
#' x
#' ga_set_user_id(x)
ga_set_user_id <- function(user_id = NULL){
  if(is.null(user_id)){
    # set USERID if null
    user_id <- NULL
    galog$client_id <- ga_set_client_id(client_id = NULL)
  }
  user_id <- curl::curl_escape(user_id)
  invisible(user_id)
}


#' Set client id
#' 
#' Defaults to a randomly generated identifier.
#' 
#' You can also set the client_id identifier which anonymously identifies a particular user or device. 
#' For R users this client_id identifies the same user across different R sessions. 
#' The value of this field should be a random UUID (version 4) as described in \url{http://www.ietf.org/rfc/rfc4122.txt}
#'
#' @param client_id a character string in UUID format which anonymously and uniquely identifies a particular R user or device across different R sessions.
#' Defaults to a randomly generated UUID.
#'
#' @return client_id
#' @export
#'
#' @examples ga_set_client_id(client_id=uuid::UUIDgenerate())
ga_set_client_id <- function(client_id = NULL){
  if(is.null(client_id)){
    # set clientID if null
    client_id <- uuid::UUIDgenerate()
  }
  client_id <- curl::curl_escape(client_id)
  invisible(client_id)
}


#' Create galog url
#' 
#' Creates galog$url and ready to send to Google
#'
#'  v=1              // Version. 
#'  &tid=UA-XXXXX-Y  // Tracking ID / Property ID.
#'  &cid=555         // This anonymously identifies a particular user, device,
#'  or browser instance 
#'  &uid=555         // Known identifier for a user provided
#'  by the site owner/tracking library user 
#'  &ds=GAlogger        // Data source: set to GAlogger
#'  
#'  Google protocol guidelines:
#'  
#' https://developers.google.com/analytics/devguides/collection/protocol/v1/devguide
#' https://developers.google.com/analytics/devguides/collection/protocol/v1/parameters
#'
#' @return galog object
#' @export
#'
#' @examples ga_set_url(tracking_id="UA-XXXXXXXX-X")
ga_set_url <- function(tracking_id=NULL){
  if(!is.null(tracking_id)){
    url <-"http://www.google-analytics.com/collect?v=1&tid=%s&ds=GAlogger" 
    tid <- tracking_id
  }else{
    stop("You must specify a Google analytics property ID (tracking_id)")
  }
  invisible(url)
}

#' Set hostname
#'
#' @param host_name the hostname as character
#'
#' @return galog object with hostname in
#' @export
#'
#' @examples
#' ga_set_hostname("Magical hostname")
ga_set_hostname <- function(hostname='Google.com'){
  if(!is.character(hostname) & length(hostname) != 1 & nchar(hostname) == 0){
    stop("hostname was not specified correctly")
  }
  invisible(hostname)
}

#' Set the aproval message for the user
#'
#' @param x sepcify a custom message; otherwise use default message
#'
#' @return the message
#' @export
#'
#' @examples
ga_set_approval_message <- function(message = NULL) {
  if (is.null(message)) {
    message <- sprintf(
      "Hello
      This is just a message to inform you that we are collecting information how you use this application\nWe will send the following information to Google Analytics:
      - Which parts of our application you are using
      - When errors are occurring
      - Your information will be tracked anonymously as user %s
      - This information is collected in order to provide us better insights on how people use this application\n"
    )
  }
  invisible(message)
}

#' @title Request for approval of the user to send information to Google Analytics
#' @description Request for approval of the user to send information to Google Analytics.
#' The approval is requested by setting yes/no in the prompt if consent is set to FALSE and the user is working in interactive mode.\cr
#' If consent is set to TRUE, the developer is responsible for complying to legislation in your area like the 
#' General Data Protection Regulation (GDPR) in Europa.
#' @param message a character string with a message to show to the user before approval is requested. If not given will show a default message.
#' @param consent logical indicating to give approval. Defaults to FALSE indicating that no approval is given.
#' @return invisibly a list all general settings used to send data to Google Analytics
#' @export
#' @examples
#' ## Request user input
#' ga_set_approval(consent = FALSE)
#' ga_set_approval(consent = FALSE,
#'                 message = "Please approve that we send usage information to Google Analytics")
#'
#' ## Developer sets consent directly assuming that he received approval in another way
#' ga_set_approval(consent = TRUE)
ga_set_approval <- function(message, consent = FALSE){
  ## Print out the message to ask for approval of sending data
  if(missing(message)){
    cat(galog$message, sep = "\n")
  }else{
    cat(message, sep = "\n")
  }
  ## If consent is set to TRUE, approval is set by the developer who is responsible
  ## If consent is set to FALSE and in interactive mode, request for approval
  if(consent == TRUE){
    consent <- ifelse(consent, "yes", "no")
  }else{
    if(interactive()) {
      consent <- readline(prompt="Is that ok for you (yes/no): ")
    }else{
      consent <- ifelse(consent, "yes", "no")
    }
  }
  ## Print out a note and set consent to TRUE/FALSE
  if(consent == "yes"){
    galog$settings$consent <- TRUE
    cat("Thank you for your consent to send usage data to Google Analytics")
  }else{
    galog$settings$consent <- FALSE
    cat("No consent given")
  }
  invisible(as.list(galog))
}

#### SETUP ####

#' Initialize tracking
#'
#' @param path Path to the file where the settings are saved (Optional). If provided, settings are loaded from that location. Further parameters are ignored
#' @param user_id  
#' @param tracking_id 
#' @param hostname 
#' @param consent 
#'
#' @return
#' @export
#'
#' @examples 
#' ga_initialize("~/galog.json")
#' ga_initalize(tracking_id="UA-XXXXXX-x,",user_id="Known user",hostname="www.foo.com",concent=TRUE)
ga_initialize <- function(path=NULL,tracking_id=NULL,user_id=NULL,hostname="TEMP",consent=TRUE){
  if(!is.null(path)){
    galog$settings <- ga_load_settings(path=path)
    #galog$user_id <- ga_set_user_id(galog$user_id) # Set userID
    #galog$client_id <- ga_set_client_id(client_id=NULL) #keep client IDs unique
    #galog$settings$tracking_id <- ga_set_tracking_id(galog$settings$tracking_id)
    #ga_set_hostname(hostname=galog$settings$hostname)
    #ga_set_approval(consent=galog$settings$consent)
    ga_set_url()
    ga_set_approval_message()
  } else{
    galog$user_id <- ga_set_user_id(user_id)
    galog$client_id <- ga_set_client_id(client_id=NULL) #keep client IDs unique
    galog$settings$tracking_id <- ga_set_tracking_id(tracking_id)
    galog$settings$hostname <- ga_set_hostname(hostname)
    ga_set_approval(consent=consent)
    ga_set_url()
    ga_set_approval_message()
  }
}
# ### Event collection ####
#' @title Send events to Google Analytics
#' @description Send events to Google Analytics.
#' If an event happens in your script, use this function to send the event to Google Analytics.
#' An event has a category, an action and optionally a label and a value can be set for the event. \cr
#' Possible use cases of this are sending when a user loads your package, sending when a user does some action on your shiny application,
#' storing when a user uses your R webservice, keeping information on the status of a long-running process, sending and error message ...\cr
#'
#' Events can be viewed in the Google Analytics > Behaviour > Events tab or in the Real-Time part of Google Analytics.
#' @param event_category a character string of length 1 with the category of the event
#' @param event_action a character string of length 1 with the action of the event
#' @param event_label a character string of length 1 with the label of the event. This is optional.
#' @param event_value a character string of length 1 with the value of the event. This is optional.
#' @return invisibly the result of a call to \code{\link[curl]{curl_fetch_memory}} which sends the data to Google Analytics
#' or an object of try-error if the internet is not working
#' @export
#' @examples
#' ga_set_tracking_id("UA-25938715-4")
#' ga_set_approval(consent = TRUE)
#'
#' ga_collect_event(event_category = "Start", event_action = "shiny app launched")
#' ga_collect_event(event_category = "Simulation",
#'                  event_label = "Launching Bayesian multi-level model",
#'                  event_action = "How many simulations", event_value = 10)
#' ga_collect_event(event_category = "Error",
#'                  event_label = "convergence failed", event_action = "Oh no")
#' ga_collect_event(event_category = "Error",
#'                  event_label = "Bad input", event_action = "send the firesquad", event_value=911)
ga_collect_event <- function(event_category="Start", event_action="default", event_label, event_value){
  # &ec=video        // Event Category. Required.
  # &ea=play         // Event Action. Required.
  # &el=holiday      // Event label.
  # &ev=300          // Event value.
  event_category <- curl::curl_escape(event_category)
  event_action <- curl::curl_escape(event_action)

  url <- sprintf("%s&t=event&ec=%s&ea=%s", galog$url, event_category, event_action)
  if(!missing(event_label)){
    event_label <- curl::curl_escape(as.character(event_label))
    url <- sprintf("%s&el=%s", url, event_label)
  }
  if(!missing(event_value)){
    event_value <- curl::curl_escape(as.character(event_value))
    url <- sprintf("%s&ev=%s", url, event_value)
  }
  req <- send(url)
  invisible(req)
}

#' @title Send pageviews to Google Analytics
#' @description Send pageviews to Google Analytics.
#' If someone visits a page, use this function to send the page and title of the page which is visited so that you can
#' easily see how users are using your application. \cr
#'
#' Pageviews can be viewed in the Google Analytics > Behaviour tab or in the Real-Time part of Google Analytics.
#'
#' @param page a character string with the page which was visited
#' @param title a character string with the title of the page which was visited
#' @param page_url a character string with the url of the page being visited
#' @param hostname a character string with the hostname. Defaults TEMP if not set
#'
#' @return invisibly the result of a call to \code{\link[curl]{curl_fetch_memory}} which sends the data to Google Analytics
#' or an object of try-error if the internet is not working
#' @export
#' @examples
#' ga_set_tracking_id("UA-25938715-4")
#' ga_set_approval(consent = TRUE)
#'
#' ga_collect_pageview(page = "/home")
#' ga_collect_pageview(page = "/simulation", title = "Mixture process")
#'
#' x <- ga_collect_pageview(page = "/home", title = "Homepage", hostname = "www.xyz.com")
#' x$status_code
ga_collect_pageview <- function(page_url=NULL,page=NULL, title=NULL, hostname=galog$settings$hostname, user_id=NULL){
  # For 'pageview' hits, either &dl or both &dh and &dp have to be specified for the hit to be valid.
  # dl	text	2048 Bytes	= http://foo.com/home?a=b //URL
  #
  # OR 
  #
  # dh	text	100 Bytes	 = foo.com // hostname
  # dp  text  2048 Bytes = /foo //page
  
  url <- sprintf("%s&t=pageview",galog$url)
  
  if(is.null(url)){
    hostname <- curl::curl_escape(as.character(hostname))
    page <- curl::curl_escape(as.character(page))
    url <-  sprintf("%s&dh=%s&dp=%s", url,hostname, page)
  }
  if(!is.null(title)){
    title <- curl::curl_escape(as.character(title))
    url <- sprintf("%s&dt=%s", url, title)
  }
  
  if(!is.null(url)){
    page_url <- curl::curl_escape(as.character(page_url))
    url <-  sprintf("%s&dl=%s", url, page_url) 
  }
  req <- send(url)
  invisible(req)
}

#' Send event using curl
#'
#' @param url url to be used to send
#'
#' @return TRUE/FALSE. Success of failure
#' @export
#'
#' @examples
send <- function(url){
  if(is.na(galog$settings$tracking_id)){
    stop("You forgot to set the tracking_id which looks like UA-XXXXXXXX-X, see ?ga_set_tracking_idor ?ga_save_settings")
  }
  if(galog$settings$consent){
    ## Send the data, put it in a try block to avoid the R program stops
    result <- try(curl_fetch_memory(url), silent = TRUE)
    invisible(result)
  }else{
    invisible()
  }
}


