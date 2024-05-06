## Functions to load the castor API

# Connect to the Castor API...
connectCastorAPI <- function()
{
  logger("Attempting to open Castor API...") 
  castor_api <<- tryCatch(
    { CastorData$new(key =      Sys.getenv("CASTOR_USER_KEY"),
                     secret =   Sys.getenv("CASTOR_SECRET"),
                     base_url = Sys.getenv("CASTOR_URL"))
    },
    
    error=function(e) {
      logger(paste("An error occurred: Could not access Castor API..."))
      logger(e)
      castor_api <<- new.env() # Its an environment so we can't use is.na() function
    },
    
    #if a warning occurs, tell me the warning
    warning=function(w) {
      logger(paste("A warning occurred during access to Castor API"))
      logger(w)
    }
  )
  
  if (length(castor_api)==0)
  {
    logger("Failed to access API with those settings")
  }
  else
  {
    logger("Accessed Castor API successfully")
  }
  return(castor_api)
}

# Disconnect from the Castor API...
disconnectCastorAPI <- function()
{
  logger("Disconnecting from Castor API...")
  castor_api <<- new.env()
}