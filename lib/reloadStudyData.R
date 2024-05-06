# reloadStudyData.R - functions to load/reload study data from Castor
# 
# The castorApi global needs to be present and set


# Return an array of study name strings
getStudyNames <- function()
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return()
  }
  
  studies <- castor_api$getStudies()
  studies$name
}



# Find study by name and return index of it, if not found return 0
findStudyIndex <- function(studies, studyName, stopIfNotFound=F) {
  index <- which(studies$name == studyName)
  if (length(index) == 0) {
    msg <- paste("Could not find study with name", studyName)
    if (stopIfNotFound==T) {
      stop(msg)
    }
    
    logger(msg)
    return(0)
  }

  logger(paste("Found study '", studyName,"' Index=", index, sep=''))
  return(index)
}

getPatientData <- function(studyID) {
  patientData <- (studyID)
  return(patientData)
}

getStudyData <- function(studyID) {
  studyData <- castor_api$getStudyData(studyID)
  return(studyData)
}

# Get the study name for a particular study ID, NA if not found
getStudyName <- function(id)
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return(NA)
  }
  studies <- castor_api$getStudies()
  
  index <- which(studies$study_id == id)
  if (length(index) == 0) {
    return(NA)
  }
  
  return(studies$name[[index]])
}

# Pull in the patient data...
pullPatientData <- function (studyID) {
  studyName <- getStudyName(studyID)
  logger(paste("Loading patient data for study ID '", studyID,"' (",studyName,")",sep=""))
  patientData <- castor_api$getRecords(studyID)
  logger (paste ("Found ", nrow(patientData), " patients in study ID '",studyID,"' (",studyName,")", sep=""))
  return(patientData)
}

# Pull in the study data...
pullStudyData <- function (studyID) {
  studyName = getStudyName(studyID)
  logger(paste("Loading study data for study ID '", studyID,"' (",studyName,")...",sep=""))
  studyData <- getStudyData(studyID)
  logger(paste("Study data loaded successfully"))
  logger(paste ("Found ", nrow(studyData), " rows of data in studyID '...",studyID,"' (",studyName,")", sep=""))
  return(studyData)
}


# reloadStudyData interrogates castor to load the study data.
# It sets the global values:
#
# patientData 
# studyData
reloadStudyData <- function(studyName)
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return()
  }
  
  if(is.na(studyName)) {
    logger("No study with the name '",studyName,"' - cannot load study...")
    return()
  }
  
  studies <- castor_api$getStudies()
  studyIndex <- findStudyIndex(studies, studyName)
  if (studyIndex==0)
  {
    logger(paste("Could not find study '", studyName, "' to load...",sep=""))
    return()
  }
  
  # Pull in the data for the OxTAR study
  oxtar_study_id   <- studies[["study_id"]][studyIndex]
  oxtar_study_name <- studies[["name"]][studyIndex]
  patientData      <<- pullPatientData(oxtar_study_id)
  studyData        <<- pullStudyData(oxtar_study_id)
}

