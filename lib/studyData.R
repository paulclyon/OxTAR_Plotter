## functions that primarily work on studyData

# Return true if the study is a valid OxTAR study with at least one patient, false otherwise
ifValidOxTARStudy <- function(stopIfNotValid = F) {
  if ("pt_rx_count" %in% colnames(studyData)) {
    if (nrow(patientData) > 0 ) {
      return(TRUE)
    }
  }
  # FIXME: Does this need to stop if not valid?
  return(FALSE)
}

# Return the indexed data otherwise return NA
# Note that NA may be returned if the field does not exist in the data
getDataEntry <- function(field, index, stopIfNotFound = F) {
  if (field %in% colnames(studyData)) {
    return(studyData[[field]][index])
  } 
  
  if (stopIfNotFound == T) {
    msg = paste("Field '", field, "' not found in the data - stopping", sep = '')
    logger(msg)
    stop(msg)
  }
  return(NA)
}