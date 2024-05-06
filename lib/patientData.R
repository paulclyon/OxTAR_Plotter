## Functions which work on patientData

# Function to find a patient using ID and return the index number
# If not found it returns 0
findPatient <- function(ptID, ignoreIfArchived = TRUE)
{
  index <- which(patientData$id == ptID & (ignoreIfArchived == FALSE | patientData$archived == FALSE))
  
  if (length(index) == 0) {
    return(0)
  }
  
  return(index)
}

# Return true only if the patient is found and the patient is archived, otherwise false
patientArchived <- function(ptID)
{
  archived <- patientData$archived[patientData$id == ptID]
  if (length(archived) == 0) {
    return(FALSE)
  }
  return(archived)
}
