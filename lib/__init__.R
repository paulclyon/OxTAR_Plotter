# __init__.R contains common functions that other function files may require.

# logger function if stderr = TRUE then it goes to stderr, otherwise stdout, may want to update this to use the ShinyApp
logger <- function(msg, stderr=FALSE) {
  if (Sys.getenv("DEBUG_MODE") == T)
  {
    if (!is.na(stderr) && stderr!=T)
    {
      write(toString(msg),stdout())
    }
    else
    {
      write(toString(msg),stderr())
    }
  }
  return()
}

# This is a wrapper for the as.Date function but it sets the origin
# This is because on some OS e.g. Linux, R may be more strict and fall over if origin is not set
asDateWithOrigin <- function(thisDate)
{
  as.Date(thisDate, origin = '1970-01-01')
}

# initialiseGlobals sets all of the default values

initialiseGlobals <- function() {
  
  # This is the order of the clock stop columns
  clockStopColNames              <<- c("date.stopped","date.restart","reason","free.text")
  recurrenceColNames             <<- c("imaging.date","exam.type","local.recurrence","new.in.target.organ","distant.progression","disease.status","free-text")
  rxdone_organ_list              <<- c()
  rxdone_pt_list                 <<- c()
  rxdone_refdate_list            <<- c()
  rxdone_dttdate_list            <<- c()
  rxdone_rxdate_list             <<- c()
  rxdone_dtt_days_list           <<- c()
  rxdone_rx_days_list            <<- c()
  rxdone_dtt_rx_days_list        <<- c()
  rxwait_organ_list              <<- c()
  rxwait_pt_list                 <<- c()
  rxwait_ref_date_list           <<- c()
  rxwait_provisional_rxdate_list <<- c()
  rxwait_dtt_days_list           <<- c()
  rxwait_days_list               <<- c()
  rxwait_clockstop_days_predtt_list  <<- c()
  rxwait_clockstop_days_postdtt_list <<- c()
  rxwait_clockstop_reason_list       <<- c()
  rxdone_clockstop_days_predtt_list  <<- c()
  rxdone_clockstop_days_postdtt_list <<- c()
  rxdone_clockstop_reason_list   <<- c()
  survival_pt_list               <<- c()
  survival_sex_list              <<- c()
  survival_age_list              <<- c()
  survival_organ_list            <<- c()
  survival_first_rx_date         <<- c()
  survival_days_list             <<- c()
  survival_deceased_list         <<- c()
  survival_deceased_date         <<- c()
  survival_deceased_related      <<- c()
  survival_lost_to_fu            <<- c()
  survival_lost_to_fu_date       <<- c()
  survival_status_list           <<- c()
  local_recurrence_list          <<- c()
  local_recurrence_date_list     <<- c()
  local_recurrence_days_list     <<- c()
  
  survivalData                 <<- NA
  recurrenceData               <<- NA
  organFactors                 <<- c()
  organCounts                  <<- c()
  organPercents                <<- c()
  Organs                       <<- NA
  OrganPercents                <<- NA
  organPie.df                  <<- NA
  rxDoneData                   <<- NA
  rxWaitData                   <<- NA
  refclockstop.df              <<- NA
  recurrence.df                <<- NA
  rxdonePlot                   <<- ggplot()
  rxwaitPlot                   <<- ggplot()
  rxdonePie                    <<- ggplot()
  rxwaitPie                    <<- ggplot()
  patientData                  <<- NA
  studyData                    <<- NA
  survivalPlotSex              <<- ggplot()
  survivalPlotOrgan            <<- ggplot()
  survivalFitSex               <<- NA
  survivalFitOrgan             <<- NA
  
  logger("System information:")
  logger(Sys.info(), TRUE)
}
