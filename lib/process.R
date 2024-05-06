# Uses global variables to reprocess the referral data
processData <- function() {
  # Crunch the Referral Data
  deceased = as.integer()
  iRef = as.integer()
  pt_ref_count = as.integer()
  pt_rx_count = as.integer()
  
  # Iterate through patient records one by one
  for (i in 1:nrow(patientData))
  {
    ptID <- patientData[["id"]][i]
    ptArchived <- patientData[["archived"]][i]
    
    if (ptArchived) {
      logger(paste("\n", i, "/", nrow(patientData), " Pt=", ptID, " Archived, skipping referral analysis...", sep = ""))
      next
    }
    
    logger(paste("\n", i, "/", nrow(patientData), " Pt=", ptID, " performing referral analysis...", sep = ""))
    sex <- getDataEntry("sex", i)
    birth_year <- getDataEntry("birth_year", i)
    if (!is.na(birth_year) && !is.integer(birth_year)) {
      logger(paste(i, "/", nrow(patientData), " Pt=", ptID, " **Data integrity issue**: patient birth year", birth_year, " is not a number.", sep = ""), T)
      birth_year <- NA
    }
    age_at_event <- NA
    pt_ref_count <- getDataEntry("pt_ref_count", i)
    pt_rx_count  <- getDataEntry("pt_rx_count", i)
    
    if (is.na(pt_ref_count)) {
      logger(paste("     Ref Count=NA, skipping from analysis...", sep = ""))
      next
    }
    
    if (pt_ref_count == 0) {
      logger(paste(i, "/", nrow(patientData), " Pt=", ptID, " Ref Count=0 ,skipping from analysis...", sep = ""))
      next
    }
    
    # Get survival status
    deceased <- getDataEntry("fu_deceased", i)
    if (is.na(deceased) ||
        deceased != 1) {
      # FIXME: We don't do fancy stuff looking for last scan or clinical interaction date yet...
      deceased <- as.integer(0)
    } else {
      # This is a very weird bug/thing but if we don't convert to a character first, there is an issue where 1 gets converted to 3
      # Something to do with the way the integer returns gets casted to integer or double
      deceased <- as.integer(as.character(deceased))
    }
    
    if (deceased == 1) {
      deceased_date <- getDataEntry("fu_deceased_date", i)
      deceased_date = convertToDate(deceased_date)
      if (!is.na(birth_year)) {
        age_at_event <- as.integer(format(asDateWithOrigin(deceased_date), "%Y")) - birth_year
      }
    } else {
      deceased_date <- NA
    }
    deceased_related <- getDataEntry("fu_deceased_related", i)
    
    if (is.na(deceased_related) || deceased_related != 1) {
      deceased_related <- 0
    }
    
    lost_to_fu_date <- NA
    lost_to_fu <- getDataEntry("fu_lost", i)
    
    if (!is.na(lost_to_fu) && lost_to_fu == 1) {
      lost_to_fu_date <- getDataEntry("fu_lost_date", i)
      lost_to_fu_date <- convertToDate(lost_to_fu_date)
      
      if (!is.na(birth_year)) {
        age_at_event <- as.integer(format(asDateWithOrigin(lost_to_fu_date), "%Y")) - birth_year
      }
      
      if (is.na(lost_to_fu_date)) {
        lost_to_fu <- 0
        lost_to_fu_date <- NA
        logger(paste(i, "/", nrow(patientData), " Pt=", ptID, " **Data integrity issue**: patient is lost to follow-up but date of lost-to-follow-up not specified or valid, thus cannot count as lost to follow-up.", sep = ""), T)
      }
    } else {
      lost_to_fu <- 0
      lost_to_fu_date <- NA
    }

    # Set the survival status
    # Censored observations are subjects who either die of causes other than the disease of interest or are lost to follow-up
    if (deceased != 1) {
      if (lost_to_fu != 1) {
        survival_status <- 0   # Alive and not lost to follow-up
      } else {
        survival_status <- 1   # Censored as lost to follow-up
      }
    } else if (deceased == 1) {
      if (deceased_related == 1) {
        survival_status <- 1   # Censored as died of something else
      } else {
        survival_status <- 2   # Dead
      }
    }
    
    survival_organ <- NA # Set this now as null, and check on each referral
    
    # Get recurrence status
    recurrenceJSON <- getDataEntry("fu_image_matrix", i)
    local_recurrence <- 0
    date_of_first_local_recurrence <- NA
    if (!is.na(recurrenceJSON) && str_length(recurrenceJSON) > 0)
    {
      recurrenceMatrix <- jsonlite::fromJSON(recurrenceJSON)
      recurrence.df <- data.frame(matrix(unlist(recurrenceMatrix), ncol = 7, byrow = T))
      colnames(recurrence.df) <- recurrenceColNames

      for (j in 1:nrow(recurrence.df))
      {
        # For each row in the matrix, check if there is any local recurrence, if so grab the first date of recurrence
        thisLR <- recurrence.df$local.recurrence[j]
        
        if (local_recurrence == 0 && !is.na(thisLR) &&thisLR != "N") {
          # Note if it is 'YA' means Yes and Ablatable Recurrence
          local_recurrence <- 1
          date_of_first_local_recurrence <- convertToDate(recurrence.df$imaging.date[j])
        }
      }
    }
    
    # Get these dates...
    date_of_diagnosis <- NA # Reset this each time we enter the referral loop
    date_of_first_rx  <- NA # Reset this each time we enter the referral loop
    
    if (pt_ref_count == 0)
    {
      logger(" No referrals for this patient")
    }
    
    # For each referral...
    for (iRef in 1:pt_ref_count)
    {
      ptForRx <- getDataEntry(paste("ref_intention_rx_", as.integer(iRef), sep = ""), i)
      ptOffPathway <- getDataEntry(paste("ref_off_rx_pathway_", as.integer(iRef), sep = ""), i)
      
      # Only continue if the patient is for treatment and is not off the pathway
      if (!is.na(ptForRx) & ptForRx == "1" & !is.na(ptOffPathway) & ptOffPathway == "0") {
        org <- getDataEntry(paste("ref_organ_", as.integer(iRef), sep = ""), i)
        organForRx = NA
        
        if (!is.na(org)) {
          organForRx <- switch(org,
            "LIV" = "Liver",
            "L" = "Lung",
            "K" = "Kidney",
            "A" = "Adrenal",
            "MO" = "Multiple Organs",
            "Other/Unspecified"
          ) #This is the catch-all default

          if (is.na(survival_organ)) {
            survival_organ <- organForRx
          } else if (survival_organ != organForRx) {
            # If this treatment is for a different organ, then we have multiple organs across
            # the multiple referrals
            survival_organ <- "Multiple Organs"
          }
        }
        
        ref_date <- convertToDate(getDataEntry(paste("ref_date_recd_", as.integer(iRef), sep = ""), i))

        if (is.na(ref_date)) {
          #If we don't have valid referral date received, use referral letter date as next best thing

          ref_date <- convertToDate(getDataEntry(paste("ref_letter_date_", as.integer(iRef), sep = ""), i))
          if (is.na(ref_date)) {
            logger(paste(i, "/", nrow(patientData), " Pt=", ptID, " **Data integrity issue**: patient is for treatment but doesn't have a valid referral date.", sep = ""), T)
          }
        }
        
        # Get the date of diagnosis which is the earliest referral date of all the referrals encountered
        if (!is.na(ref_date)) {
          if (!is.na(date_of_diagnosis)) {
            date_of_diagnosis <- convertToDate(date_of_diagnosis)
            date_of_diagnosis <- dplyr::if_else(
              asDateWithOrigin(ref_date) < asDateWithOrigin(date_of_diagnosis),
              ref_date,
              date_of_diagnosis
            )
          } else {
            date_of_diagnosis <- ref_date
          }
        }
        
        # Sort DTT if we have a valid referral rec'd or referral original date...
        if (!is.na(ref_date)) {
          ref_dtt_date <- convertToDate(getDataEntry(paste("ref_date_dtt_", as.integer(iRef), sep = ""), i))
          
          if (!is.na(ref_dtt_date)) {
            ref_dtt_days <- as.numeric(difftime(ref_dtt_date, ref_date, units = "days"), units = "days")
          } else {
            ref_dtt_days <- NA
          }
          
          # Get provisional rx date, NA if not set
          ref_provisional_rx_date <- convertToDate(getDataEntry(paste("ref_date_provisional_rx_", as.integer(iRef), sep = ""), i))

          # Sort Clock Stop
          # Parse the JSON and get it into a nice data frame
          refclockstop.df <- NA
          clockstoppedDaysPreDTT <- 0
          clockstoppedDaysPostDTT <- 0
          clockstoppedReason <- ""
          clockStopJSON <- getDataEntry(paste("ref_clock_stop_", as.integer(iRef), sep = ""), i)
          
          if (!is.na(clockStopJSON) && str_length(clockStopJSON) > 0 )
          {
            clockStopMatrix <- jsonlite::fromJSON(clockStopJSON)
            refclockstop.df <- data.frame(matrix(unlist(clockStopMatrix), ncol = 4, byrow = T))
            colnames(refclockstop.df) <- clockStopColNames
            
            for (j in 1:nrow(refclockstop.df))
            {
              
              # For each row in the matrix
              dateClockstopped <- convertToDate(refclockstop.df$date.stopped[j])
              dateClockRestart <- convertToDate(refclockstop.df$date.restart[j])
              
              if (!is.na(dateClockstopped)) {
                if (is.na(dateClockRestart)) {
                  # Make the restart date today if there is no useful restart date
                  dateClockRestart = Sys.Date()
                  virtualRestart = T # This is just so we can put an asterix if it hasn't been restarted yet, virtual stop
                } else {
                  virtualRestart = F
                }
                
                # If we have stopped the clock before the DTT...
                if (!is.na(ref_dtt_date)) {
                  if (dateClockstopped < ref_dtt_date) {
                    clockstoppedDaysPreDTT <- clockstoppedDaysPreDTT +
                      as.integer(difftime(dateClockstopped, dateClockRestart, units = "days"), units = "days")
                  } else {
                    clockstoppedDaysPostDTT  = clockstoppedDaysPostDTT +
                      as.integer(difftime(dateClockstopped, dateClockRestart, units = "days"), units = "days")
                  }
                }

                clockstoppedReason <- paste(clockstoppedReason, " ", j, ":", refclockstop.df$reason[j], sep = "")
                
                if (virtualRestart == T) {
                  clockstoppedReason <- paste(clockstoppedReason, "*", sep = "")
                }
              }
            }
          }
          
          if (clockstoppedDaysPreDTT != 0 || clockstoppedDaysPostDTT != 0) {
            clockstoppedReason <- paste("\nClock Stops:", clockstoppedReason, " (", clockstoppedDaysPreDTT, " days pre-DTT, ", clockstoppedDaysPostDTT, " days post-DTT)", sep = "")
          }
          
          if (clockstoppedDaysPreDTT == 0) {
            clockstoppedDaysPreDTT = NA # Don't plot all the zeros, only if it is non-zero
          }
          
          if (clockstoppedDaysPostDTT == 0) {
            clockstoppedDaysPostDTT = NA # Don't plot all the zeros, only if it is non-zero
          }
          
          # Sort Rx Date. Note used to use the ref_date_rx_i field but is is a calculation of anaest_date_i
          # and had some issues with it being a lot of NAs but not all NAs (weird), anyway went for the source
          # and it works much better!
          ref_rx_date <- convertToDate(getDataEntry(paste("anaes_date_", as.integer(iRef), sep = ""), i))
          
          if (!is.na(ref_rx_date)) {
            ptRefTreated = T # We have referred and treated...
            ref_rx_days = as.numeric(difftime(ref_rx_date, ref_date, units = "days"), units = "days")
            
            # Correct for clockstop
            if (!is.na(clockstoppedDaysPreDTT)) {
              ref_dtt_days <- ref_dtt_days + clockstoppedDaysPreDTT # Remember clockstoppedDaysPostDTT is -ve
              ref_rx_days  <- ref_rx_days  + clockstoppedDaysPreDTT # Remember clockstoppedDaysPreDTT is -ve
            }
            if (!is.na(clockstoppedDaysPostDTT)) {
              ref_rx_days <- ref_rx_days + clockstoppedDaysPostDTT # Remember clockstoppedDaysPreDTT is -ve
            }
            
            if (!is.na(ref_dtt_days)) {
              ref_dtt_rx_days <- ref_rx_days - ref_dtt_days
            } else {
              ref_dtt_rx_days <- NA
            }
            
            # Update date of first rx
            if (!is.na(date_of_first_rx)) {
              date_of_diagnosis <- dplyr::if_else(
                asDateWithOrigin(ref_rx_date) < asDateWithOrigin(date_of_first_rx),
                ref_rx_date,
                date_of_first_rx
              )
            } else {
              date_of_first_rx <- ref_rx_date
            }
            
            rxdone_pt_list                     <<- append(rxdone_pt_list,                       paste(ptID, "-", iRef, sep = ""))
            rxdone_organ_list                  <<- append(rxdone_organ_list,                    organForRx)
            rxdone_refdate_list                <<- append(rxdone_refdate_list,                  ref_date)
            rxdone_dttdate_list                <<- append(rxdone_dttdate_list,                  ref_dtt_date)
            rxdone_rxdate_list                 <<- append(rxdone_rxdate_list,                   ref_rx_date)
            rxdone_dtt_days_list               <<- append(rxdone_dtt_days_list,                 ref_dtt_days)    # Days from Ref to DTT
            rxdone_rx_days_list                <<- append(rxdone_rx_days_list,                  ref_rx_days)     # Days from Ref to Rx
            rxdone_dtt_rx_days_list            <<- append(rxdone_dtt_rx_days_list,              ref_dtt_rx_days) # Days from DDT to Rx
            rxdone_clockstop_days_predtt_list  <<- append(rxdone_clockstop_days_predtt_list,    clockstoppedDaysPreDTT)
            rxdone_clockstop_days_postdtt_list <<- append(rxdone_clockstop_days_postdtt_list,   clockstoppedDaysPostDTT)
            rxdone_clockstop_reason_list       <<- append(rxdone_clockstop_reason_list,         clockstoppedReason)
          } else {
            # We have not treated so we can use the difference from current date and referral
            ptRefTreated = F
            ref_rx_days                  <- difftime(Sys.Date(), ref_date, units = "days")

            if (!is.na(clockstoppedDaysPreDTT)) {
              ref_rx_days <- ref_rx_days + clockstoppedDaysPreDTT  # Remember clockstoppedDaysPreDTT and clockstoppedDaysPostDTT are -ve
            }

            if (!is.na(clockstoppedDaysPostDTT)) {
              ref_rx_days <- ref_rx_days + clockstoppedDaysPostDTT  # Remember clockstoppedDaysPreDTT and clockstoppedDaysPostDTT are -ve
            }
            
            rxwait_organ_list                  <<- append(rxwait_organ_list,                  organForRx)
            rxwait_pt_list                     <<- append(rxwait_pt_list,                     paste(ptID, "-", iRef, sep = ""))
            rxwait_ref_date_list               <<- append(rxwait_ref_date_list,               ref_date)
            rxwait_provisional_rxdate_list     <<- append(rxwait_provisional_rxdate_list,     ref_provisional_rx_date)
            rxwait_dtt_days_list               <<- append(rxwait_dtt_days_list,               ref_dtt_days)       # Days from Ref to DTT
            rxwait_days_list                   <<- append(rxwait_days_list,                   ref_rx_days)        # Days from Ref to Today
            rxwait_clockstop_days_predtt_list  <<- append(rxwait_clockstop_days_predtt_list,  clockstoppedDaysPreDTT)
            rxwait_clockstop_days_postdtt_list <<- append(rxwait_clockstop_days_postdtt_list, clockstoppedDaysPostDTT)
            rxwait_clockstop_reason_list       <<- append(rxwait_clockstop_reason_list,       clockstoppedReason)
          }
          
          # We only care about recording survival if we have treated the patient
          if (!is.na(date_of_first_rx)) {
            if (deceased == 1) {
              if (!is.na(deceased_date)) {
                survival_days <- as.numeric(difftime(deceased_date, date_of_first_rx, units = "days"), units = "days")
              } else {
                # If deceased but no valid deceased date, log it and use current date as deceased date as best guess
                survival_days <- as.numeric(difftime(deceased_date, Sys.Date(), units = "days"), units = "days")
                logger(paste(i, "/", nrow(patientData), " Pt=", ptID, " **Data integrity issue**: patient is deceased but date of death not specified or valid, assuming today.", sep = ""))
              }
            } else {
              # The patient is still alive (or status has not been recorded, assume alive)
              survival_days <- as.numeric(difftime(Sys.Date(), date_of_first_rx, units = "days"), units = "days")
            }
          } else {
            survival_days <- NA
          }
          
          local_recurrence_days <- NA   # FIXME check this - should we also have local_recurrence_free_days?
          if (!is.na(date_of_first_local_recurrence) && isConvertibleToDate(date_of_first_local_recurrence)) {
            local_recurrence_days = as.numeric(difftime(date_of_first_local_recurrence, date_of_first_rx, units = "days"), units = "days")
          }
          
          logger(paste("     Ref=", iRef, "/", pt_ref_count, 
                       " (", organForRx, ") treated=", ptRefTreated, " ref-date=", ref_date,
                       " dtt-days=", ref_dtt_days, " rx-days=", ref_rx_days, 
                       dplyr::if_else(!is.na(ref_rx_date), "", "(*)", missing = ""),
                       " survival-1st-rx=", survival_days,
                       dplyr::if_else(deceased == 1, " (deceased)", " (alive)", missing = " (alive)"),
                       sep = ""))
        }
      } else {
        logger(paste("     Ref=", iRef, "/", pt_ref_count,
                     " (", organForRx, ") is not currently selected for Rx, skipping...", sep = ""))
      }
    }

    # Now we have been through all the referrals update the survival data...
    if (!is.na(date_of_first_rx))
    {
      survival_pt_list          <<- append(survival_pt_list, ptID)
      survival_sex_list         <<- append(survival_sex_list, sex)
      survival_age_list         <<- append(survival_age_list, age_at_event)
      survival_organ_list       <<- append(survival_organ_list, survival_organ)
      survival_first_rx_date    <<- append(survival_first_rx_date, date_of_first_rx)
      survival_deceased_list    <<- append(survival_deceased_list, as.integer(deceased))
      survival_deceased_date    <<- append(survival_deceased_date, deceased_date)
      survival_days_list        <<- append(survival_days_list, survival_days)
      survival_deceased_related <<- append(survival_deceased_related, deceased_related)
      survival_lost_to_fu       <<- append(survival_lost_to_fu, lost_to_fu)
      survival_lost_to_fu_date  <<- append(survival_lost_to_fu_date, lost_to_fu_date)
      survival_status_list      <<- append(survival_status_list, survival_status)
      
      # And the survival data
      local_recurrence_list        <<- append(local_recurrence_list, local_recurrence)
      local_recurrence_date_list   <<- append(local_recurrence_date_list, date_of_first_local_recurrence)
      local_recurrence_days_list   <<- append(local_recurrence_days_list, local_recurrence_days)
    }
  }
  
  # The as.Dates() work around is needed to set Dates as the column types otherwise if the first element in the column is NA, it is represented as just the number which is still the date but unredable to the human
  # This is important as this makes them all a Dates object which displays nicely in the tables - you can check the types of the data frame easily with str(survivalData)
  if (!is.null(rxdone_pt_list))
  {

    # If the list is not empty
    rxDoneData <<- data.frame(
      ID = rxdone_pt_list,
      RefDate = asDateWithOrigin(rxdone_refdate_list),
      DTTDate = asDateWithOrigin(rxdone_dttdate_list),
      RxDate = asDateWithOrigin(rxdone_rxdate_list),
      Ref_DTT = as.numeric(rxdone_dtt_days_list),
      DTT_Rx = as.numeric(rxdone_dtt_rx_days_list),
      Ref_RxDone = as.numeric(rxdone_rx_days_list),
      Organs = rxdone_organ_list,
      ClockStopDaysPreDTT = as.numeric(rxdone_clockstop_days_predtt_list),
      ClockStopDaysPostDTT = as.numeric(rxdone_clockstop_days_postdtt_list),
      ClockStopWhy = rxdone_clockstop_reason_list
    )
  } else {
    rxDoneData <<- NA
  }
  
  if (!is.null(rxwait_pt_list)) {
    # If the list is not empty
    rxWaitData <<- data.frame(
      ID = rxwait_pt_list,
      RefDate = asDateWithOrigin(rxwait_ref_date_list),
      ProvisionalRxDate = asDateWithOrigin(rxwait_provisional_rxdate_list),
      Ref_DTT = as.numeric(rxwait_dtt_days_list),
      DaysWaiting = as.numeric(rxwait_days_list),
      Organs = rxwait_organ_list,
      ClockStopDaysPreDTT = as.numeric(rxwait_clockstop_days_predtt_list),
      ClockStopDaysPostDTT = as.numeric(rxwait_clockstop_days_postdtt_list),
      ClockStopWhy = rxwait_clockstop_reason_list
    )
  } else {
    rxWaitData <<- NA
  }
  
  # Here Time is the status is 0=alive, 1=censored, 2=dead, survival time in days (since first Rx)
  survivalData <<- data.frame(
    ID = survival_pt_list,
    Sex = survival_sex_list,
    Age = survival_age_list,
    Organ = survival_organ_list,
    Status = survival_status_list,
    FirstRxDate = asDateWithOrigin(survival_first_rx_date),
    Deceased = survival_deceased_list,
    DeceasedDate = asDateWithOrigin(survival_deceased_date),
    Time = survival_days_list,
    Related = survival_deceased_related,
    LostToFU = survival_lost_to_fu,
    LostToFUDate = asDateWithOrigin(survival_lost_to_fu_date)
  )
  
  # The recurrence stuff can also borrow some columns from the survival data...
  recurrenceData <<- data.frame(
    ID = survival_pt_list,
    Sex = survival_sex_list,
    Age = survival_age_list,
    Organ = survival_organ_list,
    Status = NA,
    FirstRxDate = asDateWithOrigin(survival_first_rx_date),
    LocalRecurrence = local_recurrence_list,
    FirstLocalRecurrenceDate = local_recurrence_date_list,
    Time = local_recurrence_days_list
  )
  
  # This is just a list the different organ targets which have been referred or treated
  organFactors <<- levels(factor(append(rxdone_organ_list, rxwait_organ_list)))
  
  #survivalFit         <- survfit(Surv(Time, Status)~1, data=survivalData)
  survivalFitSex      <<- survfit(Surv(Time, Status) ~ Sex, data = survivalData)
  survivalFitOrgan    <<- survfit(Surv(Time, Status) ~ Organ, data = survivalData)
  #recurrenceFitOrgan <<- survfit(Surv(Time, Status)~Organ, data=recurrenceData)
  
  #summary(survivalFit)
  survivalPlotOrgan   <<- ggsurvplot(survivalFitOrgan,
                                     xlab = "Time (Days)",
                                     ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  
  survivalPlotSex     <<- ggsurvplot(survivalFitSex,
                                     xlab = "Time (Days)",
                                     ggtheme = theme(plot.title = element_text(hjust = 0.5)))
  
  #recurrencePlotOrgan <<- ggsurvplot(recurrenceFitOrgan,xlab="Follow-up Time (Days)",ggtheme=theme(plot.title=element_text(hjust=0.5)))
  
  recurrencePlotOrgan <<- NA
}

