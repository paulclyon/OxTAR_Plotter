## Source all the files in lib/ (These will be listed in an alphanumerical order - files starting with "_" will be read first)
file.sources <- list.files(
  paste(dirname(sys.frames()[[length(sys.frames()) - 3]]$ofile), 'lib/', sep = '/'),
  pattern = '*.R',
  full.names = T,
  ignore.case = T
)

logger(file.sources)
sapply(file.sources, source, .GlobalEnv) 

if (require(knitr)) {
  mtime <- function(files) {
    lapply(Sys.glob(files), function(x) file.info(x)$mtime)
  }
  
  # This allows knitr to know it needs to re-calculate data if these files change
  knitr::opts_chunk$set(cache.extra = mtime(file.sources))
}

makeRxPathwayPlots <- function()
{
  if (!is.null(nrow(rxDoneData)))
  {  
    rxdonePlotColors <<- c(
      "Ref to DTT"           = "red",
      "DTT to Rx"            = "yellow",
      "Ref to Rx"            = "green",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan"
    )
    rxdonePlot <<- ggplot(rxDoneData, aes(x=RxDate, text=paste(ID, ' (',Organs,')\n', ClockStopWhy),sep='')) +
      geom_point( aes(y=Ref_DTT,              color="Ref to DTT")) + 
      geom_point( aes(y=DTT_Rx,               color="DTT to Rx")) + 
      geom_point( aes(y=Ref_RxDone,           color="Ref to Rx")) +
      geom_point( aes(y=ClockStopDaysPreDTT,  color="Clock Stops Pre-DTT")) +
      geom_point( aes(y=ClockStopDaysPostDTT, color="Clock Stops Post-DTT")) +
      theme(legend.position="bottom") +
      scale_color_manual(values = rxdonePlotColors) +
      guides(color=guide_legend("Treated Patients...")) +
      labs(y="Number of Days") +
      ggtitle("Time to Treatment")
  }
  else
  {
    rxdonePlot <<- NA
  }
  
  if (!is.null(nrow(rxWaitData)))
  {
    rxwaitPlotColors <<- c(
      "Days to DTT"          = "blue",
      "Days Waiting"         = "red",
      "Clock Stops Pre-DTT"  = "purple",
      "Clock Stops Post-DTT" = "cyan"
    )
    rxwaitPlot <<- ggplot(rxWaitData, aes(x=RefDate, text=paste(ID, ' (',Organs,')\n', 'Provisional RxDate=', ProvisionalRxDate, ClockStopWhy, sep=""))) +
      geom_point( aes(y=Ref_DTT,             colour="Days to DTT")) + 
      geom_point( aes(y=DaysWaiting,         colour="Days Waiting")) +
      geom_point( aes(y=ClockStopDaysPreDTT, color="Clock Stops Pre-DTT")) +
      geom_point( aes(y=ClockStopDaysPostDTT,color="Clock Stops Post-DTT")) +
      theme(legend.position="bottom") +
      scale_color_manual(values = rxwaitPlotColors) +
      guides(color=guide_legend("Patients with DTT...")) +
      labs(y="Number of Days") +
      ggtitle("Time on Waiting List")  
  }
  else
  {
    rxwaitPlot <<- NA
  }
}
  
makeRxPathwayPies <- function(inputStartDate, inputEndDate, inputOrganList)
{
  startDate=as.Date(inputStartDate, )
  endDate=as.Date(inputEndDate, format = "%d/%m/%Y")
  
  logger(paste("FIXME: Need to filter the audit report to get it from ",startDate,endDate))
  logger(paste("FIXME: Need to filter the audit report to get just organs ",inputOrganList))  
  # Filter just the dates we need from rxDoneData
  rxDoneData.filtered <- rxDoneData
  
  #rxDoneData.filtered <- rxDoneData %>% filter(between(RxDate, as.Date(inputStartDate, format = "%d/%m/%Y"), 
  #                                                             as.Date(inputEndDate, format = "%d/%m/%Y")))
  
  #rxDoneData.filtered <- rxDoneData.filtered %>% filter(Organs %in% inputOrganList)
  
  organFactors.filtered = levels(rxDoneData$Organs)
  
  # Get the data for the Organ Pie Chart
  organCounts <<- c()
  organPercents <<- c()
  for (organ in organFactors)
  {
    logger(organ)
    count = length(which(rxDoneData$Organs==organ))
    organCounts <<- c(organCounts,count)
    organPercents <<- c(organPercents,count)
  }
  
  treatedTotal=sum(organCounts)
  organPercents <<- organPercents/treatedTotal
  
  organPie.df <<- data.frame(
    Organs = organFactors,
    OrganRxCounts = organCounts,
    OrganRxPercents = organPercents
  )
  
  # Make and label the pie chart
  organPie.df2 <<- organPie.df %>% 
    mutate(csum = rev(cumsum(rev(OrganRxCounts))), 
           pos = OrganRxCounts/2 + lead(csum, 1),
           pos = if_else(is.na(pos), OrganRxCounts/2, pos))
           
  rxdonePie <<- ggplot(organPie.df2, aes(x = "", y = OrganRxCounts, fill = fct_inorder(Organs))) + 
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Pastel1") +
    geom_label_repel(data = organPie.df2,
                     aes(y = pos,
                         label = glue::glue("{OrganRxCounts} ({scales::percent(organPercents)})"), 
                         fill = Organs),
                     size = 4, nudge_x = 3, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Group")) +
    theme_void()
}

  
