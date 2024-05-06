# clipboard.R contains functions to help copy to the clipboard
copyDataToClipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE, quote=TRUE, ...) {
  if (require(clipr)) {
    if (clipr_available()) {
      write_clip(x,
                 sep=sep,
                 col.names = col.names,
                 row.names = row.names,
                 quote=quote,
                 ...)
    } else {
      logger("clipboard not available")
    }
  } else {
    logger("clipr not installed - run install.packages('clipr')")
    switch(Sys.info()[['sysname']],
           Windows= {logger("I'm a Windows PC, don't know how to handle copy/paste.")},
           Linux  = {
             con <- pipe("xclip -selection clipboard -i", open="w")
             write.table(x, con, sep=sep, row.names=row.names, col.names=col.names, quote=quote, ...)
             close(con)
           },
           Darwin = {  
             write.table(x,
                         file = pipe("pbcopy"),
                         sep=sep,
                         col.names = col.names,
                         row.names = row.names,
                         quote = quote, 
                         ...)
           })
  }
}

copyDataToExcelClipboard <- function(x, row.names=FALSE, col.names=TRUE, ...) {
  write.table(x,
              "clipboard",
              sep="\t",
              row.names=row.names,
              col.names=col.names,
              ...)
}

