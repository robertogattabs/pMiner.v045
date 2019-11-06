#' A class to perform a preliminary analysis on sequential data for Process Mining issues
#'
#' @description   This class aims at inspecting an event-log for descriptive analysis purposes. The public methods are:
#'                \itemize{
#'                \item \code{logInspector( ) } is the constructor of the class
#'                \item \code{loadDataset( ) } loads data taken from a \code{dataLoader::getData()} method, into a \code{logInspector()} object
#'                \item \code{getEventStats() } computes and returns event-related stats, such as absolute and relative events frequency
#'                \item \code{getProcessStats() } computes and returns process-related stats, such as absolute and relative processes frequency
#'                }
#'                In order to better undestand the use of such methods, please visit: www.pminer.info
#'
#' Parameter for \code{logInspector::plotEventStats()} and \code{logInspector::plotProcessStats()} methods is:
#'   \itemize{
#'    \item \code{num } the number of most frequent events/processes to plot
#'   }
#' @export
#' @examples \dontrun{
#' # -----------------------------------------------
#' #  USING THE METHODS of the class
#' # -----------------------------------------------
#' obj.L<-dataLoader();   # create a Loader
#'
#' # Load a .csv using "DES" and "ID" as column names to indicate events
#' # and Patient's ID
#' obj.L$load.csv(nomeFile = "../otherFiles/test_02.csv",
#' IDName = "ID",EVENTName = "DES",dateColumnName = "DATA")
#'
#' # now create an object logInspector
#' obj.logI<-logInspector();
#'
#' # load the data into logInspector object
#' obj.logI$loadDataset( obj.L$getData() );
#'
#' # get event-related descriptive statistics
#' obj.logI$getEventStats();
#'
#' # get process-related descriptive statistics
#' obj.logI$getProcessStats();
#' }
logInspector <- function() {
  eventType <-''
  processInstances <-''
  numberOfDifferentEvents <- ''
  numberOfTotalEvents <- ''
  processInstances.toSymbol <- ''
  loaded.data<-c();
  #=================================================================================
  # clearAttributes
  #=================================================================================
  clearAttributes<-function() {
    costructor();
  }
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( dataList ) {
    # Clear some possible previously inserted attribute
    clearAttributes()
    # set the new attributes
    eventType <<- dataList$arrayAssociativo
    processInstances <<- dataList$wordSequence.raw
    loaded.data<<-dataList
  }
  #===========================================================
  # getEventStats
  #===========================================================
  getEventStats<-function() {
    
    # some useful variable definitions (unlist everything: don't care about processe instances)
    
    numberOfDifferentEvents <- length(eventType)
    allEvents <- unlist(processInstances)
    numberOfTotalEvents <- length(allEvents)
    countEventOccurrence <- numeric()
    arr.max.relative.observation.time<-numeric()
    
    # calculate how frequent an event occour in patient's log (1 patient counts 1)
    
    bigList<-loaded.data$pat.process
    nome.colonna.eventi<-loaded.data$csv.EVENTName
    
    eventi<-c()
    for( IdPat in names(bigList)){
      eventi<-c(eventi,as.character(unique(bigList[[IdPat]][,nome.colonna.eventi])))
      arr.max.relative.observation.time<-c(arr.max.relative.observation.time,bigList[[IdPat]]$pMineR.deltaDate[nrow(bigList[[IdPat]])])
    }
    names(arr.max.relative.observation.time)<-as.character(names(bigList))
    coverage.tmp<-table(eventi)
    coverage<-as.vector(coverage.tmp)
    names(coverage)<-names(coverage.tmp)
    
    
    # check each event name against all log events and count occurrences
    
    for(i in 1:length(eventType)){
      countEventOccurrence[i] <- length(grep(eventType[i], allEvents))
    }
    names(countEventOccurrence) <- eventType
    
    # build output structures and fill them
    distribution.abs <- sort(countEventOccurrence, decreasing=TRUE)
    distribution.perc <- distribution.abs/numberOfTotalEvents
    eventStats <- list("Number of different event types" = numberOfDifferentEvents,
                       "Total number of events" = numberOfTotalEvents,
                       "Absolute event occurrence" = distribution.abs,
                       "Percentual event occurrence" = distribution.perc,
                       "Absolute Coverage" = coverage,
                       "arr.max.relative.observation.time"= arr.max.relative.observation.time
    )
    return(eventStats)
  }
  
  #===========================================================
  # getProcessStats
  #===========================================================
  getProcessStats<-function() {
    # 1)associate each event name to a letter, and 2) define variables
    eventType.toSymbol <- paste(letters[1:length(eventType)])
    processInstances.toSymbol <- vector("list", length(processInstances))
    processInstances.toSymbolCollapsed <- vector("list", length(processInstances))
    processInstances.toSymbolCollapsedVector <- character()
    
    
    # substitute names with letters, then collapse into a singol string for each process instance
    for (k in 1:length(processInstances)){
      processInstances.toSymbol[[k]] <- vector(mode="character", length(processInstances[[k]]))
      for (i in 1:length(processInstances[[k]])){
        for (j in 1:length(eventType)){
          if (processInstances[[k]][i]==eventType[j]) {processInstances.toSymbol[[k]][i] <- eventType.toSymbol[j]}
        }
      }
      processInstances.toSymbolCollapsed[[k]] <- paste(processInstances.toSymbol[[k]], sep="", collapse="")
    }
    
    processInstances.toSymbolCollapsedVector <- sapply(processInstances.toSymbolCollapsed,unlist)
    processDistributionTable.abs <- sort(table(processInstances.toSymbolCollapsedVector), decreasing=TRUE)
    
    # build output structures and fill them
    
    processDistribution.abs <- data.frame(matrix(ncol = 3, nrow = length(processDistributionTable.abs)))
    symbolToEventConversion <- data.frame(matrix(ncol = 2, nrow = length(eventType)))
    names(processDistribution.abs) <- c("Process signature", "Absolute frequency", "Relative frequency")
    names(symbolToEventConversion) <- c("Event name","Event symbol")
    
    processDistribution.abs[,1] <- names(processDistributionTable.abs)
    processDistribution.abs[,2] <- as.vector(processDistributionTable.abs)
    processDistribution.abs[,3] <- as.vector(processDistributionTable.abs)/length(processInstances)
    
    symbolToEventConversion[,1] <- eventType
    symbolToEventConversion[,2] <- eventType.toSymbol
    
    processStats <- list("Absolute frequency dataframe" = processDistribution.abs[,c(1,2)],
                         "Relative frequency dataframe" = processDistribution.abs[,c(1,3)],
                         "Event to symbol conversion" = symbolToEventConversion
    )
    return(processStats)
  }
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function() {
    eventType <<-''
    processInstances <<-''
    numberOfDifferentEvents <<- ''
    numberOfTotalEvents <<- ''
    processInstances.toSymbol <<- ''
    loaded.data<<-''
  }
  #===========================================================
  costructor();
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "getEventStats"=getEventStats,
    "getProcessStats"=getProcessStats
    # "plotEventStats"=plotEventStats,
    # "plotProcessStats"=plotProcessStats ,
    # "timeDistribution.stats.plot"=timeDistribution.stats.plot
  ) )
  
}
