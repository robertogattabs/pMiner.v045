#' A quality of data inspector
#'
#' @description   a QoD inspector class
#' @export
QoDInspector <- function() {
  global.processInstances.toSymbol <- list()
  global.dataLoader <- c();
  global.EFPTs <- c()
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( dataList ) {
    # Clear some possible previously inserted attribute
    clearAttributes()
    # set the new attributes
    EFOMM <- EfirstOrderMarkovModel()
    EFOMM$loadDataset( dataList )
    global.EFPTs <<- EFOMM$getEFPTs()
    # set the new attributes
    global.dataLoader <<- dataList
  }
  #===========================================================
  # defineAlias
  #===========================================================  
  defineAlias <- function( event , alias ) {
    global.processInstances.toSymbol[[ event ]] <<- list()
    global.processInstances.toSymbol[[ event ]]$alias <<- alias
  }
  #=================================================================================
  # strParser
  #=================================================================================  
  ruleScanner <- function( stringa ) {
    arr.ID <- names(global.dataLoader$pat.process)
    lst.Pat <- list()
    for( ID in arr.ID ) {
      lst.Pat[[ID]] <- strParser( ID, stringa )
    }
    return( lst.Pat ) 
  }  
  #=================================================================================
  # strParser
  #=================================================================================  
  strParser <- function( ID, stringa ) {
    
    rules <- c( "->" = "[ ']*[a-zA-Z_]+[ ']*(->)[ ']*[a-zA-Z_]+[ ']*",
                "-->" = "[ ']*[a-zA-Z_]+[ ']*(-->)[ ']*[a-zA-Z_]+[ ']*",
                "!->" = "[ ']*[a-zA-Z_]+[ ']*(!->)[ ']*[a-zA-Z_]+[ ']*",
                "!-->" = "[ ']*[a-zA-Z_]+[ ']*(!-->)[ ']*[a-zA-Z_]+[ ']*"
    )
    
    kkk <- unlist(lapply( 1:length(rules), function(i) { 
      ret <- c()
      ooo <- unique(str_trim(str_extract_all(stringa, rules[i])[[1]]))
      for( k in ooo ) ret <- c(ret, c(k , names(rules)[i]))
      return(ret)
    }))
    kkk <- matrix(kkk, ncol=2, byrow = T)
    kkk <- cbind(kkk, rep("",nrow(kkk)))
    colnames(kkk)<-c("rule","operator","value")
    
    for( riga in 1:nrow(kkk) ) {
      kkk[riga,"value"] <- strAtomicSolver( ID, kkk[riga,1] , kkk[riga,2] )
    }
    
    # ora rimpiazza i valori di verita'
    for( riga in 1:nrow(kkk) ) {
      stringa <- str_replace_all(string = stringa,pattern = as.character(kkk[riga,"rule"]),replacement = kkk[riga,"value"])
    }
    
    # EVAL: costruisci il risultato
    RISULTATO <- eval(expr = parse(text = stringa))

    return( list("res"=RISULTATO, "TF.table"=kkk) )
  }
  #=================================================================================
  # strAtomicSolver
  #=================================================================================   
  strAtomicSolver <- function( ID, stringa, operator ) {
    
    sinonimi <- unlist(global.processInstances.toSymbol)
    names(sinonimi) <- names(global.processInstances.toSymbol)
    
    ooo <- str_split(stringa , operator)[[1]]
    first.o <- str_trim(ooo[1])
    second.o <- str_trim(ooo[2])
    
    if(substr(first.o,1,1) == "'" & substr(first.o,str_length(first.o),str_length(first.o)) == "'") {
      first.o <- str_trim(str_replace_all(first.o,"'",""))
    } else {
      first.o <- names(sinonimi)[which(sinonimi %in% first.o)]
    }
    
    if(substr(second.o,1,1) == "'" & substr(second.o,str_length(second.o),str_length(second.o)) == "'") {
      second.o <- str_trim(str_replace_all(second.o,"'",""))
    } else {
      second.o <- names(sinonimi)[which(sinonimi %in% second.o)]
    }

    event.sequence <- c("BEGIN",global.dataLoader$pat.process[[ID]][, global.dataLoader$csv.EVENTName],"END")
    
    FO <- which( event.sequence %in% first.o)
    SO <- which( event.sequence %in% second.o)

    if( operator == "->") {
      if (sum(unlist(lapply( FO, function(i) { (i+1) %in% SO} ))) == 0 ) return( "FALSE" )
      else return( "TRUE" )
    }
    if( operator == "!->") {
      if (sum(unlist(lapply( FO, function(i) { (i+1) %in% SO} ))) == 0 ) return( "FALSE" )
      else return( "TRUE" )
    }    
    if( operator == "-->") {
      if (sum(unlist(lapply( FO, function(i) { i < SO} ))) == 0 ) return( "FALSE" )
      else return( "TRUE" )
    }    
    if( operator == "!-->") {
      if (sum(unlist(lapply( FO, function(i) { i < SO} ))) == 0 ) return( "TRUE" )
      else return( "FALSE" )
    }        
  }
  #=================================================================================
  # getStats
  #=================================================================================  
  getStats <- function()  {
    matriceAlias <- c()
    if(length(names(global.processInstances.toSymbol)) > 0) {
      matriceAlias <- cbind( names(global.processInstances.toSymbol), unlist(global.processInstances.toSymbol))
      colnames(matriceAlias)<-c("Event","Alias")      
    }

    return(
      list(
        "EFPTs" = global.EFPTs,
        "events" = global.dataLoader$arrayAssociativo,
        "aliases" = matriceAlias
      )
    )
  }    
  #=================================================================================
  # clearAttributes
  #=================================================================================
  clearAttributes<-function() {
    costructor();
  }  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function() {
    global.processInstances.toSymbol <<- list()
    global.dataLoader <<- ''
    global.EFPTs <<- c()
  }
  #===========================================================
  costructor();
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "defineAlias"=defineAlias,
    "getStats"=getStats,
    "ruleScanner"=ruleScanner
  ) )
  
}
