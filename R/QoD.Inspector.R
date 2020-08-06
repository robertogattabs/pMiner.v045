#' A quality of data inspector
#'
#' @description   a QoD inspector class
#' @export
QoDInspector <- function( UM = "" ) {
  global.processInstances.toSymbol <- list()
  global.dataLoader <- c();
  global.EFPTs <- c()
  global.UM <- ""
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
  # checkPath
  #================================================================================= 
  checkPath <- function( arr.ev.from, arr.ev.to, 
                         arr.ev.pass.through = c(), 
                         arr.ev.pass.not.through = c(),
                         arr.trapped.at = c(),
                         arr.some.global.counting.rules = c(),
                         arr.some.local.counting.rules = c(),
                         arr.additional.timing.rules = c(),
                         verboseReturn = FALSE ) {
    
    arr.ID <- names(global.dataLoader$pat.process)
    original.arr.ID <- arr.ID
    
    ID.true <- c()
    ID.false <- c()
    ID.trapped <- c()    
    
    if( length(arr.some.global.counting.rules) > 0 ) {
      for( regola in arr.some.global.counting.rules ) {
        aaa <- checkRule( regola , verboseReturn = FALSE)
        arr.ID <- intersect( arr.ID , aaa )
      }
    }
    arr.global.timing.rules <- c()
    if( length(arr.global.timing.rules) > 0 ) {
      for( regola in arr.global.timing.rules ) {
        aaa <- checkRule( regola , verboseReturn = FALSE, countingRules = FALSE)
        arr.ID <- intersect( arr.ID , aaa )
      }
    }    
    ID.false <- c(ID.false,original.arr.ID[!(original.arr.ID %in% arr.ID)])
    
    # inizia ad analizzare gli eventi
    for( ID in arr.ID ) {
      # event.sequence <- c("BEGIN",global.dataLoader$pat.process[[ID]][, global.dataLoader$csv.EVENTName],"END")
      startFrom <- which(arr.ev.from %in% event.sequence)
      finito <- FALSE; trappato <- FALSE; PNT.attivato <- FALSE; PT.attivato <- FALSE; iniziato <- FALSE
      if( length(arr.ev.pass.through) == 0 ) PT.attivato <- TRUE
      
      if( length(startFrom) > 0 ) {
        iniziato <- TRUE
        startFrom <- min( startFrom )
        for( indice in seq( startFrom + 1, length(event.sequence) ) ) {
          current.Event <- event.sequence[indice]
          
          if( current.Event %in% arr.ev.to) {
            finito <- TRUE
          }
          if( current.Event %in% arr.trapped.at) {
            trappato <- TRUE
          }
          if( current.Event %in% arr.ev.pass.not.through) {
            PNT.attivato <- TRUE
          } 
          if( current.Event %in% arr.ev.pass.through) {
            PT.attivato <- TRUE
          }      
          if( finito == TRUE | trappato == TRUE | PNT.attivato == TRUE ) break;
        }
      }
      # browser()
      # Se il paziente avesse avuto un percorso di successo, verifica le local
      # counting and timing rules
      if( finito == TRUE ) {
        if( length(arr.some.local.counting.rules) > 0 ) {
          event.subsequence <- event.sequence[startFrom:indice]
          for( regola in arr.some.local.counting.rules) {
            risultato <- strParser(ID = ID, stringa = regola, evt.sequence = event.subsequence, verboseReturn = FALSE )
            # se il risultato e' FALSE fai si' che il resto dei controlli lo inseriscano come opportuno
            # (nell'array degli ID.false)
            if( risultato == FALSE ) {
              iniziato <- FALSE; finito <- FALSE; trappato <- FALSE; PNT.attivato <- FALSE; PT.attivato <- TRUE;
            }
          }
        }
      }
      
      # Fai il resto dei controlli
      if( iniziato == FALSE ) {
        ID.false <- c( ID.false, ID ) 
      } else {
        if( PNT.attivato == TRUE | PT.attivato == FALSE  ) {
          ID.false <- c( ID.false, ID )
        } else {
          if( trappato == TRUE ) { 
            ID.trapped <- c( ID.trapped, ID )
          } else {
            if( finito == TRUE ) {
              ID.true <- c( ID.true, ID )
            } else {
              ID.false <- c( ID.false, ID )
            }
          }
        }
      }
      
      if( length(intersect(ID.true,ID.false))>0 | length(intersect(ID.true,ID.trapped))>0 |
          length(intersect(ID.trapped,ID.false))>0   ) browser()
    }
    
    return( list( "TRUE" = ID.true , "FALSE" = ID.false, "TRAPPED" = ID.trapped)   )
  }   
  #=================================================================================
  # checkRule
  #=================================================================================  
  checkRule <- function( stringa ,  verboseReturn = TRUE , countingRules = TRUE) {
    arr.ID <- names(global.dataLoader$pat.process)
    res <- list()
    for( ID in arr.ID ) {
      # browser()
      evt.sequence <- global.dataLoader$pat.process[[ID]][[global.dataLoader$csv.EVENTName]]
      # evt.sequence <- c("BEGIN",evt.sequence,"END")
      res[[ID]] <- strParser( ID, stringa , countingRules = countingRules, evt.sequence= evt.sequence )
      # browser() 
      # 1077086
    }
    # browser()
    # IDD.true <- unlist(lapply( 1:length(res) , function(i) { if(res[[i]]$res == TRUE ) return( names(res)[i] )}))
    # IDD.false <- unlist(lapply( 1:length(res) , function(i) { if(res[[i]]$res == FALSE ) return( names(res)[i] )}))
    
    
    MM <- c()
    tmp.1 <- lapply(names(res), function( ID ) {  
        # if(length(res[[ID]]$res) > 0 )  {
          MM <<- rbind( MM , cbind( rep( ID, nrow(res[[ID]]$res ) ), res[[ID]]$res ) )
        # } 
      })
    si <- c()
    no <- c()
    tmp.1 <- lapply( names(res), function(ID){ if( res[[ID]]$esito == TRUE ) {si <<- c( si , ID)} })
    tmp.1 <- lapply( names(res), function(ID){ if( res[[ID]]$esito == FALSE ) {no <<- c( no , ID)} })
    
    # browser()
    
    # lapply(names(res), function(ID)  {   MM <<- rbind( MM , cbind(rep(ID,length(res[[ID]]$res)),res[[ID]]$res) ) }   )
    # browser()
    toReturn <- list()
    toReturn$details <- res
    toReturn$yes <- si
    toReturn$no <- no
    # toReturn$IDD.true <- IDD.true
    # toReturn$IDD.false <- IDD.false
    
    if( verboseReturn == FALSE ) return ( IDD.true )
    
    return( toReturn ) 
  }  
  #=================================================================================
  # strParser
  #=================================================================================  
  strParser <- function( ID, stringa , evt.sequence ,verboseReturn = TRUE , countingRules = TRUE ) {
    
    # str_extract_all(string = "('a' -> 'b' & 'c' -> 'ggui') | 'dfds' -> 'dfs' ",pattern = "[ ']*[a-zA-Z_ ]+[ ']*(->)[ ']*[a-zA-Z_ ]+[ ']*")
    
    rules <- c( "->" = "[ ']*[a-zA-Z_ ]+[']*(->)[ ']*[a-zA-Z_ ]+[']*",
                "-->" = "[ ']*[a-zA-Z_ ]+[']*(-->)[ ']*[a-zA-Z_ ]+[']*",
                "-X" = "[ ']*[a-zA-Z_ ]+[']*(-X)[ ']*[a-zA-Z_ ]+[']*",
                "--X" = "[ ']*[a-zA-Z_ ]+[']*(--X)[ ']*[a-zA-Z_ ]+[']*"
    )
    # browser()
    kkk <- unlist(lapply( 1:length(rules), function(i) { 
      ret <- c()
      ooo <- unique(str_trim(str_extract_all(stringa, rules[i])[[1]]))
      for( k in ooo ) ret <- c(ret, c(k , names(rules)[i]))
      return(ret)
    }))
    kkk <- matrix(kkk, ncol=2, byrow = T)
    kkk <- cbind(kkk, rep("",nrow(kkk)))
    colnames(kkk)<-c("rule","operator","value")
    
    # for( riga in 1:nrow(kkk) ) {
    riga <- 1
    risultato <- strAtomicSolver( ID, kkk[riga,1] , kkk[riga,2] , 
                                          evt.sequence = evt.sequence , conditionOnTime = countingRules )
    
    if( is.na(sum(risultato$ret)) || sum(risultato$ret)==0 ) {
      risultato$esito <- FALSE  
    } else {
      risultato$esito <- TRUE
    }
    
    # browser()
    # kkk[riga,"value"] <- risultato$ret
    # browser()
    if(kkk[1,"operator"]=="->" || kkk[1,"operator"]=="-->") {
      numero.che.soddisfano <- sum(risultato$res[,kkk[1,"operator"]] == "x")  
    }
    # if(kkk[1,"operator"]=="-X" || kkk[1,"operator"]=="--X") {
    #   numero.che.soddisfano <- sum(risultato$res[,kkk[1,"operator"]] == "x")  
    # }
    
# cio' che segue e' roba vecchia
    # -im
    # browser()
    # # }
    # # browser()
    # # ora rimpiazza i valori di verita'
    # for( riga in 1:nrow(kkk) ) {
    #   stringa <- str_replace_all(string = stringa,pattern = as.character(kkk[riga,"rule"]),replacement = kkk[riga,"value"])
    # }
    # 
    # # EVAL: costruisci il risultato
    # RISULTATO <- eval(expr = parse(text = stringa))
    # if( verboseReturn == TRUE ) {
    #   res <- list("res"=RISULTATO, "TF.table"=kkk)      
    # } else {
    #   res <- RISULTATO
    # }
    res <- risultato
    # -fm
    
    # browser()
    return( res )
  }
  #=================================================================================
  # strAtomicSolver
  #=================================================================================   
  strAtomicSolver <- function( ID, stringa, operator , evt.sequence  , conditionOnTime = FALSE ) {
    
    sinonimi <- unlist(global.processInstances.toSymbol)
    names(sinonimi) <- names(global.processInstances.toSymbol)
    # browser()
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
    
    if( length(evt.sequence) == 0)
      event.sequence <- c("BEGIN",global.dataLoader$pat.process[[ID]][, global.dataLoader$csv.EVENTName],"END")
    else 
      event.sequence <- c("BEGIN",evt.sequence,"END")
    
    arr.FO <- which( event.sequence %in% first.o)
    arr.SO <- which( event.sequence %in% second.o)
    arr.FO.t <- c()
    arr.SO.t <- c()
    
    if( length(arr.FO) > 0 ) {
      for(i in 1:length(arr.FO)) {
        if( arr.FO[i] > 1 ) { arr.FO.t <- c( arr.FO.t, global.dataLoader$pat.process[[ID]][arr.FO[i]-1,"pMineR.deltaDate"]) } 
      }
    } 
    if( length(arr.SO) > 0 ) {
      for( i in 1:length(arr.SO) ) {
        if( arr.SO[i] > 1 ) { arr.SO.t <- c(arr.SO.t,global.dataLoader$pat.process[[ID]][arr.SO[i]-2,"pMineR.deltaDate"]) }
      }
    }

    conta.diretti <- sum(unlist(lapply( arr.FO, function(i) { (i+1) %in% arr.SO} )))
    quali.FO <- arr.FO[unlist(lapply( arr.FO, function(i) { (i+1) %in% arr.SO} ))]
    tempi.diretti <- unlist(lapply( quali.FO, function(x) { global.dataLoader$pat.process[[ID]]$pMineR.deltaDate[x]-global.dataLoader$pat.process[[ID]]$pMineR.deltaDate[x-1] } ))

    conta.indiretti <- sum(unlist(lapply( arr.FO, function(i) { i < arr.SO} )))
    
    tmpQWE <- expand.grid(arr.FO,arr.SO)
    colnames(tmpQWE)<-c("From","To")
    tmpQWE <- cbind(tmpQWE, "->"=rep("",nrow(tmpQWE)))
    tmpQWE <- cbind(tmpQWE, "-->"=rep("",nrow(tmpQWE)))
    tmpQWE <- cbind(tmpQWE, "-X"=rep("",nrow(tmpQWE)))
    tmpQWE <- cbind(tmpQWE, "--X"=rep("",nrow(tmpQWE)))
    tmpQWE <- as.matrix(tmpQWE)
# browser()

    if( nrow(tmpQWE) != 0 ) {
      tmp <- lapply( 1:nrow(tmpQWE), function(riga) { if( as.numeric(tmpQWE[riga,"To"])==(as.numeric(tmpQWE[riga,"From"])+1) ) tmpQWE[riga,"->"]<<-"x"  } )
      tmp <- lapply( 1:nrow(tmpQWE), function(riga) { if( as.numeric(tmpQWE[riga,"To"])>as.numeric(tmpQWE[riga,"From"]) ) {tmpQWE[riga,"-->"]<<-"x"}  } ) 
  # browser()
      tmp <- lapply( 1:nrow(tmpQWE), function(riga) { 
                      if( tmpQWE[riga,"->"] == "x" )  {
                        if( as.numeric(tmpQWE[riga,"From"]) >  1 ) from <- global.dataLoader$pat.process[[ID]][as.numeric(tmpQWE[riga,"From"])-1,"pMineR.deltaDate"]
                        if( as.numeric(tmpQWE[riga,"From"]) <=  1 ) from <- 0
                        if( event.sequence[as.numeric(tmpQWE[riga,"To"])] == "END" ) to <- global.dataLoader$pat.process[[ID]][as.numeric(tmpQWE[riga,"To"])-2,"pMineR.deltaDate"]
                        if( event.sequence[as.numeric(tmpQWE[riga,"To"])] != "END" ) to <- global.dataLoader$pat.process[[ID]][as.numeric(tmpQWE[riga,"To"])-1,"pMineR.deltaDate"]
                        tmpQWE[riga,"-X"] <<- global.dataLoader$pat.process[[ID]][as.numeric(tmpQWE[riga,"To"])-1,"pMineR.deltaDate"] - from
                      }
                    })
      tmp <- lapply( 1:nrow(tmpQWE), function(riga) { 
                      if( tmpQWE[riga,"-->"] == "x" ) 
                      {  
                        if( as.numeric(tmpQWE[riga,"From"]) >  1 ) from <- global.dataLoader$pat.process[[ID]][as.numeric(tmpQWE[riga,"From"])-1,"pMineR.deltaDate"]
                        if( as.numeric(tmpQWE[riga,"From"]) <= 1 ) from <- 0
                        if( event.sequence[as.numeric(tmpQWE[riga,"To"])] == "END" ) to <- global.dataLoader$pat.process[[ID]][as.numeric(tmpQWE[riga,"To"])-2,"pMineR.deltaDate"]
                        if( event.sequence[as.numeric(tmpQWE[riga,"To"])] != "END" ) to <- global.dataLoader$pat.process[[ID]][as.numeric(tmpQWE[riga,"To"])-1,"pMineR.deltaDate"]
                        tmpQWE[riga,"--X"] <<- to - from
                      }
                    })
  
      if( global.UM=="hours" ) tmpQWE[,"--X"] <- as.numeric(tmpQWE[,"--X"])/60
      if( global.UM=="hours" ) tmpQWE[,"-X"] <- as.numeric(tmpQWE[,"-X"])/60
      if( global.UM=="days" ) tmpQWE[,"--X"] <- as.numeric(tmpQWE[,"--X"])/(60*24)
      if( global.UM=="days" ) tmpQWE[,"-X"] <- as.numeric(tmpQWE[,"-X"])/(60*24)
      if( global.UM=="weeks" ) tmpQWE[,"--X"] <- as.numeric(tmpQWE[,"--X"])/(60*24*7)
      if( global.UM=="weeks" ) tmpQWE[,"-X"] <- as.numeric(tmpQWE[,"-X"])/(60*24*7)
      if( global.UM=="months" ) tmpQWE[,"--X"] <- as.numeric(tmpQWE[,"--X"])/(60*24*7*30)
      if( global.UM=="months" ) tmpQWE[,"-X"] <- as.numeric(tmpQWE[,"-X"])/(60*24*7*30)    
      tmpQWE[,"-X"][which(is.na(tmpQWE[,"-X"]))] <- ""
      tmpQWE[,"--X"][which(is.na(tmpQWE[,"--X"]))] <- ""
      
      # browser()
      
      if( operator == "->" ) {
        if( conditionOnTime == TRUE ) {
          ret <- as.numeric(tmpQWE[,"-X"][tmpQWE[,"-X"]!=""])
        } else {
          ret <- suppressWarnings(as.numeric(tmpQWE[,"->"]))
        }
        # browser()
      }
      if( operator == "-->" ) {
        if( conditionOnTime == TRUE ) {
          ret <- !is.na(as.numeric(tmpQWE[,"--X"]))
        } else {
          ret <- suppressWarnings(as.numeric(tmpQWE[,"-->"]))
        }
        # browser()
      }    
    } else {
      ret <- NA
    }
    
        # browser()
    return( list( "res" = tmpQWE, "ret" = ret ) )
          
  }
  strParser.all <- function( ID , query , evt.sequence ) {
    
    rules <- c( "->" = "[ ']*[a-zA-Z_ ]+[ ']*(->)[ ']*[a-zA-Z_ ]+[ ']*",
                "-->" = "[ ']*[a-zA-Z_ ]+[ ']*(-->)[ ']*[a-zA-Z_ ]+[ ']*"
    )

    # Estrai i pezzi che soddisfano la regexp
    stringa <- query
    kkk <- unlist(lapply( 1:length(rules), function(i) { 
      ret <- c()
      ooo <- unique(str_trim(str_extract_all(stringa, rules[i])[[1]]))
      for( k in ooo ) ret <- c(ret, c(k , names(rules)[i]))
      return(ret)
    }))
    kkk <- matrix(kkk, ncol=2, byrow = T)
    kkk <- cbind(kkk, rep("",nrow(kkk)))
    colnames(kkk)<-c("rule","operator","value")
    nuova.stringa <- stringa
    
    # ora risolvile una per una
    for( riga in 1:nrow(kkk) ) {
      risultato <- strAtomicSolver( ID, kkk[riga,1] , kkk[riga,2] , evt.sequence = evt.sequence  )
      risultato$esito <- c("x") %in% risultato$res[,kkk[riga,2] ]

      if( risultato$esito == TRUE)
        nuova.stringa <- str_replace_all(string = str_replace_all(nuova.stringa,"'","`"),pattern = str_replace_all(kkk[riga,"rule"],"'","`"), "TRUE")
      if( risultato$esito == FALSE)      
        nuova.stringa <- str_replace_all(string = str_replace_all(nuova.stringa,"'","`"),pattern = str_replace_all(kkk[riga,"rule"],"'","`"), "FALSE")
      
    }
    
    res.parsato <- eval(expr = parse(text = nuova.stringa))
    
    return( 
      list( "res.parsato" =  res.parsato,
            "nuova.stringa" = nuova.stringa)
      )
  }
  executeQuery <- function( query ) {
    
    arr.ID <- names(global.dataLoader$pat.process)
    res <- list()
    si <- c()
    no <- c()
    detail <- list()
    for( ID in arr.ID ) {
      evt.sequence <- global.dataLoader$pat.process[[ID]][[global.dataLoader$csv.EVENTName]]
      res <- strParser.all( ID, query , evt.sequence = evt.sequence )
      # browser()
      if( res$res.parsato == TRUE ) si <- c( si , ID)
      if( res$res.parsato == FALSE ) no <- c( no , ID)
      detail[[ID]] <- res$nuova.stringa
    }
    
    return( list(  "yes" = si, "no"=no, "detail"=detail) )
    
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
    costructor( UM = UM );
  }  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( UM ) {
    global.processInstances.toSymbol <<- list()
    global.dataLoader <<- ''
    global.EFPTs <<- c()
    global.UM <<- UM
    if( global.UM == "" ) global.UM <<- "mins"
  }
  #===========================================================
  costructor( UM = UM );
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "defineAlias"=defineAlias,
    "getStats"=getStats,
    "checkRule"=checkRule,
    "checkPath"=checkPath,
    "executeQuery"=executeQuery
  ) )
}

# --------------------------------------------------

#' #' A quality of data inspector
#' #'
#' #' @description   a QoD inspector class
#' #' @export
#' QoDInspector <- function() {
#'   global.processInstances.toSymbol <- list()
#'   global.dataLoader <- c();
#'   global.EFPTs <- c()
#'   #===========================================================
#'   # loadDataset
#'   #===========================================================
#'   loadDataset<-function( dataList ) {
#'     # Clear some possible previously inserted attribute
#'     clearAttributes()
#'     # set the new attributes
#'     EFOMM <- EfirstOrderMarkovModel()
#'     EFOMM$loadDataset( dataList )
#'     global.EFPTs <<- EFOMM$getEFPTs()
#'     # set the new attributes
#'     global.dataLoader <<- dataList
#'   }
#'   #===========================================================
#'   # defineAlias
#'   #===========================================================  
#'   defineAlias <- function( event , alias ) {
#'     global.processInstances.toSymbol[[ event ]] <<- list()
#'     global.processInstances.toSymbol[[ event ]]$alias <<- alias
#'   }
#'   #=================================================================================
#'   # checkPath
#'   #================================================================================= 
#'   checkPath <- function( arr.ev.from, arr.ev.to, 
#'                          arr.ev.pass.through = c(), 
#'                          arr.ev.pass.not.through = c(),
#'                          arr.trapped.at = c(),
#'                          arr.some.global.counting.rules = c(),
#'                          arr.some.local.counting.rules = c(),
#'                          arr.additional.timing.rules = c(),
#'                          verboseReturn = FALSE ) {
#'     
#'     arr.ID <- names(global.dataLoader$pat.process)
#'     original.arr.ID <- arr.ID
#'     
#'     ID.true <- c()
#'     ID.false <- c()
#'     ID.trapped <- c()    
#'     
#'     if( length(arr.some.global.counting.rules) > 0 ) {
#'       for( regola in arr.some.global.counting.rules ) {
#'         aaa <- checkRule( regola , verboseReturn = FALSE)
#'         arr.ID <- intersect( arr.ID , aaa )
#'       }
#'     }
#'     arr.global.timing.rules <- c()
#'     if( length(arr.global.timing.rules) > 0 ) {
#'       for( regola in arr.global.timing.rules ) {
#'         aaa <- checkRule( regola , verboseReturn = FALSE, countingRules = FALSE)
#'         arr.ID <- intersect( arr.ID , aaa )
#'       }
#'     }    
#'     ID.false <- c(ID.false,original.arr.ID[!(original.arr.ID %in% arr.ID)])
#'     
#'     # inizia ad analizzare gli eventi
#'     for( ID in arr.ID ) {
#'       event.sequence <- c("BEGIN",global.dataLoader$pat.process[[ID]][, global.dataLoader$csv.EVENTName],"END")
#'       startFrom <- which(arr.ev.from %in% event.sequence)
#'       finito <- FALSE; trappato <- FALSE; PNT.attivato <- FALSE; PT.attivato <- FALSE; iniziato <- FALSE
#'       if( length(arr.ev.pass.through) == 0 ) PT.attivato <- TRUE
#'       
#'       if( length(startFrom) > 0 ) {
#'         iniziato <- TRUE
#'         startFrom <- min( startFrom )
#'         for( indice in seq( startFrom + 1, length(event.sequence) ) ) {
#'           current.Event <- event.sequence[indice]
#'           
#'           if( current.Event %in% arr.ev.to) {
#'             finito <- TRUE
#'           }
#'           if( current.Event %in% arr.trapped.at) {
#'             trappato <- TRUE
#'           }
#'           if( current.Event %in% arr.ev.pass.not.through) {
#'             PNT.attivato <- TRUE
#'           } 
#'           if( current.Event %in% arr.ev.pass.through) {
#'             PT.attivato <- TRUE
#'           }      
#'           if( finito == TRUE | trappato == TRUE | PNT.attivato == TRUE ) break;
#'         }
#'       }
#' 
#'       # Se il paziente avesse avuto un percorso di successo, verifica le local
#'       # counting and timing rules
#'       if( finito == TRUE ) {
#'         if( length(arr.some.local.counting.rules) > 0 ) {
#'           event.subsequence <- event.sequence[startFrom:indice]
#'           for( regola in arr.some.local.counting.rules) {
#'             risultato <- strParser(ID = ID, stringa = regola, evt.sequence = event.subsequence, verboseReturn = FALSE )
#'             # se il risultato e' FALSE fai si' che il resto dei controlli lo inseriscano come opportuno
#'             # (nell'array degli ID.false)
#'             if( risultato == FALSE ) {
#'               iniziato <- FALSE; finito <- FALSE; trappato <- FALSE; PNT.attivato <- FALSE; PT.attivato <- TRUE;
#'             }
#'           }
#'         }
#'       }
#'       
#'       # Fai il resto dei controlli
#'       if( iniziato == FALSE ) {
#'         ID.false <- c( ID.false, ID ) 
#'       } else {
#'         if( PNT.attivato == TRUE | PT.attivato == FALSE  ) {
#'           ID.false <- c( ID.false, ID )
#'         } else {
#'           if( trappato == TRUE ) { 
#'             ID.trapped <- c( ID.trapped, ID )
#'           } else {
#'             if( finito == TRUE ) {
#'               ID.true <- c( ID.true, ID )
#'             } else {
#'               ID.false <- c( ID.false, ID )
#'             }
#'           }
#'         }
#'       }
#' 
#'       if( length(intersect(ID.true,ID.false))>0 | length(intersect(ID.true,ID.trapped))>0 |
#'           length(intersect(ID.trapped,ID.false))>0   ) browser()
#'     }
#'     
#'     return( list( "TRUE" = ID.true , "FALSE" = ID.false, "TRAPPED" = ID.trapped)   )
#'   }   
#'   #=================================================================================
#'   # checkRule
#'   #=================================================================================  
#'   checkRule <- function( stringa , verboseReturn = TRUE , countingRules = TRUE) {
#'     arr.ID <- names(global.dataLoader$pat.process)
#'     res <- list()
#'     for( ID in arr.ID ) {
#'       res[[ID]] <- strParser( ID, stringa , countingRules = countingRules )
#'     }
#'     browser()
#'     IDD.true <- unlist(lapply( 1:length(res) , function(i) { if(res[[i]]$res == TRUE ) return( names(res)[i] )}))
#'     IDD.false <- unlist(lapply( 1:length(res) , function(i) { if(res[[i]]$res == FALSE ) return( names(res)[i] )}))
#'     
#'     toReturn <- list()
#'     toReturn$details <- res
#'     toReturn$IDD.true <- IDD.true
#'     toReturn$IDD.false <- IDD.false
#'     
#'     if( verboseReturn == FALSE ) return ( IDD.true )
#'     
#'     return( toReturn ) 
#'   }  
#'   #=================================================================================
#'   # strParser
#'   #=================================================================================  
#'   strParser <- function( ID, stringa , evt.sequence=c() ,verboseReturn = TRUE , countingRules = TRUE ) {
#'     
#'     rules <- c( "->" = "[ ']*[a-zA-Z_]+[ ']*(->)[ ']*[a-zA-Z_]+[ ']*",
#'                 "-->" = "[ ']*[a-zA-Z_]+[ ']*(-->)[ ']*[a-zA-Z_]+[ ']*",
#'                 "!->" = "[ ']*[a-zA-Z_]+[ ']*(!->)[ ']*[a-zA-Z_]+[ ']*",
#'                 "!-->" = "[ ']*[a-zA-Z_]+[ ']*(!-->)[ ']*[a-zA-Z_]+[ ']*"
#'     )
#'     # browser()
#'     kkk <- unlist(lapply( 1:length(rules), function(i) { 
#'       ret <- c()
#'       ooo <- unique(str_trim(str_extract_all(stringa, rules[i])[[1]]))
#'       for( k in ooo ) ret <- c(ret, c(k , names(rules)[i]))
#'       return(ret)
#'     }))
#'     kkk <- matrix(kkk, ncol=2, byrow = T)
#'     kkk <- cbind(kkk, rep("",nrow(kkk)))
#'     colnames(kkk)<-c("rule","operator","value")
#'     
#'     for( riga in 1:nrow(kkk) ) {
#'       kkk[riga,"value"] <- strAtomicSolver( ID, kkk[riga,1] , kkk[riga,2] , 
#'                                             evt.sequence = evt.sequence , countingRules = countingRules )
#'     }
#'     # browser()
#'     # ora rimpiazza i valori di verita'
#'     for( riga in 1:nrow(kkk) ) {
#'       stringa <- str_replace_all(string = stringa,pattern = as.character(kkk[riga,"rule"]),replacement = kkk[riga,"value"])
#'     }
#'     
#'     # EVAL: costruisci il risultato
#'     RISULTATO <- eval(expr = parse(text = stringa))
#'     if( verboseReturn == TRUE ) {
#'       res <- list("res"=RISULTATO, "TF.table"=kkk)      
#'     } else {
#'       res <- RISULTATO
#'     }
#' 
#'     
#'     return( res )
#'   }
#'   #=================================================================================
#'   # strAtomicSolver
#'   #=================================================================================   
#'   strAtomicSolver <- function( ID, stringa, operator , evt.sequence = c() , countingRules = TRUE) {
#'     
#'     sinonimi <- unlist(global.processInstances.toSymbol)
#'     names(sinonimi) <- names(global.processInstances.toSymbol)
#'     
#'     ooo <- str_split(stringa , operator)[[1]]
#'     first.o <- str_trim(ooo[1])
#'     second.o <- str_trim(ooo[2])      
#' 
#'     if(substr(first.o,1,1) == "'" & substr(first.o,str_length(first.o),str_length(first.o)) == "'") {
#'       first.o <- str_trim(str_replace_all(first.o,"'",""))
#'     } else {
#'       first.o <- names(sinonimi)[which(sinonimi %in% first.o)]
#'     }
#'     
#'     if(substr(second.o,1,1) == "'" & substr(second.o,str_length(second.o),str_length(second.o)) == "'") {
#'       second.o <- str_trim(str_replace_all(second.o,"'",""))
#'     } else {
#'       second.o <- names(sinonimi)[which(sinonimi %in% second.o)]
#'     }
#'     
#'     if( length(evt.sequence) == 0)
#'       event.sequence <- c("BEGIN",global.dataLoader$pat.process[[ID]][, global.dataLoader$csv.EVENTName],"END")
#'     else 
#'       event.sequence <- c("BEGIN",evt.sequence,"END")
#' 
#'     arr.FO <- which( event.sequence %in% first.o)
#'     arr.SO <- which( event.sequence %in% second.o)
#'     arr.FO.t <- c()
#'     arr.SO.t <- c()
#'     
#'     if( length(arr.FO) > 0 ) {
#'       for(i in 1:length(arr.FO)) {
#'         if( arr.FO[i] > 1 ) { arr.FO.t <- c( arr.FO.t, global.dataLoader$pat.process[[ID]][arr.FO[i]-1,"pMineR.deltaDate"]) } 
#'       }
#'     } 
#'     if( length(arr.SO) > 0 ) {
#'       for( i in 1:length(arr.SO) ) {
#'         if( arr.SO[i] > 1 ) { arr.SO.t <- c(arr.SO.t,global.dataLoader$pat.process[[ID]][arr.SO[i]-2,"pMineR.deltaDate"]) }
#'       }
#'     }
#'     
#'     # if( length(FO) > 0 ) {
#'     #   if( FO > 1 ) { FO.t <- global.dataLoader$pat.process[[ID]][FO-1,"pMineR.deltaDate"] } else { FO.t <- 0 }
#'     # } else { FO.t <- 0 }
#'     # if( length(SO) ) {
#'     #   if( SO > 1 ) { SO.t <- global.dataLoader$pat.process[[ID]][SO-2,"pMineR.deltaDate"] } else { SO.t <- 0 }
#'     # } else { SO.t <- 0 }
#'     browser()
#'     cat("FO e SO possono essere array. qui ci devi mettere la logica some vs all")
#' 
#'     if( operator == "->") {
#'       if( countingRules == TRUE ) {
#'         return( sum(unlist(lapply( arr.FO, function(i) { (i+1) %in% arr.SO} ))) )
#'       } else {
#'         browser()
#'       }
#'     }
#'     if( operator == "!->") {
#'       if( countingRules == TRUE ) {
#'         return( sum(unlist(lapply( arr.FO, function(i) { (i+1) %in% arr.SO} ))) )
#'       } else {
#'         browser()
#'       }
#'     }    
#'     if( operator == "-->") {
#'       if( countingRules == TRUE ) {
#'         return( sum(unlist(lapply( arr.FO, function(i) { i < arr.SO} )))  )
#'       } else {
#'         browser()
#'       }
#'     }    
#'     if( operator == "!-->") {
#'       if( countingRules == TRUE ) {
#'         return( sum(unlist(lapply( arr.FO, function(i) { i < arr.SO} ))  ) )
#'       } else {
#'         browser()
#'       }
#'     }        
#'   }
#'   #=================================================================================
#'   # getStats
#'   #=================================================================================  
#'   getStats <- function()  {
#'     matriceAlias <- c()
#'     if(length(names(global.processInstances.toSymbol)) > 0) {
#'       matriceAlias <- cbind( names(global.processInstances.toSymbol), unlist(global.processInstances.toSymbol))
#'       colnames(matriceAlias)<-c("Event","Alias")      
#'     }
#'     
#'     return(
#'       list(
#'         "EFPTs" = global.EFPTs,
#'         "events" = global.dataLoader$arrayAssociativo,
#'         "aliases" = matriceAlias
#'       )
#'     )
#'   }    
#'   #=================================================================================
#'   # clearAttributes
#'   #=================================================================================
#'   clearAttributes<-function() {
#'     costructor();
#'   }  
#'   #===========================================================
#'   # costructor
#'   # E' il costruttore della classe
#'   #===========================================================
#'   costructor<-function() {
#'     global.processInstances.toSymbol <<- list()
#'     global.dataLoader <<- ''
#'     global.EFPTs <<- c()
#'   }
#'   #===========================================================
#'   costructor();
#'   #===========================================================
#'   return( list(
#'     "loadDataset"=loadDataset,
#'     "defineAlias"=defineAlias,
#'     "getStats"=getStats,
#'     "checkRule"=checkRule,
#'     "checkPath"=checkPath
#'   ) )
#' }
#' 
#' 

# ----------------------------------------------------------------------------------

#' #' A quality of data inspector
#' #'
#' #' @description   a QoD inspector class
#' #' @export
#' QoDInspector <- function() {
#'   global.processInstances.toSymbol <- list()
#'   global.dataLoader <- c();
#'   global.EFPTs <- c()
#'   #===========================================================
#'   # loadDataset
#'   #===========================================================
#'   loadDataset<-function( dataList ) {
#'     # Clear some possible previously inserted attribute
#'     clearAttributes()
#'     # set the new attributes
#'     EFOMM <- EfirstOrderMarkovModel()
#'     EFOMM$loadDataset( dataList )
#'     global.EFPTs <<- EFOMM$getEFPTs()
#'     # set the new attributes
#'     global.dataLoader <<- dataList
#'   }
#'   #===========================================================
#'   # defineAlias
#'   #===========================================================
#'   defineAlias <- function( event , alias ) {
#'     global.processInstances.toSymbol[[ event ]] <<- list()
#'     global.processInstances.toSymbol[[ event ]]$alias <<- alias
#'   }
#'   #=================================================================================
#'   # strParser
#'   #=================================================================================
#'   checkRule <- function( stringa ) {
#'     arr.ID <- names(global.dataLoader$pat.process)
#'     res <- list()
#'     for( ID in arr.ID ) {
#'       res[[ID]] <- strParser( ID, stringa )
#'     }
#'     IDD.true <- unlist(lapply( 1:length(res) , function(i) { if(res[[i]]$res == TRUE ) return( names(res)[i] )}))
#'     IDD.false <- unlist(lapply( 1:length(res) , function(i) { if(res[[i]]$res == TRUE ) return( names(res)[i] )}))
#' 
#'     toReturn <- list()
#'     toReturn$details <- res
#'     toReturn$IDD.true <- IDD.true
#'     toReturn$IDD.false <- IDD.false
#' 
#'     return( toReturn )
#'   }
#'   #=================================================================================
#'   # strParser
#'   #=================================================================================
#'   strParser <- function( ID, stringa ) {
#' 
#'     rules <- c(
#'                 "->" = "[ ']*[a-zA-Z_]+[ ']*(->)[ ']*[a-zA-Z_]+[ ']*",
#'                 "-->" = "[ ']*[a-zA-Z_]+[ ']*(-->)[ ']*[a-zA-Z_]+[ ']*",
#'                 "!->" = "[ ']*[a-zA-Z_]+[ ']*(!->)[ ']*[a-zA-Z_]+[ ']*",
#'                 "!-->" = "[ ']*[a-zA-Z_]+[ ']*(!-->)[ ']*[a-zA-Z_]+[ ']*",
#'                 "[ ]*((all|some|count)\()[ ']*[a-zA-Z_]+[ ']*(->)[ ']*[a-zA-Z_]+[ ']*(\))[ ]*"
#' 
#'     )
#' 
#'     kkk <- unlist(lapply( 1:length(rules), function(i) {
#'       ret <- c()
#'       ooo <- unique(str_trim(str_extract_all(stringa, rules[i])[[1]]))
#'       for( k in ooo ) ret <- c(ret, c(k , names(rules)[i]))
#'       return(ret)
#'     }))
#'     kkk <- matrix(kkk, ncol=2, byrow = T)
#'     kkk <- cbind(kkk, rep("",nrow(kkk)))
#'     colnames(kkk)<-c("rule","operator","value")
#' 
#'     for( riga in 1:nrow(kkk) ) {
#'       kkk[riga,"value"] <- strAtomicSolver( ID, kkk[riga,1] , kkk[riga,2] )
#'     }
#' 
#'     # ora rimpiazza i valori di verita'
#'     for( riga in 1:nrow(kkk) ) {
#'       stringa <- str_replace_all(string = stringa,pattern = as.character(kkk[riga,"rule"]),replacement = kkk[riga,"value"])
#'     }
#' 
#'     # EVAL: costruisci il risultato
#'     RISULTATO <- eval(expr = parse(text = stringa))
#' 
#'     return( list("res"=RISULTATO, "TF.table"=kkk) )
#'   }
#'   #=================================================================================
#'   # strAtomicSolver
#'   #=================================================================================
#'   strAtomicSolver <- function( ID, stringa, operator ) {
#' 
#'     sinonimi <- unlist(global.processInstances.toSymbol)
#'     names(sinonimi) <- names(global.processInstances.toSymbol)
#' 
#'     ooo <- str_split(stringa , operator)[[1]]
#'     first.o <- str_trim(ooo[1])
#'     second.o <- str_trim(ooo[2])
#' 
#'     if(substr(first.o,1,1) == "'" & substr(first.o,str_length(first.o),str_length(first.o)) == "'") {
#'       first.o <- str_trim(str_replace_all(first.o,"'",""))
#'     } else {
#'       first.o <- names(sinonimi)[which(sinonimi %in% first.o)]
#'     }
#' 
#'     if(substr(second.o,1,1) == "'" & substr(second.o,str_length(second.o),str_length(second.o)) == "'") {
#'       second.o <- str_trim(str_replace_all(second.o,"'",""))
#'     } else {
#'       second.o <- names(sinonimi)[which(sinonimi %in% second.o)]
#'     }
#' 
#'     MMM <- rbind( c(-1,ID,"","BEGIN",0),global.dataLoader$pat.process[[ID]])
#'     MMM <- rbind( MMM, c(-999,ID,"","END", max(MMM$pMineR.deltaDate)))
#'     # event.sequence <- c("BEGIN",global.dataLoader$pat.process[[ID]][, global.dataLoader$csv.EVENTName],"END")
#' 
#'     output.riga <- c()
#'     output.tempi <- c()
#'     trovato <- FALSE; tempo.calcolato <- ""
#'     for( riga in 1:nrow(MMM)) {
#'       trovato <- FALSE; tempo.calcolato <- ""
#'       if(MMM[riga,global.dataLoader$csv.EVENTName] == first.o) {
#'         # estrai le righe corrispondenti al numero di eventi in avanti da indagare
#'         if( operator == "->" | operator == "!->") row.2.search <- riga+1
#'         if( operator == "-->" | operator == "!-->") row.2.search <- riga:nrow(MMM)
#'         # ora scorrili tutti e guarda quando trova un'occorrenza
#'         trovato <- FALSE; tempo.calcolato <- ""
#'         for( righe2Check in row.2.search) {
#'           trovato <- (second.o == MMM[righe2Check,global.dataLoader$csv.EVENTName] )
#'           if ( trovato == TRUE ) {
#'             tempo.calcolato <- as.numeric(MMM[righe2Check,"pMineR.deltaDate"])-as.numeric(MMM[riga,"pMineR.deltaDate"])
#'             break;
#'           }
#'         }
#'         # Considera un TRUE, se lo trovi. Questo perche' di default tutte le ricerche
#'         # si basano sulla prima occorrenza trovata: vedi se il true diventa false e/o viceversa:
#'         risultato <- trovato
#'         output.riga <- c( output.riga , risultato )
#'         output.tempi <- c( output.tempi , tempo.calcolato )
#'       } else {
#'         output.riga <- c( output.riga , "" )
#'         output.tempi <- c( output.tempi , "" )
#'       }
#'     }
#' 
#'     browser()
#'     toRet <- list()
#'     toRet.array <- output.riga
#'     toREt.time <- output.tempi
#'     toRet.any <- ( sum( output.riga==TRUE ) == sum( output.riga != "" ) )
#'     toRet.some <- ( sum(output.riga==TRUE) != 0 )
#'     return( toRet )
#'     #
#'     #
#'     # browser()
#'     # FO <- which( event.sequence %in% first.o)
#'     # SO <- which( event.sequence %in% second.o)
#'     #
#'     # if( operator == "->") {
#'     #   if (sum(unlist(lapply( FO, function(i) { (i+1) %in% SO} ))) == 0 ) return( "FALSE" )
#'     #   else return( "TRUE" )
#'     # }
#'     # if( operator == "!->") {
#'     #   if (sum(unlist(lapply( FO, function(i) { (i+1) %in% SO} ))) == 0 ) return( "FALSE" )
#'     #   else return( "TRUE" )
#'     # }
#'     # if( operator == "-->") {
#'     #   if (sum(unlist(lapply( FO, function(i) { i < SO} ))) == 0 ) return( "FALSE" )
#'     #   else return( "TRUE" )
#'     # }
#'     # if( operator == "!-->") {
#'     #   if (sum(unlist(lapply( FO, function(i) { i < SO} ))) == 0 ) return( "TRUE" )
#'     #   else return( "FALSE" )
#'     # }
#'   }
#'   #=================================================================================
#'   # getStats
#'   #=================================================================================
#'   getStats <- function()  {
#'     matriceAlias <- c()
#'     if(length(names(global.processInstances.toSymbol)) > 0) {
#'       matriceAlias <- cbind( names(global.processInstances.toSymbol), unlist(global.processInstances.toSymbol))
#'       colnames(matriceAlias)<-c("Event","Alias")
#'     }
#' 
#'     return(
#'       list(
#'         "EFPTs" = global.EFPTs,
#'         "events" = global.dataLoader$arrayAssociativo,
#'         "aliases" = matriceAlias
#'       )
#'     )
#'   }
#'   #=================================================================================
#'   # clearAttributes
#'   #=================================================================================
#'   clearAttributes<-function() {
#'     costructor();
#'   }
#'   #===========================================================
#'   # costructor
#'   # E' il costruttore della classe
#'   #===========================================================
#'   costructor<-function() {
#'     global.processInstances.toSymbol <<- list()
#'     global.dataLoader <<- ''
#'     global.EFPTs <<- c()
#'   }
#'   #===========================================================
#'   costructor();
#'   #===========================================================
#'   return( list(
#'     "loadDataset"=loadDataset,
#'     "defineAlias"=defineAlias,
#'     "getStats"=getStats,
#'     "checkRule"=checkRule
#'   ) )
#' 
#' }
