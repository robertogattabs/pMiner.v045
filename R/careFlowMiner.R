#' A class for a revisited Careflow Mining
#'
#' @description  This is an implementation of the Care Flow Mining algorithm, a bit revisited
#' @import progress
#' @export

careFlowMiner <- function( verbose.mode = FALSE ) {
  lst.nodi <- list()
  MM <- c()
  IDD <- 0
  attr.dateToFormat <- ""
  attr.date.format <- ""
  param.verbose<-''
  intGraph <- ""
  cmpStr <- list()
  loadedDataset <- list()
  
  # ---------------------------------------------------------------
  # add a node to the tree
  # ---------------------------------------------------------------
  add.node <- function( father = NA , evento ) {
    id <- as.character(IDD)
    IDD <<- IDD + 1
    
    old.colNames <- colnames(MM)
    old.rowNames <- rownames(MM)
    
    MM <<- cbind(MM , rep("",nrow(MM)))
    MM <<- rbind(MM , rep("",ncol(MM)))
    colnames(MM) <<- c(old.colNames,id)
    rownames(MM) <<- c(old.rowNames,id)
    
    if( is.na(father) ) {
      MM[ "root", id] <<- evento; 
    } else {
      MM[ father, id] <<- evento; 
    }
    return(id)
  }
  # ---------------------------------------------------------------
  # ritorna l'ID di un elemento di una sequenza
  # ---------------------------------------------------------------
  get.id <- function(sequenza, i, fromID, debug = FALSE  ) {
    if( debug == TRUE ) browser()
    
    if(length(which(MM[fromID,] == sequenza[i])) == 0) return( NA )
    if(length(which(MM[fromID,] == sequenza[i])) > 1 ) browser()
    
    col.id <- colnames(MM)[which(MM[fromID,] == sequenza[i])]
    return( col.id )
  }
  # ---------------------------------------------------------------
  # aggiungi un path all'albero
  # ---------------------------------------------------------------
  add.path <- function( sequenza, debug = FALSE, col.dateFrom=c(), col.dateTo=c() , ID="", UM="days" ,IPP="") {
    old.id <- "root"
    for( i in 1:length(sequenza)) {
      id <- get.id( sequenza, i, fromID = old.id , debug = debug )
      if( is.na(id) ) {
        if( i == 1) {
          id <- add.node(evento = sequenza[i])
        } else {
          id <- add.node(evento = sequenza[i], father = old.id)
        }
        lst.nodi[[ id ]] <<- list("evento" = sequenza[i], "hits" = 1, "depth" = i, "IPP"=IPP)
      } else {
        lst.nodi[[id]]$hits <<- lst.nodi[[id]]$hits + 1
      }
      
      if(length(col.dateFrom)>0) {
        lst.nodi[[id]]$activationDates <<- c( lst.nodi[[id]]$activationDates , col.dateFrom[i]) 
        lst.nodi[[id]]$IPP <<- unique(c( lst.nodi[[id]]$IPP , IPP ))
        if(length(col.dateTo)>0) {
          deltaTime <- difftime(as.Date(col.dateTo[i],format = attr.dateToFormat),as.Date(col.dateFrom[i],format = attr.date.format),units = "days")
          lst.nodi[[id]]$duration <<- c(lst.nodi[[id]]$duration , deltaTime)
        }  
      } 
      old.id <- id
    }
  }
  # ---------------------------------------------------------------
  # canonical function to load the data.
  # ---------------------------------------------------------------  
  loadDataset <- function( inputData , dateToColumnName=NA , dateToFormat = "") {
    DLS <- inputData
    attr.date.format <<- DLS$csv.date.format
    attr.dateToFormat <<- dateToFormat
    lst.nodi[[ "root" ]] <<- list("evento" = "root", "hits" = length(DLS$pat.process), "depth" = 0, "duration"=c())
    loadedDataset <<-  DLS
    
    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(names(pat.process)), style = 3)
    pb.ct <- 0
    
    for( ID in names(DLS$pat.process) ) {
      pb.ct <- pb.ct + 1;
      if(param.verbose == TRUE) setTxtProgressBar(pb, pb.ct)
      
      sequenza <- DLS$pat.process[[ID]][,DLS$csv.EVENTName]
      col.dateFrom <- DLS$pat.process[[ID]][,DLS$csv.dateColumnName]
      if(!is.na(dateToColumnName)) {
        col.dateTo <- DLS$pat.process[[ID]][,dateToColumnName]  
      } else { col.dateTo <- c() }
      
      # cat("\n ID: (",ID,")",sequenza)
      add.path(sequenza = sequenza, col.dateFrom = col.dateFrom, col.dateTo = col.dateTo, IPP = ID )
      lst.nodi[[ "root" ]]$hits <-  lst.nodi[[ "root" ]]$hits + 1
    }
  }
  # ---------------------------------------------------------------
  # retrieve the data structure
  # ---------------------------------------------------------------   
  getDataStructure <- function() {
    
    arr.depth <- unlist(lapply(1:length(lst.nodi),function(i) {lst.nodi[[i]]$depth} ))
    arr.freq <- unlist(lapply(1:length(lst.nodi),function(i) {lst.nodi[[i]]$hits} ))
    arr.mean.duration <- unlist(lapply(1:length(lst.nodi),function(i) { if(length(lst.nodi[[i]]$duration)>0) {return(mean(lst.nodi[[i]]$duration))} else{return(0)}   } ))
    nomi <- as.character(unlist(lapply(1:length(lst.nodi),function(i) { names(lst.nodi)[i]} )))
    mtr.res <- cbind( nomi , arr.freq , arr.mean.duration, arr.depth )
    colnames(mtr.res) <- c("ID","freq","mean.duration","depth")
    rownames(mtr.res) <- nomi
    
    return(
      list("MM"=MM,
           "lst.nodi"=lst.nodi,
           "mtr.res"=mtr.res)
    )
  }
  # ---------------------------------------------------------------
  # retrieve the data structure
  # ---------------------------------------------------------------   
  plotCFGraph <- function(  depth= 2 , starting.ID = "root", currentLevel = 0, total.hits = 0,
                        kindOfGraph = "twopi", GraphFontsize = "9" , 
                        # withPercentages = TRUE, relative.percentages = FALSE, 
                        proportionalPenwidth=TRUE , default.arcColor = "Black",
                        arr.States.color=c(),
                        predictive.model = FALSE, predictive.model.outcome = "", predictive.model.skipNodeLabel = c(),
                        preserve.topology = FALSE, set.to.gray = FALSE, set.to.gray.color= "WhiteSmoke" , debug.it = FALSE,
                        show.far.leaf = FALSE) {
    withPercentages <- TRUE; relative.percentages <- FALSE
    if( starting.ID != "root") {
      if( lst.nodi[[starting.ID]]$depth == depth | 
          ( predictive.model==TRUE & lst.nodi[[starting.ID]]$evento == predictive.model.outcome & preserve.topology == FALSE  ) ) {
        if(lst.nodi[[starting.ID]]$evento == predictive.model.outcome) {
          num.outcome <- lst.nodi[[starting.ID]]$hits
        } else {
          num.outcome <- 0
        }
        return(list("stringa.nodi"=c(),"stringa.archi"=c(),"script"="", "num.outcome" = num.outcome, "sonHits" = lst.nodi[[starting.ID]]$hits))
      }
    }
    if( predictive.model==TRUE & lst.nodi[[starting.ID]]$evento == predictive.model.outcome & preserve.topology == TRUE) {
      set.to.gray <- TRUE
      default.arcColor <- set.to.gray.color
    }
    if(debug.it==TRUE) browser()
    # if( currentLevel > 4 ) browser()
    
    arrId2Jump <- names(which(MM[starting.ID,]!=""))
    if(currentLevel == 0 ) { total.hits <- lst.nodi[[starting.ID]]$hits }
    arr.nodi <- c()
    arr.archi <- c()
    script <- ""
    
    if( currentLevel == 0) {
      arr.nodi <- c(paste( c("'",starting.ID,"' [ label='",lst.nodi[[starting.ID]]$evento,"\n(",total.hits,")', penwidth=3]"),collapse = "" ))
    }
    # if(debug.it==TRUE) browser()
    num.outcome <- 0
    totaleSonHits <- 0
    totale <- lst.nodi[[starting.ID]]$hits
    if( length(arrId2Jump) > 0 ) {
      totale <- sum(unlist(lapply( arrId2Jump, function(x) {lst.nodi[[x]]$hits} )))
      num.outcome <- 0
      
      for( son in arrId2Jump) {
        sonLabel <- lst.nodi[[son]]$evento
        sonHits <- lst.nodi[[son]]$hits
        totaleSonHits <- totaleSonHits + sonHits
        
        arc.fontsize <- "1"
        penwidth <- "1"
        arcColor <- default.arcColor
        arcLabel <- sonHits
        fillColor <- "White"
        
        if( withPercentages == TRUE ) {
          if(relative.percentages == TRUE) {
            percentuale <- as.integer((arcLabel/totale)*100)  
          } else {
            percentuale <- as.integer((arcLabel/total.hits)*100)  
          }
          if( predictive.model == FALSE) {
            arcLabel <- paste( c(percentuale,"%"),collapse =  '')
          } else {
            percentuale <- as.integer((arcLabel/totale)*100)
            arcLabel <- paste( c(as.integer((arcLabel/totale)*100) ,"%"),collapse =  '')  
          }
          arc.fontsize <- "8.5"
        }
        if( proportionalPenwidth == TRUE ) {
          penwidth <- 5*(percentuale/100)+0.2
        }
        if(length(arr.States.color) > 0) {
          if( lst.nodi[[son]]$evento %in% names(arr.States.color)) {
            fillColor <- arr.States.color[ which(names(arr.States.color) == lst.nodi[[son]]$evento)  ]
            if( set.to.gray == TRUE) { fillColor <- set.to.gray.color } 
          }
        }
        
        if(debug.it==TRUE) browser()
        # if( lst.nodi[[son]]$evento == predictive.model.outcome ) browser()
        res <- plotCFGraph( depth = depth , starting.ID = son , currentLevel = currentLevel + 1, total.hits = total.hits,
                        default.arcColor = default.arcColor, arr.States.color = arr.States.color,
                        predictive.model = predictive.model, predictive.model.outcome = predictive.model.outcome, 
                        predictive.model.skipNodeLabel = predictive.model.skipNodeLabel,
                        preserve.topology = preserve.topology, set.to.gray = set.to.gray,
                        set.to.gray.color = set.to.gray.color , debug.it = debug.it,
                        show.far.leaf = show.far.leaf)
        
        
        # nodo.partenza <- lst.nodi[[starting.ID]]$depth + 1
        # quanti.eventi.finali <- sum(unlist(lapply(lst.nodi[[starting.ID]]$IPP , function(IPP) {
        #     sequenza <- c()
        #     if( length(loadedDataset$wordSequence.raw[[IPP]]) >= nodo.partenza) {
        #       sequenza <- loadedDataset$wordSequence.raw[[IPP]][nodo.partenza:length(loadedDataset$wordSequence.raw[[IPP]])] 
        #     }
        #     if( predictive.model.outcome %in% sequenza) return(1)
        #     return(0)
        # })))
        # nodo.partenza <- lst.nodi[[son]]$depth + 1
        nodo.partenza <- lst.nodi[[son]]$depth
        quanti.eventi.finali <- sum(unlist(lapply(unique(lst.nodi[[son]]$IPP) , function(IPP) {
          sequenza <- c()
          if( length(loadedDataset$wordSequence.raw[[IPP]]) >= nodo.partenza) {
            sequenza <- loadedDataset$wordSequence.raw[[IPP]][nodo.partenza:length(loadedDataset$wordSequence.raw[[IPP]])] 
          }
          if( predictive.model.outcome %in% sequenza) return(1)
          return(0)
        })))   
        if( lst.nodi[[son]]$evento == predictive.model.outcome ) quanti.eventi.finali <- lst.nodi[[son]]$hits
        
        if( predictive.model == FALSE) {
          riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n(",sonHits,")' ,  fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
        } else{
          if(sonLabel %in% predictive.model.skipNodeLabel) {
            totale.outcome <- res$num.outcome
            totale.outcome <- quanti.eventi.finali
            riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n(",totale.outcome,")' , color=",default.arcColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          } else {
            totale.outcome <- res$num.outcome
            totale.outcome <- quanti.eventi.finali
            percentuale <- as.integer((totale.outcome/res$sonHits)*100)
            riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n(",totale.outcome,"/",res$sonHits,": ",percentuale,"%)' , color=",default.arcColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          }
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
        }

        if( show.far.leaf & (lst.nodi[[son]]$depth == depth) & (lst.nodi[[son]]$evento != predictive.model.outcome) ) {
          arr.ultimi <- unlist(lapply( lst.nodi[[son]]$IPP, function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )
          # browser()  
          if( length(arr.ultimi) > 0) {
            
            colore.arco <- "grey"
            if( set.to.gray == TRUE ) colore.arco <- set.to.gray.color
            colore.arco <- "grey"
            colore.nodo <- colore.arco
            
            
            tabella.ultimi <- table(arr.ultimi)
            names(tabella.ultimi) <- unlist(lapply( 1:nrow(tabella.ultimi), function(i) { paste(c( names(tabella.ultimi)[i] ,son),collapse = "_") }))
            # ak47 <- unlist(lapply( 1:nrow(tabella.ultimi), function(i) { paste(c( names(tabella.ultimi)[i] ,son),collapse = "_") }))
            for(i in 1:length(tabella.ultimi)) {
              fillColor <- "White"
              if(length(arr.States.color) > 0) {
                if( names(table(arr.ultimi))[i] %in% names(arr.States.color)) {
                  fillColor <- arr.States.color[ which(names(arr.States.color) == names(table(arr.ultimi))[i])  ]
                  if( set.to.gray == TRUE) { fillColor <- set.to.gray.color } 
                }
              }
              
              tmp.str <- paste( c("'",son,"'->'",names(tabella.ultimi)[i],"' [style='dashed', label='', color = '",colore.arco <- "grey","', penwidth = 0.8, arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
              arr.archi <- c( arr.archi , tmp.str)
              # browser()
              tmp.str <- paste( c("'",names(tabella.ultimi)[i],"' [ label='",names(table(arr.ultimi))[i],"\n(",tabella.ultimi[i],"/",res$sonHits,")' , color='",colore.nodo,"', fillcolor = '",fillColor,"' , style = filled]"),collapse = "" )
              arr.nodi <- c( arr.nodi , tmp.str )
            }
          }
        }
        
        arr.nodi <- c( arr.nodi , riga.nodi )
        arr.archi <- c( arr.archi , riga.archi)
        
        if( res$num.outcome > 0 ) num.outcome <- num.outcome +  res$num.outcome
        altri.nodi <- res$arr.nodi
        altri.archi <- res$arr.archi 
        
        
        arr.nodi <- c( arr.nodi , altri.nodi)
        arr.archi <- c( arr.archi , altri.archi)
      }
      
    }
    
    if( currentLevel == 0 ) {
      script <- "
          digraph boxes_and_circles {
            graph [overlap = false, fontsize = ##GraphFontsize##, layout = ##kindOfGraph##]
          
            # several 'node' statements
            node [shape = oval, fontname = Helvetica , fontsize = 9]
            ##NodesPlaceholder##
          
            # several 'edge' statements
            edge [ fontname = Helvetica]
            ##ArcsPlaceholder##
          }" 
      NodesPlaceholder <- paste(arr.nodi,collapse = "\n")
      ArcsPlaceholder <- paste(arr.archi,collapse = "\n")
      script <- str_replace_all( script , "##NodesPlaceholder##", NodesPlaceholder )
      script <- str_replace_all( script , "##ArcsPlaceholder##", ArcsPlaceholder )
      script <- str_replace_all( script , "##kindOfGraph##", kindOfGraph )
      script <- str_replace_all( script , "##GraphFontsize##", GraphFontsize )
    }
    if(  length(arrId2Jump) == 0 ) {
      if(lst.nodi[[starting.ID]]$evento == predictive.model.outcome) {
        num.outcome <- lst.nodi[[starting.ID]]$hits
      }
    }
    return(list("arr.nodi"=arr.nodi,"arr.archi"=arr.archi, "script"=script,"num.outcome" = num.outcome, "sonHits"= totaleSonHits))
  }
  
  plotCFGraphComparison <- function( stratifyFor , stratificationValues, depth = 4, fisher.threshold = 0.1,
                                     checkDurationFromRoot = FALSE, 
                                     hitsMeansReachAGivenFinalState = FALSE, finalStateForHits = c(),
                               arr.States.color=c("Deces"="Red","intensive care"="Orange","Recovered"="YellowGreen"), 
                               debug.it = F, show.far.leaf = FALSE ) {
    
    # stratificationValues <- c(1,2)
    b <- plot.comparison( stratifyFor = stratifyFor, stratificationValues = stratificationValues, depth = depth,
                          fisher.threshold = fisher.threshold, checkDurationFromRoot = checkDurationFromRoot,
                          hitsMeansReachAGivenFinalState = hitsMeansReachAGivenFinalState, finalStateForHits = finalStateForHits,
                          arr.States.color=arr.States.color, set.to.gray = FALSE , set.to.gray.color= "WhiteSmoke",
                          debug.it = debug.it, show.far.leaf = show.far.leaf)
    return(b)
  }
  compare.array <- function( a, b ) {
    if( length( a ) != length( b ) ) return( FALSE )
    tmp <- unlist(lapply( 1:length(a), function(x) { if(a[x]!=b[x])  return(TRUE); return(FALSE) } ))
    if(sum(tmp)>0) return( FALSE )
    return( TRUE )
  }
  getPatientWithSpecificedPath <- function( sequenza ){
    IDName <- loadedDataset$csv.IDName; EventName <- loadedDataset$csv.EVENTName
    arr.ID <- unlist(lapply( names(loadedDataset$pat.process), function(ID) {
      if(compare.array( loadedDataset$pat.process[[ID]][[EventName]][1:min(length(loadedDataset$pat.process[[ID]][[EventName]]),
                                                                           length(sequenza))] , sequenza) ==TRUE  ) return(ID)
      return(NA)
    }))
    arr.ID <- arr.ID[which(!is.na(arr.ID))]
    return(arr.ID)
  }
  
  plot.comparison <- function( stratifyFor, stratificationValues, 
                               fisher.threshold = 0.1, checkDurationFromRoot = FALSE,
                               hitsMeansReachAGivenFinalState = FALSE, finalStateForHits = c(),
                               starting.ID = "root", sequenza =c("root") , currentLevel = 0, 
                               depth = 4, arr.States.color=c(), GraphFontsize = "9" ,
                               set.to.gray = FALSE , set.to.gray.color= "WhiteSmoke", 
                               debug.it = F, show.far.leaf = FALSE) {
    
    IDName <- loadedDataset$csv.IDName; EventName <- loadedDataset$csv.EVENTName
    decoded.seq <- sequenza[ which(sequenza!="root")]
    if( length(decoded.seq) > 0 ) {
      decoded.seq <- unlist(lapply( 1:length(decoded.seq), function(i) { lst.nodi[[decoded.seq[[i]]]]$evento } ))      
    }
    # browser()
    if( lst.nodi[[starting.ID]]$depth == depth ) {
      # browser()
      if( debug.it == TRUE)  browser()
      
      first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
      second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
      
      ID <- getPatientWithSpecificedPath( decoded.seq )
      
      quanti.first <- sum( ID %in% first.ID)
      quanti.second <- sum( ID %in% second.ID)
      
      validi.first.ID <- ID[ID %in% first.ID]
      validi.second.ID <- ID[ID %in% second.ID] 
      
      return(list("stringa.nodi"=c(),"stringa.archi"=c(),"script"="", "sonHits" = lst.nodi[[starting.ID]]$hits,
                  "first.hits" = quanti.first, "second.hits" = quanti.second ,
                  "first.ID" = validi.first.ID, "second.ID" = validi.second.ID, 
                  "first.missed"= (length(first.ID)-length(quanti.first)), 
                  "second.missed"=(length(second.ID)-length(quanti.second))
      ))
    }
    
    arrId2Jump <- names(which(MM[starting.ID,]!=""))
    arr.nodi <- c()
    arr.archi <- c()
    script <- ""
    fillColor <- "White"
    arcLabel <- ""
    arcColor <- "Black"
    penwidth <- 0.5
    arc.fontsize <- 10
    if( set.to.gray == TRUE) { fillColor <- set.to.gray.color;  }
    
    if( currentLevel == 0) {
      arr.nodi <- c(paste( c("'",starting.ID,"' [ label='",lst.nodi[[starting.ID]]$evento,"', penwidth=3]"),collapse = "" ))
    }    
    
    num.outcome <- 0
    totaleSonHits <- 0
    # totale <- lst.nodi[[starting.ID]]$hits
    
    if( length(arrId2Jump) > 0 ) {
      # totale <- sum(unlist(lapply( arrId2Jump, function(x) {lst.nodi[[x]]$hits} )))
      # browser()
      num.outcome <- 0
      
      for( son in arrId2Jump) {
        sonLabel <- lst.nodi[[son]]$evento
        sonHits <- lst.nodi[[son]]$hits
        totaleSonHits <- totaleSonHits + sonHits
        
        # percentuale <- as.integer((sonHits/totale)*100)
        # penwidth <- 5*(percentuale/100)+0.2
        
        res <- plot.comparison( stratifyFor = stratifyFor, stratificationValues = stratificationValues, 
                                starting.ID = son, currentLevel = currentLevel + 1, sequenza = c(sequenza,son),
                                GraphFontsize = GraphFontsize, fisher.threshold = fisher.threshold,
                                checkDurationFromRoot = checkDurationFromRoot,
                                hitsMeansReachAGivenFinalState = hitsMeansReachAGivenFinalState, finalStateForHits = finalStateForHits,
                                depth = depth, arr.States.color = arr.States.color,
                                set.to.gray = set.to.gray, show.far.leaf = show.far.leaf)
        # browser()
        matriceFisher <- matrix( c(res$first.hits, res$first.missed , res$second.hits , res$second.missed), byrow = F, ncol=2 )
        wilcoxTest.p <- NA
        if(checkDurationFromRoot == TRUE) {
          if( length(res$first.ID) > 0 & length(res$second.ID) > 0 & starting.ID!="root") {
            wilcoxTest.p <- suppressWarnings(wilcox.test( unlist(lapply( res$first.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) })),
                                                          unlist(lapply( res$second.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) })))$p.value)
            wilcoxTest.p <- as.numeric(format(wilcoxTest.p, digits = 3))
          } else {
            wilcoxTest.p <- 1
          }
        }
        if( is.na(wilcoxTest.p) ) wilcoxTest.p <- 1
        # cat("\n", son)
        # if( son %in% c("0","1","9","37","2") ) browser()
        # cat("\n FT: ",sum(matriceFisher[1,])," - ", ((sum(matriceFisher[2,])*fisher.threshold)) )
        if(checkDurationFromRoot == FALSE) {
          if( sum(matriceFisher[1,]) > ((sum(matriceFisher[2,])*fisher.threshold)) ) { 
            # browser()
            p.value <- format(fisher.test(matriceFisher)$p.value,digits = 3)
            fillColor <- "White"; 
            borderColor <- "Black"
            fontColor <- "Black"
            arcColor <- "Black"
            
            if( p.value < 0.05) fillColor <- "Yellow";
            if( p.value < 0.01) fillColor <- "Yellow";
          } else {
            p.value = "NA"
            set.to.gray <- TRUE;
            fillColor <- set.to.gray.color; 
            borderColor <- "Gray"
            fontColor <- "Gray"
            arcColor <- "Gray"
          }
        } else {
          if( ((length(res$first.ID)+length(res$second.ID)) > 7) & length(res$first.ID)>3 & length(res$second.ID)>3 ) { 
            # browser()
            fillColor <- "White"; 
            borderColor <- "Black"
            fontColor <- "Black"
            arcColor <- "Black"
            
            if( wilcoxTest.p < 0.05) fillColor <- "Yellow";
            if( wilcoxTest.p < 0.01) fillColor <- "Yellow";
            p.value <- wilcoxTest.p
          } else {
            p.value = "NA"
            set.to.gray <- TRUE;
            fillColor <- set.to.gray.color; 
            borderColor <- "Gray"
            fontColor <- "Gray"
            arcColor <- "Gray"
          }
        }

        if(checkDurationFromRoot == FALSE) {
          if( hitsMeansReachAGivenFinalState == TRUE ) {
            # if( son == "22") browser()
            fillColor <- "White";
            
            morti.first <- sum(unlist(lapply( res$first.ID , function(IPP) {
              return(finalStateForHits %in% loadedDataset$pat.process[[IPP]][  , loadedDataset$csv.EVENTName ])
            })))
            morti.second <- sum(unlist(lapply( res$second.ID , function(IPP) {
              return(finalStateForHits %in% loadedDataset$pat.process[[IPP]][  , loadedDataset$csv.EVENTName ])
            })))            
            totali.first <- length(res$first.ID)
            totali.second <- length(res$second.ID) 
            bbb <- matrix( c(morti.first, totali.first, morti.second , totali.second ), nrow=2)
            p.value.fisher <- fisher.test(bbb)$p.value

            res$first.hits <- as.numeric(format(morti.first / totali.first,digits = 2))
            res$second.hits <- as.numeric(format(morti.second / totali.second,digits = 2))

            # res$first.hits <- morti.first
            # res$second.hits <- morti.second
            
            p.value <- p.value.fisher
            p.value <- format(p.value,digits = 3)

            if( p.value < 0.05) fillColor <- "Yellow";

            if( (morti.first + morti.second) < 10  ) {
              p.value = "NA"
              set.to.gray <- TRUE;
              fillColor <- set.to.gray.color; 
              borderColor <- "Gray"
              fontColor <- "Gray"
              arcColor <- "Gray"
           }
            
          }
          
          ratio.hits <- format( (res$first.hits / res$second.hits) , digits = 2)
          riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n",res$first.hits,"/",res$second.hits,"(",ratio.hits,")","\n p = ",p.value,"' ,  fontcolor = ",fontColor,", color = ",borderColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
        } else {
          a <- unlist(lapply( res$first.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) }))
          b <- unlist(lapply( res$second.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) }))
          a <- as.integer(mean(a)/(60*24))
          b <- as.integer(mean(b)/(60*24))
          riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n",a,"/",b,"\n p = ",p.value,"' ,  fontcolor = ",fontColor,", color = ",borderColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
          
        }
        
        if( show.far.leaf & (lst.nodi[[son]]$depth == depth ) &
            ( hitsMeansReachAGivenFinalState == FALSE ) &
            ( checkDurationFromRoot == FALSE )) {
          arr.ultimi <- unlist(lapply( lst.nodi[[son]]$IPP, function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )
          
          arr.ultimi.first <- unlist(lapply( res$first.ID, function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )
          arr.ultimi.second <- unlist(lapply( res$second.ID , function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )          
          
          colore.arco <- "grey"
          if( set.to.gray == TRUE ) colore.arco <- set.to.gray.color
          colore.arco <- "grey"
          colore.nodo <- colore.arco
          
          tabella.ultimi.first <- table(arr.ultimi.first)
          tabella.ultimi.second <- table(arr.ultimi.second)
        
          arr.possibili.stati <- unique( c( names(tabella.ultimi.first),names(tabella.ultimi.second) ))
          
          for(i in 1:length(arr.possibili.stati)) {

            if(  arr.possibili.stati[i] %in% names(tabella.ultimi.first) ) {
              quanti.first <- tabella.ultimi.first[ arr.possibili.stati[i] ]
            } else {
              quanti.first <- 0
            }
            if(  arr.possibili.stati[i] %in% names(tabella.ultimi.second) ) {
              quanti.second <- tabella.ultimi.second[ arr.possibili.stati[i] ]
            } else {
              quanti.second <- 0
            }
            
            matriceFisher.leaf <- matrix( c(quanti.first, res$first.missed , quanti.second , res$second.missed), byrow = F, ncol=2 )
            p.value <- "NA"
            fillColor <- "White";
            if(checkDurationFromRoot == FALSE) {
              if( sum(matriceFisher.leaf[1,]) > ((sum(matriceFisher.leaf[2,])*fisher.threshold)) ) { 
                # browser()
                p.value <- format(fisher.test(matriceFisher.leaf)$p.value,digits = 3)
                
                if( p.value < 0.05) fillColor <- "Yellow";
                if( p.value < 0.01) fillColor <- "Yellow";
              } else {
                p.value = "NA"
                set.to.gray <- TRUE;
              }
            }
            
            nome.nodo.tmp <- paste(c(arr.possibili.stati[i],"_",son),collapse='')
            
            tmp.str <- paste( c("'",son,"'->'",nome.nodo.tmp,"' [style='dashed', label='', color = '",colore.arco <- "grey","', penwidth = 0.8, arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
            arr.archi <- c( arr.archi , tmp.str)
            tmp.str <- paste( c("'",nome.nodo.tmp,"' [ label='",arr.possibili.stati[i],"\n(",quanti.first ,"/",quanti.second,")\n p = ",p.value,"' , color='",colore.nodo,"', fillcolor = '",fillColor,"' , style = filled]"),collapse = "" )            
            arr.nodi <- c( arr.nodi , tmp.str )
          }
          
        }        

        arr.nodi <- c( arr.nodi , riga.nodi )
        arr.archi <- c( arr.archi , riga.archi)
        
        # if( res$num.outcome > 0 ) num.outcome <- num.outcome +  res$num.outcome
        altri.nodi <- res$arr.nodi
        altri.archi <- res$arr.archi 
        
        arr.nodi <- c( arr.nodi , altri.nodi)
        arr.archi <- c( arr.archi , altri.archi)   
        
      }
    }
    # browser()
    if( currentLevel == 0 ) {
      script <- "
          digraph boxes_and_circles {
            graph [overlap = false, fontsize = ##GraphFontsize##, layout = neato]
          
            # several 'node' statements
            node [shape = oval, fontname = Helvetica , fontsize = 9]
            ##NodesPlaceholder##
          
            # several 'edge' statements
            edge [ fontname = Helvetica]
            ##ArcsPlaceholder##
          }" 
      NodesPlaceholder <- paste(arr.nodi,collapse = "\n")
      ArcsPlaceholder <- paste(arr.archi,collapse = "\n")
      script <- str_replace_all( script , "##NodesPlaceholder##", NodesPlaceholder )
      script <- str_replace_all( script , "##ArcsPlaceholder##", ArcsPlaceholder )
      script <- str_replace_all( script , "##GraphFontsize##", GraphFontsize )
    }
    # if(  length(arrId2Jump) == 0 ) {
    #   if(lst.nodi[[starting.ID]]$evento == predictive.model.outcome) {
    #     num.outcome <- lst.nodi[[starting.ID]]$hits
    #   }
    # }
    # browser()
    first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
    second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
    
    ID <- getPatientWithSpecificedPath( decoded.seq )
    
    quanti.first <- sum( ID %in% first.ID)
    quanti.second <- sum( ID %in% second.ID)
    
    validi.first.ID <- ID[ID %in% first.ID]
    validi.second.ID <- ID[ID %in% second.ID]
    # browser()
    return(list("arr.nodi"=arr.nodi,"arr.archi"=arr.archi, "script"=script,"num.outcome" = num.outcome, 
                "first.hits" = quanti.first, "second.hits" = quanti.second ,
                "first.missed"= (length(first.ID)-length(quanti.first)), "second.missed"=(length(second.ID)-length(quanti.second)),
                "first.ID" = validi.first.ID, "second.ID" = validi.second.ID,  
                "sonHits"= totaleSonHits))
  }
  # The OLD ONE
  old.plot.comparison <- function( stratifyFor, stratificationValues, 
                               fisher.threshold = 0.1, checkDurationFromRoot = FALSE,
                               starting.ID = "root", sequenza =c("root") , currentLevel = 0, 
                               depth = 4, arr.States.color=c(), GraphFontsize = "9" ,
                               set.to.gray = FALSE , set.to.gray.color= "WhiteSmoke", 
                               debug.it = F) {
    
    IDName <- loadedDataset$csv.IDName; EventName <- loadedDataset$csv.EVENTName
    decoded.seq <- sequenza[ which(sequenza!="root")]
    if( length(decoded.seq) > 0 ) {
      decoded.seq <- unlist(lapply( 1:length(decoded.seq), function(i) { lst.nodi[[decoded.seq[[i]]]]$evento } ))      
    }
    # browser()
    if( lst.nodi[[starting.ID]]$depth == depth ) {
      # browser()
      if( debug.it == TRUE)  browser()
      
      first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
      second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
      
      ID <- getPatientWithSpecificedPath( decoded.seq )
      
      quanti.first <- sum( ID %in% first.ID)
      quanti.second <- sum( ID %in% second.ID)
      
      validi.first.ID <- ID[ID %in% first.ID]
      validi.second.ID <- ID[ID %in% second.ID]      
      
      return(list("stringa.nodi"=c(),"stringa.archi"=c(),"script"="", "sonHits" = lst.nodi[[starting.ID]]$hits,
                  "first.hits" = quanti.first, "second.hits" = quanti.second ,
                  "first.ID" = validi.first.ID, "second.ID" = validi.second.ID, 
                  "first.missed"= (length(first.ID)-length(quanti.first)), 
                  "second.missed"=(length(second.ID)-length(quanti.second))
      ))
    }
    
    arrId2Jump <- names(which(MM[starting.ID,]!=""))
    arr.nodi <- c()
    arr.archi <- c()
    script <- ""
    fillColor <- "White"
    arcLabel <- ""
    arcColor <- "Black"
    penwidth <- 0.5
    arc.fontsize <- 10
    if( set.to.gray == TRUE) { fillColor <- set.to.gray.color;  }
    
    if( currentLevel == 0) {
      arr.nodi <- c(paste( c("'",starting.ID,"' [ label='",lst.nodi[[starting.ID]]$evento,"', penwidth=3]"),collapse = "" ))
    }    
    
    num.outcome <- 0
    totaleSonHits <- 0
    # totale <- lst.nodi[[starting.ID]]$hits
    
    if( length(arrId2Jump) > 0 ) {
      # totale <- sum(unlist(lapply( arrId2Jump, function(x) {lst.nodi[[x]]$hits} )))
      # browser()
      num.outcome <- 0
      
      for( son in arrId2Jump) {
        sonLabel <- lst.nodi[[son]]$evento
        sonHits <- lst.nodi[[son]]$hits
        totaleSonHits <- totaleSonHits + sonHits
        
        # percentuale <- as.integer((sonHits/totale)*100)
        # penwidth <- 5*(percentuale/100)+0.2
        
        res <- plot.comparison( stratifyFor = stratifyFor, stratificationValues = stratificationValues, 
                                starting.ID = son, currentLevel = currentLevel + 1, sequenza = c(sequenza,son),
                                GraphFontsize = GraphFontsize, fisher.threshold = fisher.threshold,
                                checkDurationFromRoot = checkDurationFromRoot,
                                depth = depth, arr.States.color = arr.States.color,
                                set.to.gray = set.to.gray)
        # browser()
        matriceFisher <- matrix( c(res$first.hits, res$first.missed , res$second.hits , res$second.missed), byrow = F, ncol=2 )
        wilcoxTest.p <- NA
        if(checkDurationFromRoot == TRUE) {
          if( length(res$first.ID) > 0 & length(res$second.ID) > 0 & starting.ID!="root") {
            wilcoxTest.p <- suppressWarnings(wilcox.test( unlist(lapply( res$first.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) })),
                                                          unlist(lapply( res$second.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) })))$p.value)
            wilcoxTest.p <- as.numeric(format(wilcoxTest.p, digits = 3))
          } else {
            wilcoxTest.p <- 1
          }
        }
        if( is.na(wilcoxTest.p) ) wilcoxTest.p <- 1
        # cat("\n", son)
        # if( son %in% c("0","1","9","37","2") ) browser()
        # cat("\n FT: ",sum(matriceFisher[1,])," - ", ((sum(matriceFisher[2,])*fisher.threshold)) )
        if(checkDurationFromRoot == FALSE) {
          if( sum(matriceFisher[1,]) > ((sum(matriceFisher[2,])*fisher.threshold)) ) { 
            # browser()
            p.value <- format(fisher.test(matriceFisher)$p.value,digits = 3)
            fillColor <- "White"; 
            borderColor <- "Black"
            fontColor <- "Black"
            arcColor <- "Black"
            
            if( p.value < 0.05) fillColor <- "Yellow";
            if( p.value < 0.01) fillColor <- "Yellow";
          } else {
            p.value = "NA"
            set.to.gray <- TRUE;
            fillColor <- set.to.gray.color; 
            borderColor <- "Gray"
            fontColor <- "Gray"
            arcColor <- "Gray"
          }
        } else {
          if( ((length(res$first.ID)+length(res$second.ID)) > 7) & length(res$first.ID)>3 & length(res$second.ID)>3 ) { 
            # browser()
            fillColor <- "White"; 
            borderColor <- "Black"
            fontColor <- "Black"
            arcColor <- "Black"
            
            if( wilcoxTest.p < 0.05) fillColor <- "Yellow";
            if( wilcoxTest.p < 0.01) fillColor <- "Yellow";
            p.value <- wilcoxTest.p
          } else {
            p.value = "NA"
            set.to.gray <- TRUE;
            fillColor <- set.to.gray.color; 
            borderColor <- "Gray"
            fontColor <- "Gray"
            arcColor <- "Gray"
          }
        }
        
        if(checkDurationFromRoot == FALSE) {
          ratio.hits <- format( (res$first.hits / res$second.hits) , digits = 2)
          riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n",res$first.hits,"/",res$second.hits,"(",ratio.hits,")","\n p = ",p.value,"' ,  fontcolor = ",fontColor,", color = ",borderColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
        } else {
          a <- unlist(lapply( res$first.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) }))
          b <- unlist(lapply( res$second.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) }))
          a <- as.integer(mean(a)/(60*24))
          b <- as.integer(mean(b)/(60*24))
          riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n",a,"/",b,"\n p = ",p.value,"' ,  fontcolor = ",fontColor,", color = ",borderColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
          
        }
        
        arr.nodi <- c( arr.nodi , riga.nodi )
        arr.archi <- c( arr.archi , riga.archi)
        
        # if( res$num.outcome > 0 ) num.outcome <- num.outcome +  res$num.outcome
        altri.nodi <- res$arr.nodi
        altri.archi <- res$arr.archi 
        
        arr.nodi <- c( arr.nodi , altri.nodi)
        arr.archi <- c( arr.archi , altri.archi)   
        
      }
    }
    # browser()
    if( currentLevel == 0 ) {
      script <- "
          digraph boxes_and_circles {
            graph [overlap = false, fontsize = ##GraphFontsize##, layout = neato]
          
            # several 'node' statements
            node [shape = oval, fontname = Helvetica , fontsize = 9]
            ##NodesPlaceholder##
          
            # several 'edge' statements
            edge [ fontname = Helvetica]
            ##ArcsPlaceholder##
          }" 
      NodesPlaceholder <- paste(arr.nodi,collapse = "\n")
      ArcsPlaceholder <- paste(arr.archi,collapse = "\n")
      script <- str_replace_all( script , "##NodesPlaceholder##", NodesPlaceholder )
      script <- str_replace_all( script , "##ArcsPlaceholder##", ArcsPlaceholder )
      script <- str_replace_all( script , "##GraphFontsize##", GraphFontsize )
    }
    # if(  length(arrId2Jump) == 0 ) {
    #   if(lst.nodi[[starting.ID]]$evento == predictive.model.outcome) {
    #     num.outcome <- lst.nodi[[starting.ID]]$hits
    #   }
    # }
    # browser()
    first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
    second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
    
    ID <- getPatientWithSpecificedPath( decoded.seq )
    
    quanti.first <- sum( ID %in% first.ID)
    quanti.second <- sum( ID %in% second.ID)
    
    validi.first.ID <- ID[ID %in% first.ID]
    validi.second.ID <- ID[ID %in% second.ID]
    # browser()
    return(list("arr.nodi"=arr.nodi,"arr.archi"=arr.archi, "script"=script,"num.outcome" = num.outcome, 
                "first.hits" = quanti.first, "second.hits" = quanti.second ,
                "first.missed"= (length(first.ID)-length(quanti.first)), "second.missed"=(length(second.ID)-length(quanti.second)),
                "first.ID" = validi.first.ID, "second.ID" = validi.second.ID,  
                "sonHits"= totaleSonHits))
  }  
  pathBeetweenStackedNodes <- function( fromState , toState, stratifyFor = "" , minPath = FALSE, stratificationValues , fisher.threshold = 0.1,
                                        kindOfGraph = "dot", arcColor = "black", arc.fontsize = 10, arc.fontcolor = "red",
                                        arr.States.color=c(), set.to.gray.color= "WhiteSmoke", p.value.threshold = 0.05 ) {
    
    stratify <- FALSE
    parameter.arcColor <- arcColor
    parameter.arc.fontcolor <- arc.fontcolor
    if( stratifyFor != "" ) stratify <- TRUE
    # browser()
    # get all the paths with at least one occurrence
    a <- unlist(lapply(1:length(loadedDataset$wordSequence.raw), function(i) {  
      a <- which(loadedDataset$wordSequence.raw[[i]]==fromState)
      b <- which(loadedDataset$wordSequence.raw[[i]]==toState)
      if(length(a)==0 | length(b)==0) return(FALSE)
      if( min(a) < min(b) ) return(TRUE) 
      return(FALSE)  
    }  ))
    IPP.all <- names(loadedDataset$wordSequence.raw)[a]
    
    # extract the possible occurencies
    lst.path <- lapply( IPP.all , function(IPP) {
      a <- which(loadedDataset$wordSequence.raw[[IPP]]==fromState)
      b <- which(loadedDataset$wordSequence.raw[[IPP]]==toState)   
      if( minPath == TRUE ) { inizio <- max(a[a < b])
      } else {  inizio <- min(a) }
      fine <- min(b[b>inizio])  
      return( loadedDataset$wordSequence.raw[[IPP]][inizio:fine] )
    })
    str.path <- unlist(lapply( lst.path, function(x) {paste( x, collapse = "->" )}))
    kind.of.path <- table(str.path)
    numero.in.From <- sum(kind.of.path); numero.in.To <- sum(kind.of.path)
    
    if( stratify == TRUE ) {
      IDName <- loadedDataset$csv.IDName;
      first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
      second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
      first.ID <- first.ID[which( first.ID %in% IPP.all)]
      second.ID <- second.ID[which( second.ID %in% IPP.all)]  
      first.kind.of.path <- table(str.path[which(IPP.all %in% first.ID)])
      second.kind.of.path <- table(str.path[which(IPP.all %in% second.ID)])
    }
    
    nodeColor <- "White"
    if( fromState %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==fromState)]
    if( stratify == FALSE ) {
      nodo.inizio <- paste( c("'fromState' [ label='",fromState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')  
    } else {
      nodo.inizio <- paste( c("'fromState' [ label='",fromState,"\n(",length(first.ID),"/",length(second.ID),")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
    }
    
    nodo.fine <- paste( c("'toState' [ label='",toState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = White , style = filled]"),collapse = '')
    
    str.fatte <- c()
    ct <- 1
    arr.nuovi.nodi <- c(); arr.nuovi.archi <- c()
    for( i in 1:length(lst.path) ) {
      # nodeColor <- "White"
      
      stringa.sequenza <- paste( lst.path[[i]], collapse = "->" )
      
      numero.ricorrenze <- as.numeric(kind.of.path[which( names(kind.of.path) == stringa.sequenza)])
      if( stratify == TRUE ) {
        first.ricorrenza <- as.numeric(first.kind.of.path[which( names(first.kind.of.path) == stringa.sequenza)])
        second.ricorrenza <- as.numeric(second.kind.of.path[which( names(second.kind.of.path) == stringa.sequenza)])
        if(length(first.ricorrenza)==0) first.ricorrenza <- 0
        if(length(second.ricorrenza)==0) second.ricorrenza <- 0
        first.totale <- sum(first.kind.of.path)
        second.totale <- sum(second.kind.of.path)
        # browser()
        piccolaM <- matrix(  c( first.ricorrenza , second.ricorrenza , (first.totale-first.ricorrenza) , (second.totale-second.ricorrenza) ), nrow=2 , byrow = T )
        p.value <- fisher.test(piccolaM)$p.value
        p.value <- format(p.value,digits = 3)
        
        aaa <- ((sum(piccolaM[2,])*fisher.threshold))
        bbb <-  sum(piccolaM[1,])
        if( bbb > aaa ) Fisher.valido <- TRUE
        else Fisher.valido <- FALSE
      }
      
      penwidth = 0.3 + 3 * (numero.ricorrenze / numero.in.From)
      
      if( !(stringa.sequenza %in% str.fatte) ) { 
        
        for( pos in 2:length(lst.path[[i]]) ) {
          
          nodeColor <- "White"
          nodeBorderColor <- "Black"
          nodeFontColor <- "Black"
          nomeNodo <- as.character(ct)
          nomeNodoPrecedente <- as.character(ct - 1)
          labelNuovoNodo <- lst.path[[i]][[pos]]
          
          # se lunghezza e' due o se e' il primo
          if( pos == 2 | length(lst.path[[i]])==2) {
            # se lunghezzza e' 2'
            if( length(lst.path[[i]]) == 2 ) {
              nuovoNodo <- ""
              if( stratify == FALSE ) {
                nuovoArco <- paste( c("'fromState'->'",toState,"' [label='",numero.ricorrenze,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor," ]"),collapse = "" )
              } else {
                if( Fisher.valido == TRUE) {
                  if( p.value < p.value.threshold ) {
                    arcColor <- "Red"
                    arc.fontcolor <- parameter.arc.fontcolor                      
                  } else {
                    arcColor <- parameter.arcColor
                    arc.fontcolor <- "Black"
                  }
                } else {
                  arcColor <- set.to.gray.color
                  arc.fontcolor <- set.to.gray.color
                }
                nuovoArco <- paste( c("'fromState'->'",toState,"' [label='",first.ricorrenza,"/",second.ricorrenza," \np=",p.value,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor," ]"),collapse = "" )                  
              }
            } else {
              if( stratify == FALSE ) {
                # See e' il primo
                if( labelNuovoNodo %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==labelNuovoNodo)]
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
                nuovoArco <- paste( c("'fromState'->'",nomeNodo,"' [label='",numero.ricorrenze,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize," , fontcolor = ",arc.fontcolor," ]"),collapse = "" )
              } else {
                if( Fisher.valido == TRUE) {
                  arcColor <- parameter.arcColor
                  arc.fontcolor <- "Black"
                  nodeColor <- "White"
                  if(p.value < p.value.threshold ) nodeColor <- "Yellow"
                  if(p.value < p.value.threshold ) arcColor <- "Red"
                  if(p.value < p.value.threshold ) arc.fontcolor <- parameter.arc.fontcolor
                  # if(p.value < p.value.threshold ) arcColor <- "Red"
                } else {
                  arcColor <- "Gray"
                  arc.fontcolor <- "Gray"
                  nodeColor <- set.to.gray.color
                  nodeBorderColor <- "Gray"
                  nodeFontColor <- "Gray"
                  p.value <- NA
                }
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = ",nodeFontColor,", color = ",nodeBorderColor,", fillcolor = ",nodeColor," , style = filled]"),collapse = '')
                nuovoArco <- paste( c("'fromState'->'",nomeNodo,"' [label='",first.ricorrenza,"/",second.ricorrenza," \np=",p.value,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize," , fontcolor = ",arc.fontcolor," ]"),collapse = "" )
              }
            }
          }  else { 
            # Sono negli altri (fosse anche l'ultimo)
            if( pos == length(lst.path[[i]]) ) nomeNodo <- toState
            # E' l'utimo
            if( nomeNodo == toState)
            {
              if( stratify == FALSE ) {
                if( toState %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==toState)]
                nuovoNodo <- paste( c("'",toState,"' [ label='",toState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
              } else {
                if( toState %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==toState)]
                nuovoNodo <- paste( c("'",toState,"' [ label='",toState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
              }
            }
            else
            {
              if( stratify == FALSE ) {
                # Uno dei tanti
                if( labelNuovoNodo %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==labelNuovoNodo)]
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')                
              } else {
                # Uno dei tanti
                if( Fisher.valido == TRUE) {
                  # arcColor <- parameter.arcColor
                  arc.fontcolor <- parameter.arc.fontcolor
                  nodeColor <- "White"
                  arc.fontcolor <- "Black"
                  if(p.value < p.value.threshold ) nodeColor <- "Yellow"
                  if(p.value < p.value.threshold ) arcColor <- "Red"
                  if(p.value < p.value.threshold ) arc.fontcolor <- parameter.arc.fontcolor
                } else {
                  arcColor <- "Gray"
                  arc.fontcolor <- "Gray"
                  nodeColor <- set.to.gray.color
                  nodeBorderColor <- "Gray"
                  nodeFontColor <- "Gray"
                  p.value <- NA
                }
                # if( labelNuovoNodo %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==labelNuovoNodo)]
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = ",nodeFontColor,", color = ",nodeBorderColor,", fillcolor = ",nodeColor," , style = filled]"),collapse = '')
                # nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')                
              }
            }
            if( stratify == FALSE ) {
              nuovoArco <- paste( c("'",nomeNodoPrecedente,"'->'",nomeNodo,"' [label='', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor,"]"),collapse = "" )
            } else {
              nuovoArco <- paste( c("'",nomeNodoPrecedente,"'->'",nomeNodo,"' [label='', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor,"]"),collapse = "" )
            }
          }
          arr.nuovi.nodi <- c( arr.nuovi.nodi , nuovoNodo )
          arr.nuovi.archi <- c ( arr.nuovi.archi, nuovoArco )
          
          ct <- ct + 1
        }
      }
      str.fatte <- c( str.fatte , stringa.sequenza )
    }
    
    str.nuovi.nodi <- paste( arr.nuovi.nodi, collapse = "\n")
    str.nuovi.archi <- paste( arr.nuovi.archi, collapse = "\n")
    
    script <- "
          digraph boxes_and_circles {
            graph [overlap = false, fontsize = 1, layout = ##kindOfGraph##]
          
            # several 'node' statements
            node [shape = oval, fontname = Helvetica , fontsize = 9]
            ##nodo.inizio##
            ##str.nuovi.nodi##
            
            # several 'edge' statements
            edge [ fontname = Helvetica]
            ##str.nuovi.archi##
          }"
    
    script <- str_replace_all( script , "##nodo.inizio##", nodo.inizio )
    script <- str_replace_all( script , "##str.nuovi.nodi##", str.nuovi.nodi )
    script <- str_replace_all( script , "##str.nuovi.archi##", str.nuovi.archi )
    script <- str_replace_all( script , "##kindOfGraph##", kindOfGraph )
    
    return(script)
  }
  
  
  constructor <- function( verboseMode  ) {
    MM <<- matrix("",ncol=1, nrow=1)
    colnames(MM) <<- c("root")
    rownames(MM) <<- c("root")
    lst.nodi <<- list()
    attr.date.format <<- ""
    attr.dateToFormat <<- ""
    param.verbose <<- verbose.mode
    intGraph <<- create_graph()
    cmpStr <<- list()
    loadedDataset <<- list()
  }
  constructor(verboseMode = verbose.mode)
  return(list(
    "add.node"=add.node,
    "add.path"=add.path,
    "get.id"=get.id,
    "loadDataset"=loadDataset,
    "getDataStructure"=getDataStructure,
    "getPatientWithSpecificedPath"=getPatientWithSpecificedPath,
    "plotCFGraph"=plotCFGraph,
    "plotCFGraphComparison"= plotCFGraphComparison,
    "pathBeetweenStackedNodes"=pathBeetweenStackedNodes
  ))
}