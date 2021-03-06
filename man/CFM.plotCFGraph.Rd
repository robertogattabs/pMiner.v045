% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/careFlowMiner.wrapper.R
\name{CFM.plotCFGraph}
\alias{CFM.plotCFGraph}
\title{CFM: plot the graph with the stories}
\usage{
CFM.plotCFGraph(
  objCFM,
  depth = 2,
  starting.ID = "root",
  kindOfGraph = "twopi",
  GraphFontsize = "9",
  withPercentages = TRUE,
  relative.percentages = FALSE,
  proportionalPenwidth = TRUE,
  default.arcColor = "Black",
  arr.States.color = c(),
  predictive.model = FALSE,
  predictive.model.outcome = "",
  predictive.model.skipNodeLabel = c(),
  preserve.topology = FALSE,
  set.to.gray = FALSE,
  set.to.gray.color = "WhiteSmoke"
)
}
\arguments{
\item{objCFM}{the \code{careFlowMiner} obj.}

\item{depth}{the depht of the final graph, with the respect of the initial node}

\item{starting.ID}{the starting node (the default is the \code{root} node )}

\item{kindOfGraph}{the shape of the graph. Can be \code{twopi} (by default), \code{neato} or \code{circo}}

\item{GraphFontsize}{set to 9 by default}

\item{withPercentages}{default = \code{TRUE}: do you want percentages, on the arcs?}

\item{relative.percentages}{default = \code{FALSE}. If the parameter \code{withPercentages} is set to TRUE this define the kind of percentages the graph will present on the edges}

\item{proportionalPenwidth}{default = \code{TRUE}. Do the bolder attribute has to be proportional with the percentage?}

\item{default.arcColor}{default = \code{black},  define the default color for the arcs}

\item{arr.States.color}{an array by which you can specify for some nodes (key) the associated color(value). E.g. c("Home"="Black","Hospital"="Red")}

\item{predictive.model}{default = \code{FALSE}. If set to \code{TRUE} the entire graph will work as a predictive mode, with the probability on each node to reach one or mode final states (indicated in \code{predictive.model.outcome})}

\item{predictive.model.outcome}{the array of nodes which represent the goals in predicting the evolution. This makes sense if \code{predictive.model} is set to \code{TRUE}}

\item{predictive.model.skipNodeLabel}{Shit, I forgot the meaning of this parameter}

\item{preserve.topology}{default = \code{FALSE}. If set to YES it will preserve the  topology of the graph even if you change some parameters, by plotting all the nodes, in any case (however, the not useful ones will painted in a softgrey color)}

\item{set.to.gray}{default = \code{FALSE} I don't remember}

\item{set.to.gray.color}{default = \code{WhiteSmoke} the default color for the nodes not used in coputation but only aimed at preserving the topology}
}
\description{
Plot the graph containing the stories
}
\examples{
\dontrun{

# instantiate a dataLoader object
library(pMineR)

 tmp.objDL <- dataLoader()
 invisible(tmp.objDL$load.data.frame( mydata = data.frame(myInputCSVData) , IDName = "PatientID",EVENTName = "clinicalEvent",
  dateColumnName = "STRT_DT", format.column.date = "\%Y-\%m-\%d \%H:\%M:\%S" ))
 DLS <- tmp.objDL$getData()

a <- careFlowMiner()
a$loadDataset(DLS = DLS , dateToColumnName = "END_DT",dateToFormat="\%Y-\%m-\%d \%H:\%M:\%S") 

b <- CFM.plotCFGraph(objCFM = a, depth = 4,kindOfGraph = "neato",default.arcColor = "Red", 
              arr.States.color=c("Deces"="Red","intensive care"="Orange","Recovered"="YellowGreen"),
              predictive.model = TRUE, predictive.model.outcome = "Deces", 
              predictive.model.skipNodeLabel = c("Deces","Recovered"), preserve.topology = TRUE )
grViz(b$script)
}
}
