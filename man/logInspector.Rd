% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logInspector.R
\name{logInspector}
\alias{logInspector}
\title{A class to perform a preliminary analysis on sequential data for Process Mining issues}
\usage{
logInspector()
}
\description{
This class aims at inspecting an event-log for descriptive analysis purposes. The public methods are:
               \itemize{
               \item \code{logInspector( ) } is the constructor of the class
               \item \code{loadDataset( ) } loads data taken from a \code{dataLoader::getData()} method, into a \code{logInspector()} object
               \item \code{getEventStats() } computes and returns event-related stats, such as absolute and relative events frequency
               \item \code{getProcessStats() } computes and returns process-related stats, such as absolute and relative processes frequency
               }
               In order to better undestand the use of such methods, please visit: www.pminer.info

Parameter for \code{logInspector::plotEventStats()} and \code{logInspector::plotProcessStats()} methods is:
  \itemize{
   \item \code{num } the number of most frequent events/processes to plot
  }
}
