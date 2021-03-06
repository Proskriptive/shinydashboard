% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/box_extensions.R
\name{kpi_metric_box}
\alias{kpi_metric_box}
\title{Create a Metrics Box}
\usage{
kpi_metric_box(..., id = NULL, title = NULL, footer = NULL,
  status = NULL, info, solidHeader = FALSE, background = NULL,
  width = 12, height = NULL, collapsible = TRUE, collapsed = FALSE,
  expandable = TRUE)
}
\arguments{
\item{id}{is the unique id of html elemnt.}

\item{title}{is the title of the Table box.}

\item{footer}{is the footer of the table box.}

\item{background}{is the backgroundcolor of box.}

\item{width}{id the width of the box.}

\item{height}{is the heigth of the box.}

\item{collapsible}{is the box should be collapsibe.}

\item{collapsed}{is the box is collapsed.}

\item{Status}{is the status of the box.}

\item{Info}{is the information about the matrix box.}

\item{solidheader}{is the header of the table box.}
}
\value{
<div class="col-sm-6">
<div class="box">
 <div class="box-header">
 <div class="box-tools pull-right">
 <button class="btn btn-box-tool" data-widget="collapse">
 <i class="fa fa-minus"></i>
 </button>
 </div>
 </div>
 <div class="box-body">
 <div class="row">Matrix box</div>
 </div>
 </div>
 </div>
}
\description{
Create a Metrics Box
}
