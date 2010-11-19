\name{twelveMonthEnrollment}
\alias{twelveMonthEnrollment}
\title{twelveMonthEnrollment}
\description{This function generates a XML file for the twelve month enrollment survey.}
\usage{}
\arguments{
  \item{data}{A data frame containing the required fields for IPEDS reporting.}
  \item{mappings}{The mapping between the fields in data and IPEDS (see \code{\link{mapIPEDSFields}} function)}
  \item{institutionId}{The IPEDS institution ID the data is being generated for.}
  \item{institutionName}{The name of the institution.}
  \item{documentId}{Use specified.}
  \item{createdDate}{The date the document is created.}
  \item{noteMessage}{Optional note message to include in the generated file.}
  \item{transmissionType}{Either original for first upload or replace.}
  \item{documentType}{Defaults to application}
  \item{reportingPeriod}{}
  \item{undergraduateContactHours}{The total number of undergraduate contact hours for the given reporting period.}
  \item{undergraduateCreditHours}{The total number of undergraduate credit hours for the given reporting period.}
  \item{graduateCreditHours}{The total number of graduate credit hours for the given reporting period.}
}
\details{}
\value{}
\references{https://surveys.nces.ed.gov/IPEDS/Downloads/Forms/package_9_69.pdf, https://surveys.nces.ed.gov/ipeds/}
\author{Jason Bryer <jbryer@excelsior.edu>}
\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ipeds }
