\name{mapIPEDSFields}
\alias{mapIPEDSFields}
\title{mapIPEDSFields}
\description{This function creates a mapping between the field (column) names from source data frame and the names used by the ipeds package.}
\usage{
  mapIPEDSFields(programCIPCode, educationalProgramType, educationalProgramName, educationalAwardLevel, ethnicityColumn, genderColumn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{programCIPCode}{The name of the column containing the CIP codes.}
  \item{educationalProgramType}{The name of the column containing the program types.}
  \item{educationalProgramName}{The name of the column containing the program names.}
  \item{educationalAwardLevel}{The name of the column containing the award levels.}
  \item{ethnicityColumn}{The name of the column containing student ethnicities.}
  \item{genderColumn}{The name of the column containing student genders.}
}
\details{}
\value{}
\references{}
\author{Jason Bryer <jbryer@excelsior.edu>}
\note{}
\seealso{}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ipeds }
