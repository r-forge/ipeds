\name{completions}
\alias{completions}
\title{completions}
\description{
  This function generates an XML file to be uploaded to IPEDS for reporting completions within the academic year. See \url{https://surveys.nces.ed.gov/IPEDS/Downloads/Forms/package_10_80.pdf} for more details.
}
\usage{
}
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
}
\details{
}
\value{
}
\references{
  https://surveys.nces.ed.gov/IPEDS/Downloads/Forms/package_10_80.pdf
}
\author{
  Jason Bryer <jbryer@excelsior.edu>
}
\note{
}


\seealso{
  \code{\link{mapIPEDSFields}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ completions }

