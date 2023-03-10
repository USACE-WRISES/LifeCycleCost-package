% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annual.to.present.R
\name{annual.to.present}
\alias{annual.to.present}
\title{Calculate present value from uniform annual payment

\code{annual.to.present} Compute present value from uniform annual payments
using uniform series present worth factor (uswf)}
\usage{
annual.to.present(i, n, A)
}
\arguments{
\item{i}{discount rate in percent per year}

\item{n}{life span in years}

\item{A}{series of uniform annual payments}
}
\value{
pr.value
}
\description{
Calculate present value from uniform annual payment

\code{annual.to.present} Compute present value from uniform annual payments
using uniform series present worth factor (uswf)
}
\examples{
 
# Result: pr.value = $33,3773 
annual.to.present(0.08, 30, 30000)  

}
\references{
Newnan, Donald G., Ted G. Eschenbach, and Jerome P. Lavelle.
Engineering economic analysis. Vol. 12. Oxford University Press, 2012.
}
