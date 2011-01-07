\name{unk}
\alias{unk}
\title{ Starts GUI interface for unknownR }
\description{
  Starts GUI interface for unknownR.
}
\usage{
unk(fnam)
}
\arguments{
  \item{fnam}{ Full path to data file containing your knowns. By default this is .knowns.Rdata in your home folder. }
}
\details{
  Fast and fun way to discover useful functions in R that you didn't know you didn't know. Cheatsheets, reference cards, tip of the day's, ticking off the index of reference manuals are other ways to discover unknown unknowns, but how do you know you haven't missed something, and how do you do it quickly?
  Using unk() for the first time takes about 15 mins. Your 'knowns' are then saved. You then have a list of known unknowns to work through at your leisure. Once you 'know' all functions you have confidence you haven't missed anything. Chances are you will learn at least one new function that you wished you knew before. 
  When R is upgraded, simply run unk() again. Any new functions added to base will automatically be presented to you, in case you missed them from NEWS.
  Tip: If you know you don't need to know a function, then just say you know it. That way it won't pop up in your known unknowns. Err on the side of 'knowing' when pressing SPACE. That will get you through the list quicker too. You can always come back later to review your knowns again.
}
\value{
   The list of unknowns is silently returned as a character vector.
}
\references{
   Thanks to Greg Snow for his answer on R-help, 28 May 2010. A search returned his answer several months later. This package makes extensive use of the 'after' function in tcltk.
   \url{http://r.789695.n4.nabble.com/Wait-for-keystroke-or-timeout-tp2233895p2234858.html} 
}
\author{ Matthew Dowle }
\examples{
   \dontrun{
   unk()}
}
\keyword{ data }


