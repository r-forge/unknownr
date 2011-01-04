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
  Fast and fun way to discover useful functions in R that you didn't know you didn't know. Cheatsheets, reference cards, tip of the day's, manual indexes are other ways to discover you unkown unkowns, but how do you know you haven't missed something?
  The author hear so many times other users say 'I didn't know R had function ...' that he decided there may be a market for this small utility.
  Using unk() for the first time takes about 15 mins. Your 'knowns' are then saved. You then have a list of known unknowns to work through at your leasure. Once you 'know' all functions you have confidence you haven't missed anything. Chances are you will learn at least one new function that you wished you knew before. 
  When R is upgraded, simply run unk() again. Any new functions added to base will automatically be presented to you, in case you missed them from NEWS.
  If you know you don't need to know a function, then just say you know it. That way it won't pop up in your known unknowns.
}
\value{
   The list of unknowns is silently returned as a character vector.
}
% \references{ }
\author{ Matthew Dowle }
\examples{
   #donotrun
   #unk()  # TODO uncomment this once donotrun syntax checked
}
\keyword{ data }


