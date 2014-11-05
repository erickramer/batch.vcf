\name{concat.vcf}
\alias{concat.vcf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Concatenates two vcf files tohether
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
concat.vcf(vcf1, vcf2, vcf3 = NULL, tmpdir = tempdir())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vcf1}{
%%     ~~Describe \code{vcf1} here~~
}
  \item{vcf2}{
%%     ~~Describe \code{vcf2} here~~
}
  \item{vcf3}{
%%     ~~Describe \code{vcf3} here~~
}
  \item{tmpdir}{
%%     ~~Describe \code{tmpdir} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (vcf1, vcf2, vcf3 = NULL, tmpdir = tempdir()) 
{
    if (is.null(vcf1)) 
        return(vcf2)
    if (is.null(vcf2)) 
        return(vcf1)
    vcf3 = if (is.null(vcf3)) 
        temp.file(tmpdir = tmpdir)
    else vcf3
    system(paste("bcftools concat", "-o", vcf3, "-O z", vcf1, 
        vcf2))
    return(vcf3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
