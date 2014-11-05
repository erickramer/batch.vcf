\name{shard.vcf}
\alias{shard.vcf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Splits a vcf file into many smaller files for easy computation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shard.vcf(vcf.file, n = 1e+05, directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vcf.file}{
%%     ~~Describe \code{vcf.file} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{directory}{
%%     ~~Describe \code{directory} here~~
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
function (vcf.file, n = 1e+05, directory = NULL) 
{
    write.file = function(f, file.list = c()) {
        tmpdir = paste(directory, "/", "part", floor(length(file.list)/100), 
            sep = "")
        if (!file.exists(tmpdir)) 
            dir.create(tmpdir)
        lines = scanTabix(f)[[1]]
        if (length(lines) > 0) {
            output.file = paste(tempfile(tmpdir = tmpdir), ".vcf.gz", 
                sep = "")
            o = pipe(paste("bgzip -c >", output.file, sep = ""), 
                open = "w")
            writeLines(header, o)
            writeLines(lines, o)
            close(o)
            write.file(f, file.list = c(file.list, output.file))
        }
        else file.list
    }
    index.file = function(filename) {
        system(paste("tabix", filename))
        filename
    }
    if (!file.exists(paste(vcf.file, "tbi", sep = ""))) 
        index.file(vcf.file)
    directory = if (is.null(directory)) 
        gsub(".vcf.gz$", "", vcf.file)
    else directory
    if (!file.exists(directory)) 
        dir.create(directory)
    header = headerTabix(vcf.file)$header
    f = TabixFile(vcf.file, yieldSize = n)
    open(f)
    files = write.file(f)
    close(f)
    sapply(files, index.file)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
