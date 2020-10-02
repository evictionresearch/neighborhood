\name{nt}
\alias{nt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Neighborhood racial typology formula}
\description{
Formula to determine the racial and ethnic type of neighborhood
}
\usage{
nt(
  df, 
  GEOID = "GEOID", 
  totraceE = "toteraceE", 
  pWhite = "pWhite", 
  pBlack = "pBlack", 
  pAsian = "pAsian", 
  pLatinx = "pLatinx", 
  pOther = "pOther", 
  NeighType = "NeighType",
  nt_conc = "nt_conc"
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{df}{data frame}
    \item{GEOID}{ = "GEOID", }
    \item{totraceE}{ = "toteraceE", }
    \item{pWhite}{ = "pWhite", }
    \item{pBlack}{ = "pBlack", }
    \item{pAsian}{ = "pAsian", }
    \item{pLatinx}{ = "pLatinx", }
    \item{pOther}{ = "pOther", }
    \item{NeighType}{ = "NeighType",}
    \item{nt_conc}{ = "nt_conc"}
  }
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
% }
\references{
Hall, Matthew, Kyle Crowder, and Amy Spring. 2015. “Neighborhood Foreclosures, Racial/Ethnic Transitions, and Residential Segregation.” American Sociological Review 80:526–549.
}
\author{
tim thomas
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
% ## Formula for ntdf
% {
%   }
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
