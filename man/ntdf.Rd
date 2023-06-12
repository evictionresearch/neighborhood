\name{ntdf}
\alias{ntdf}
\alias{nt}
\alias{ntcheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Download data and run nt}
\description{
Data creation using the nt formula
}
\usage{
ntdf(
  	state,
	geography = "tract",
	county = NULL,
	geometry = FALSE,
	cache_table = TRUE,
	output = "wide",
	year = NULL,
	cb = TRUE,
	GEOID = "GEOID",
	WhiteE = "WhiteE",
	AsianE = "AsianE",
	BlackE = "BlackE",
	LatinxE = "LatinxE",
	totraceE = "totraceE"
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  	\item{state}{State of choice. Can add multiple states with `c()` function}
	\item{geography}{ = Default is "tract", can choose any geography based on tidycensus `get_acs` parameters.}
	\item{county}{ = Select a state's specific county or group of counties. Default is `NULL` to download the entire state.}
	\item{geometry}{ = Download spatial files. Default = FALSE.}
	\item{cache_table}{ = Decide whether to cache the census data for repeated downloads. Default = TRUE.}
	\item{output}{ = Default is "wide". Can set to "long" }
	\item{year}{ = Census data year. Default is `NULL`}
	\item{cb}{ = Download the census' smaller cartographic boundary for visualization. Default is `TRUE`. Set to `FALSE` for more accurate boundaries appropriate for spatial analysis.}
	\item{GEOID}{ = "GEOID",}
	\item{WhiteE}{ = "WhiteE",}
	\item{AsianE}{ = "AsianE",}
	\item{BlackE}{ = "BlackE",}
	\item{LatinxE}{ = "LatinxE",}
	\item{totraceE}{ = "totraceE"}
}
% \details{
%%  ~~ If necessary, more details than the description above ~~
% }
% \value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
% }
% \references{
%% ~put references to the literature/web site here ~
% }
\author{tim thomas}
% \note{
%%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
% }
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.

% ## The function is currently defined as
% function (x)
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
