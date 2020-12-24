\name{afford}
\alias{afford}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Available affordable housing}
\description{
Formula to determine the rate of affordable housing for different AMI groups
}
\usage{
afford(
   state = "state",
   counties = "counties",
   ami_limit = "ami_limit",
   year = 2018,
   GEOID = "GEOID",
   acs2018 = "acs2018",
   variable = "variable",
   estimate = "estimate",
   limit = "limit",
   tr_own_accessible = "tr_own_accessible",
   tr_own_total = "tr_own_total",
   tr_own_supply = "tr_own_supply",
   tr_rent_accessible = "tr_rent_accessible",
   tr_rent_total = "tr_rent_total",
   tr_rent_supply = "tr_rent_supply"
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{state}{ = "state",}
   \item{counties}{ = "counties",}
   \item{ami_limit}{ = "ami_limit",}
   \item{year}{ = 2018  }
   \item{GEOID}{ = "GEOID",}
   \item{acs2018}{ = "acs2018",}
   \item{variable}{ = "variable",}
   \item{estimate}{ = "estimate",}
   \item{limit}{ = "limit",}
   \item{tr_own_accessible}{ = "tr_own_accessible",}
   \item{tr_own_total}{ = "tr_own_total",}
   \item{tr_own_supply}{ = "tr_own_supply",}
   \item{tr_rent_accessible}{ = "tr_rent_accessible",}
   \item{tr_rent_total}{ = "tr_rent_total",}
   \item{tr_rent_supply}{ = "tr_rent_supply",}
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
none
}
\author{
Alex Ramiller and tim thomas
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
