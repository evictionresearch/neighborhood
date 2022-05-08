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
   year = NULL,
   GEOID = "GEOID",
   acsvars = "acsvars",
   variable = "variable",
   estimate = "estimate",
   limit = "limit",
   tr_own_accessible = "tr_own_accessible",
   tr_own_total = "tr_own_total",
   tr_own_supply = "tr_own_supply",
   tr_rent_accessible = "tr_rent_accessible",
   tr_rent_total = "tr_rent_total",
   tr_rent_supply = "tr_rent_supply",
   tr_rent_rate = "tr_rent_rate",
   tr_own_rate = "tr_own_rate",
   reg_total_pop = "reg_total_pop",
   reg_class_pop = "reg_class_pop"
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{state}{ = "state",}
   \item{counties}{ = "counties",}
   \item{ami_limit}{ = "ami_limit",}
   \item{year}{ = NULL,}
   \item{GEOID}{ = "GEOID",}
   \item{acsvars}{ = "acs2018",}
   \item{variable}{ = "variable",}
   \item{estimate}{ = "estimate",}
   \item{limit}{ = "limit",}
   \item{tr_own_accessible}{ = "tr_own_accessible",}
   \item{tr_own_total}{ = "tr_own_total",}
   \item{tr_own_supply}{ = "tr_own_supply",}
   \item{tr_rent_accessible}{ = "tr_rent_accessible",}
   \item{tr_rent_total}{ = "tr_rent_total",}
   \item{tr_rent_supply}{ = "tr_rent_supply",}
   \item{tr_rent_rate}{ = "tr_rent_rate",}
   \item{tr_own_rate}{ = "tr_own_rate",}
   \item{reg_total_pop}{ = "reg_total_pop",}
   \item{reg_class_pop}{ = "reg_class_pop"}
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
\concept{ ~housing }
\concept{ ~affordability }
% Use only one concept per line.
