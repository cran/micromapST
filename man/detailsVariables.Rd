\name{detailsVariables}
\alias{detailsVariables}
\docType{data}
\title{Validation and Translation table for details variables}
\description{
   An internal table containing a list of the details variables and parameters to 
   do validation of user provided variable overrides of the defaults.  The table also contains the 
   information needed to translate the existing (as of 9/17/2015) details structure into a
   new details structure segmented by each type of glyphic.
   }
   
\usage{data(detailsVariables)}

\format{
  A data frame with 6 variables:
  \describe{
    \item{varName}{a character string of the exact details variable
    name}
    \item{method}{a character string describing the type of test
       required to validate the variable. The supported tests are:
       colors, integer, numeric, logical, and vector3}
    \item{v1}{first variable for the "test" for this variable.}
    \item{v2}{second variable (if needed) for the "test" for this
      variable.} 
    \item{usedBy}{a vector of character string listing
      the glyphics that use this variable. \cr For Example:
      c('ts','tsconf')}
    \item{newVarName}{the new variable name to be used within a
      glyphic.  The glyphic name and variable name must be unique.
      This eliminates having to include the glyphic name in the
      variable name.} 
    \item{d_range}{defines the range of the
      dependency for the dependent relationship.}
    \item{dependent}{indicates this variable is dependent on another
      variable and it's name.} 
    \item{default}{the default values or
      the variable.}
    \item{comments}{operational comments on the variable.}
  }
}
\details{
   This dataset provide a table to \var{micromapST} for verification
   of the details variables provided by a user to override the
   packages default values.  The \var{varName} is the exact name of
   the variable.  If the variable name provide by the user does not
   match this list, it is flagged as an error and ignored.
   The test supported are:  colors, integer, numeric, logical and
   vector3. The colors test calls the \code{is.Color} function.  The
   integer and numeric tests use the range provided in \var{v1} and
   \var{v2} columns to check the range of the value for the
   variable.  The logical test verifies the value is TRUE or FALSE.
   The vector3 test check to make sure the value is a vector or
   length 3 and each value is within the range in \var{v1} to
   \var{v2}.

   The micromapST package is being updated to use a new variable
   structure. The new structure will allow options/parameters to be
   specified on glyphic and column basis.  This allows each column
   to be uniquely tuned to the user requirements.  The variable
   names have also been simplifed to use the same name across
   glyphics when the purpose is the same. This should reduce the
   users learning curve.
}
\author{
Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\keyword{datasets}
