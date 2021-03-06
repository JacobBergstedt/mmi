% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\docType{class}
\name{mmi_lm-class}
\alias{mmi_lm-class}
\alias{.make_lm}
\title{S4 class for representing a linear model specification and a fit to that specification.}
\description{
Inherits from \code{\linkS4class{spec_lm}} and \code{\linkS4class{mmi_model}}.
An \code{\link[stats]{lm}} fit and sandwich estimated standard errors are added.
}
\section{Slots}{

\describe{
\item{\code{fit}}{The fitted \code{\link[stats]{lm}} object.}

\item{\code{se}}{The standard error of the treatment (levels) estimated using
\code{\link[sandwich]{vcovHC}}}
}}

