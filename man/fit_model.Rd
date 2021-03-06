% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specifications.R
\name{fit_model}
\alias{fit_model}
\alias{fit_model.spec_lm}
\alias{fit_model.spec_lmm}
\alias{fit_model.spec_logreg}
\alias{fit_model.spec_nb}
\alias{fit_model.spec_beta}
\title{S3 Generic function fit_model to construct mmi_model objects.}
\usage{
fit_model(object, study_frame)

\method{fit_model}{spec_lm}(object, study_frame)

\method{fit_model}{spec_lmm}(object, study_frame)

\method{fit_model}{spec_logreg}(object, study_frame)

\method{fit_model}{spec_nb}(object, study_frame)

\method{fit_model}{spec_beta}(object, study_frame)
}
\arguments{
\item{object}{\code{\linkS4class{spec}} object.}

\item{study_frame}{The data frame that includes all variables needed for the analysis}
}
\description{
Sets up the formula for the particular model using \code{\link{get_formula}} and then
fits an an appropriate model using the data in \code{study_frame}. An mmi_model object is
created by adding the model fit, and possibly additional needed information,
to the specification object.
}
\section{Methods (by class)}{
\itemize{
\item \code{spec_lm}: Constructs a \code{\linkS4class{mmi_lm}} object.

Sets up the formula using \code{\link{get_formula}} and then fits an
\code{\link[stats]{lm}} using the formula and the data in study_frame.

\item \code{spec_lmm}: Constructs a \code{\linkS4class{mmi_lmm}} object by setting up a formula using \code{\link{get_formula}}
and then fitting a \code{\linkS4class{lmerMod}} using the formula and the data in study_frame.

\item \code{spec_logreg}: Constructs a \code{\linkS4class{mmi_logreg}} object by setting up a formula using \code{\link{get_formula}}
and then fitting a \code{\link[stats]{glm}} logistic regression using the formula and the data in study_frame.

\item \code{spec_nb}: Constructs a \code{\linkS4class{mmi_nb}} object by setting up a formula using \code{\link{get_formula}}
and then fitting a \code{\link[MASS]{glm.nb}} logistic regression using the formula and the data in study_frame.

\item \code{spec_beta}: Constructs a \code{\linkS4class{mmi_beta}} object by setting up a formula using \code{\link{get_formula}}
and then fitting a \code{\link{betareg}} beta regression model using the formula and the data in study_frame.
}}

