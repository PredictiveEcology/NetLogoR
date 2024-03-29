% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/worldNLR-classes-methods.R,
%   R/agentMatrix-Class-methods.R
\name{[}
\alias{[}
\alias{[,worldMatrix,numeric,numeric,ANY-method}
\alias{[,worldMatrix,missing,missing,ANY-method}
\alias{[<-}
\alias{[<-,worldMatrix,numeric,numeric,ANY-method}
\alias{[<-,worldMatrix,missing,missing,ANY-method}
\alias{[,worldArray,numeric,numeric,ANY-method}
\alias{[,worldArray,missing,missing,ANY-method}
\alias{[<-,worldArray,numeric,numeric,matrix-method}
\alias{[<-,worldArray,missing,missing,matrix-method}
\alias{[,agentMatrix,numeric,numeric,ANY-method}
\alias{[,agentMatrix,logical,missing,ANY-method}
\alias{[,agentMatrix,numeric,missing,ANY-method}
\alias{[,agentMatrix,missing,missing,missing-method}
\alias{[,agentMatrix,missing,character,ANY-method}
\alias{[,agentMatrix,numeric,character,ANY-method}
\alias{[,agentMatrix,missing,numeric,ANY-method}
\alias{[<-,agentMatrix,numeric,numeric,numeric-method}
\alias{[<-,agentMatrix,missing,numeric,numeric-method}
\alias{[<-,agentMatrix,numeric,missing,numeric-method}
\alias{[<-,agentMatrix,numeric,character,data.frame-method}
\alias{[<-,agentMatrix,numeric,numeric,character-method}
\alias{[<-,agentMatrix,missing,numeric,character-method}
\alias{[<-,agentMatrix,missing,character,character-method}
\alias{[<-,agentMatrix,numeric,character,character-method}
\alias{$,agentMatrix-method}
\title{Extract or Replace Parts of an Object}
\usage{
\S4method{[}{worldMatrix,numeric,numeric,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{worldMatrix,missing,missing,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{worldMatrix,numeric,numeric,ANY}(x, i, j) <- value

\S4method{[}{worldMatrix,missing,missing,ANY}(x, i, j) <- value

\S4method{[}{worldArray,numeric,numeric,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{worldArray,missing,missing,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{worldArray,numeric,numeric,matrix}(x, i, j) <- value

\S4method{[}{worldArray,missing,missing,matrix}(x, i, j) <- value

\S4method{[}{agentMatrix,numeric,numeric,ANY}(x, i, j, ..., drop = FALSE)

\S4method{[}{agentMatrix,logical,missing,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{agentMatrix,numeric,missing,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{agentMatrix,missing,missing,missing}(x, i, j, ..., drop = FALSE)

\S4method{[}{agentMatrix,missing,character,ANY}(x, i, j, ..., drop = TRUE)

\S4method{[}{agentMatrix,numeric,character,ANY}(x, i, j, ..., drop = FALSE)

\S4method{[}{agentMatrix,missing,numeric,ANY}(x, i, j, ..., drop = FALSE)

\S4method{[}{agentMatrix,numeric,numeric,numeric}(x, i, j) <- value

\S4method{[}{agentMatrix,missing,numeric,numeric}(x, i, j) <- value

\S4method{[}{agentMatrix,numeric,missing,numeric}(x, i, j) <- value

\S4method{[}{agentMatrix,numeric,character,data.frame}(x, i, j) <- value

\S4method{[}{agentMatrix,numeric,numeric,character}(x, i, j) <- value

\S4method{[}{agentMatrix,missing,numeric,character}(x, i, j) <- value

\S4method{[}{agentMatrix,missing,character,character}(x, i, j) <- value

\S4method{[}{agentMatrix,numeric,character,character}(x, i, j) <- value

\S4method{$}{agentMatrix}(x, name)
}
\arguments{
\item{x}{A \code{agentMatrix} object from which to extract element(s) or
in which to replace element(s).}

\item{i}{Indices specifying elements to extract or replace.}

\item{j}{see \code{i}.}

\item{...}{other named arguments}

\item{drop}{not implemented}

\item{value}{Any R object}

\item{name}{A literal character string or a \code{\link[=name]{name()}} (possibly backtick quoted).}
}
\value{
An \code{agentMatrix} when full row(s), full column(s) or element(s)
at specific row(s) and column(s) is/are extracted.
}
\description{
Operators acting on vectors, matrices, arrays and lists to extract or replace parts.
}
\note{
Extract methods for \code{agentMatrix} class will generally maintain the \code{agentMatrix} class.
This means that there will still be coordinates, character columns represented as numerics etc.
\code{$} is for extracting the raw columns and does not maintain the \code{agentMatrix} class.
\verb{[]} will extract all values, and result in a data.frame with the correct character and
numeric columns.
}
