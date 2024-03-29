% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fonctions.R
\name{mrmt_wmean}
\alias{mrmt_wmean}
\title{Moyennes pondérées sur un tableau nesté}
\usage{
mrmt_wmean(nestdf, mesure, weights)
}
\arguments{
\item{nestdf}{Un base de donnée traitée avec \code{\link{mrmt_nest}}}

\item{mesure}{La variable d'indérêt}

\item{weights}{La colonne des poids}
}
\value{
Un tibble avec la moyenne pondérée, l'intervalle de confiance et le nombre d'individu de chaque sous-tableau
}
\description{
Cette fonction retourne la moyenne pondérée et l'intervalle de confiance d'une variable d'intérêt, ainsi que le nombre d'invidivu pour chaque sous-tableau
}
\examples{
mrmt_wmean(ZPHH_nest,rdist_sum_Inland,WP) # distance journalière moyenne (total)
}
