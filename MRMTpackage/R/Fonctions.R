#' Recode selon les catégories de l'OFS
#'
#' Cette fonction va créer, à partir d'une base de donnée, de nouvelles colonnes correspondant aux catégories classiques MRMT (les lignes du tableau A2).
#' @param df Une base de donnée
#' @param sexe La colonne 'sexe' de la base de donnée choisie (nom en output : Sexe)
#' @param age  La colonne 'âge' de la base de donnée choisie (nom en output : Âge)
#' @param act_prof La colonne 'activité professionnelle' de la base de donnée choisie (nom en output : Activité_professionnelle)
#' @param revenu La colonne 'revenu' de la base de donnée choisie (nom en output : Revenu_mensuel_du_ménage)
#' @param deg_urb La colonne 'degré d'urbanisation' de la base de donnée choisie (nom en output : Degré_d_urbanisation_du_domicile)
#' @return La base de donnée d'entrée plus les nouvelles colonnes recodées selon la nomenclature OFS
#' @examples
#' mrmt_recode(ZPHH_raw,gesl,alter,ERWERB,F20601,W_staedt_char_2012)
#' @export

mrmt_recode <- function(df,sexe,age,act_prof,revenu,deg_urb) {

sexe <- dplyr::enquo(sexe)
age <- dplyr::enquo(age)
act_prof <- dplyr::enquo(act_prof)
revenu <- dplyr::enquo(revenu)
deg_urb <- dplyr::enquo(deg_urb)

df$Âge <- dplyr::select(df,!! age) %>% unlist() %>% cut(., breaks=c(6, 18, 25, 45, 65, 80, 110 ), right = FALSE,  # On doit utiliser unlist() parce que cut()
               labels = c("6-17 ans","18-24 ans","25-44 ans","45-64 ans","65-79 ans", "80 ans et plus"))  # ne marche que sur des vecteurs

Grp <- df %>% dplyr::select(!! sexe,!! act_prof,!! revenu,!! deg_urb) %>% purrr::map_df(as.factor)  # Pour plus de sécrutité, j'ai transformé le reste des variables
names(Grp) <- c("sexe","act_prof","revenu","deg_urb")                                               # en facteur, mais je ne sais pas si c'est obligatoire

df$Sexe <- dplyr::recode(Grp$sexe, "1" = "Hommes" , "2" = "Femmes")

df$Activité_professionnelle <- dplyr::recode(Grp$act_prof,
                                      "1" = "Employé à plein temps",
                                      "2" = "Employé à temps partiel",
                                      "3" = "Personne en formation",
                                      "4" = "Sans activité professionnelle",
                                      "-97" = "A", "-98" = "A", "9" = "A") %>%
  dplyr::recode("A" = "Autres (taux d'activité inconnu / ne sait pas / pas de réponse)")

df$Revenu_mensuel_du_ménage <- dplyr::recode(Grp$revenu,
                                      "-99" = "A", "-98" = "A", "-97" = "A",
                                      "1" = "B","2" = "B",
                                      "3" = "C", "4" = "C",
                                      "5" = "D", "6" = "D",
                                      "7" = "E", "8" = "E", "9" = "E") %>%
  dplyr::recode("A" = "Ne sait pas/pas de réponse", "B" = "Jusqu'à 4 000 CHF" ,
         "C" = "4 001 - 8 000 CHF", "D" = "8 001 - 12 000 CHF", "E" = "Plus de 12 000 CHF")

df$Degré_d_urbanisation_du_domicile <- dplyr::recode(Grp$deg_urb,
                                              "1" = "A", "2" = "A", "3" = "A", "6" = "A",
                                              "4" = "B", "5" = "B",
                                              "0" = "Espace hors influence des centres urbains") %>%
  dplyr::recode("A" = "Espace des centres urbains","B" = "Espace sous influence des centres urbains")

df

}

#' Divisie un tableau selon les catégories
#'
#' Cette fonction va diviser la base de donnée d'entrée en différents sous-tableaux selon les catégories MRMT classiques
#' @param df Une base de donnée traitée avec \code{\link{mrmt_recode}}
#' @return Un tibble avec les différentes modalités et sous-tableaux
#' @examples
#' mrmt_nest(ZPHH)
#' @export


mrmt_nest <- function(df) {

  sexe <- df %>% dplyr::group_nest(Sexe) %>% dplyr::rename(Modalité = Sexe) %>% dplyr::mutate(Variable = "Sexe")
  age <- df %>% dplyr::group_nest(Âge) %>% dplyr::rename(Modalité = Âge) %>% dplyr::mutate(Variable = "Âge")
  actprof <- df %>% dplyr::group_nest(Activité_professionnelle) %>% dplyr::rename(Modalité = Activité_professionnelle) %>%
    dplyr::mutate(Variable = "Activité professionnelle")
  revmens <- df %>% dplyr::group_nest(Revenu_mensuel_du_ménage) %>% dplyr::rename(Modalité = Revenu_mensuel_du_ménage) %>%
    dplyr::mutate(Variable = "Revenu mensuel du ménage")
  urb <- df %>% dplyr::group_nest(Degré_d_urbanisation_du_domicile) %>% dplyr::rename(Modalité = Degré_d_urbanisation_du_domicile) %>%
    dplyr::mutate(Variable = "Degré d'urbanisation du domicile")

  new_df <- dplyr::bind_rows(sexe,age,actprof,revmens,urb) %>% dplyr::select(Variable,Modalité,data)

  new_df

}


#' Moyennes pondérées sur un tableau nesté
#'
#' Cette fonction retourne la moyenne pondérée et l'intervalle de confiance d'une variable d'intérêt, ainsi que le nombre d'invidivu pour chaque sous-tableau
#' @param nestdf Un base de donnée traitée avec \code{\link{mrmt_nest}}
#' @param mesure La variable d'indérêt
#' @param weights La colonne des poids
#' @return Un tibble avec la moyenne pondérée, l'intervalle de confiance et le nombre d'individu de chaque sous-tableau
#' @examples
#' mrmt_wmean(ZPHH_nest,rdist_sum_Inland,WP) # distance journalière moyenne (total)
#' @export

mrmt_wmean <- function(nestdf,mesure,weights) {

  mesr <- dplyr::enquo(mesure)
  w <- dplyr::enquo(weights)

  N <- nrow(nestdf)
  nestdf2 <- nestdf %>% dplyr::mutate(estimation_ponctuelle = 0, plus_moins_IC = 0, N = 0)

  for(i in 1:N) {

    nestdf2$estimation_ponctuelle[i] =  nestdf$data[[i]] %>% dplyr::summarise(Hmisc::wtd.mean(!! mesr,!! w))
    nestdf2$plus_moins_IC[i] =  nestdf$data[[i]] %>% dplyr::summarise(1.645 * 1.14 * sqrt(Hmisc::wtd.var(!! mesr,!! w)) / sqrt(n()))
    nestdf2$N[i] =  nestdf$data[[i]] %>% dplyr::summarise(n())

  }

  nestdf2 %>% dplyr::select(-data) %>% tidyr::unnest(estimation_ponctuelle,plus_moins_IC,N)

}

#' Proportions pondérées sur un tableau nesté
#'
#' Cette fonction retourne la proportion pondérée et l'intervalle de confiance de la modalité d'une variable d'intérêt, ainsi que le nombre d'invidivu pour chaque sous-tableau
#' @param nestdf Un base de donnée traitée avec \code{\link{mrmt_nest}}
#' @param category La variable d'indérêt
#' @param value La modalité de la variable d'intérêt
#' @param weights La colonne des poids
#' @return Un tibble avec la proportion pondérée, l'intervalle de confiance et le nombre d'individu de chaque sous-tableau
#' @examples
#' mrmt_wprop(ZPHH_nest,f50200,1,WP) # personnes mobiles le jour de référence, 1 = oui
#' @export

mrmt_wprop <- function(nestdf,category,value,weights) {

  cat <- dplyr::enquo(category)
  w <- dplyr::enquo(weights)

  N <- nrow(nestdf)
  nestdf2 <- nestdf %>% dplyr::mutate(estimation_ponctuelle = 0, plus_moins_IC = 0, N = 0)

  for(i in 1:N) {

    Nb <- nestdf$data[[i]] %>% nrow()
    tot_w <- nestdf$data[[i]] %>% dplyr::select(!! w) %>% sum()

    nestdf2$estimation_ponctuelle[i] =  nestdf$data[[i]] %>% dplyr::filter(!! cat == value) %>% dplyr::summarise((sum(!! w)/tot_w)*100)
    nestdf2$plus_moins_IC[i] =  nestdf$data[[i]] %>% dplyr::filter(!! cat == value) %>%
      dplyr::summarise((1.14*1.645 * sqrt((sum(!! w)/tot_w)*(1-(sum(!! w)/tot_w))/Nb))*100) # Pour info, 1.14 * 1.645  = 1.8753
    nestdf2$N[i] =  nestdf$data[[i]] %>% dplyr::summarise(n())

  }

  nestdf2 %>% dplyr::select(-data) %>% tidyr::unnest(estimation_ponctuelle,plus_moins_IC,N)

}

#' Moyennes pondérées pour une modalité spécifique sur un tableau nesté
#'
#' Cette fonction retourne la moyenne pondérée et l'intervalle de confiance d'une variable d'intérêt pour la modalité d'une catégorie voulue, ainsi que le nombre d'invidivu pour chaque sous-tableau
#' @param nestdf Un base de donnée traitée avec \code{\link{mrmt_nest}}
#' @param mesure La variable d'intérêt
#' @param category La catégorie voulue
#' @param value La modalité de la catégorie voulue
#' @param weights La colonne des poids
#' @return Un tibble avec la moyenne pondérée, l'intervalle de confiance et le nombre d'individu de chaque sous-tableau
#' @examples
#' mrmt_wmean_by(DéplacementZPHH_nest,déplacements,Motif,"Travail",WP) # moyenne pondérée du nombre de déplacement pour le motif travail
#' @export

mrmt_wmean_by <- function(nestdf,mesure,category,value,weights) {

  mesr <- dplyr::enquo(mesure)
  cat <- dplyr::enquo(category)
  w <- dplyr::enquo(weights)

  N <- nrow(nestdf)
  nestdf2 <- nestdf %>% dplyr::mutate(estimation_ponctuelle = 0, plus_moins_IC = 0, N = 0)

  for(i in 1:N) {

    nestdf2$estimation_ponctuelle[i] =  nestdf$data[[i]] %>% dplyr::mutate(dummy = ifelse(!! cat == value,!! mesr,0)) %>%
      dplyr::group_by(HHNR,!! w) %>% dplyr::summarise(by_individuals = sum(dummy)) %>% dplyr::ungroup() %>%
      dplyr::summarise(Hmisc::wtd.mean(by_individuals,!! w))

    nestdf2$plus_moins_IC[i] =  nestdf$data[[i]] %>% dplyr::mutate(dummy = ifelse(!! cat == value,!! mesr,0)) %>%
      dplyr::group_by(HHNR,!! w) %>% dplyr::summarise(by_individuals = sum(dummy)) %>% dplyr::ungroup() %>%
      dplyr::summarise(1.645 * 1.14 * sqrt(Hmisc::wtd.var(by_individuals,!! w))/sqrt(n()))

    nestdf2$N[i] =  nestdf$data[[i]] %>% dplyr::distinct(HHNR) %>% dplyr::summarise(n())

  }

  nestdf2 %>% select(-data) %>% tidyr::unnest(estimation_ponctuelle,plus_moins_IC,N)

}
