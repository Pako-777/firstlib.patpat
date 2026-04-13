#' Filtre les trajets sans anomalies
#'
#' @param trajet Data frame contenant les données de trajets vélo
#'
#' @returns Data frame filtré sans les lignes avec anomalies
#' @export
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' data_propre <- filtre_anomalie(data_velo)
#' }
filtre_anomalie <- function(trajet){
  trajet |>
    filter(is.na(`Probabilité de présence d'anomalies`))
}



#' Compter le nombre total de trajets
#'
#' @param trajet Data frame contenant les données de trajets avec une colonne Total
#'
#' @returns Un nombre (integer) représentant la somme totale des trajets
#' @importFrom dplyr pull
#' @export
#'
#' @examples
#' # compter_nombre_trajets(mes_donnees)
compter_nombre_trajets <- function(trajet){
  trajet |>
    pull(Total) |>
    sum()
}





#' Compter le nombre de boucles distinctes
#'
#' @param trajet Data frame contenant les données de trajets avec une colonne "Numéro de boucle"
#'
#' @returns Un nombre (integer) représentant le nombre de boucles distinctes
#' @importFrom dplyr pull n_distinct
#' @export
#'
#' @examples
#' # compter_nombre_boucle(mes_donnees)
compter_nombre_boucle <- function(trajet){
  trajet |>
    pull(`Numéro de boucle`) |>
    n_distinct()
}





#' Trouver le trajet avec le maximum de passages
#'
#' @param trajet Data frame contenant les données de trajets avec une colonne Total
#'
#' @returns Un data frame avec les colonnes "Boucle de comptage", "Jour" et "Total" pour le trajet maximum
#' @importFrom dplyr slice_max select
#' @export
#'
#' @examples
#' # trouver_trajet_max(mes_donnees)
trouver_trajet_max <- function(trajet){
  trajet |>
    slice_max(Total) |>
    select(`Boucle de comptage`, Jour, Total)
}





#' Calculer la distribution des trajets par jour de la semaine
#'
#' @param trajet Data frame contenant les données de trajets avec colonnes "Jour de la semaine" et "Total"
#'
#' @returns Un data frame avec deux colonnes : "Jour de la semaine" et "trajets" (somme des trajets par jour)
#' @importFrom dplyr count
#' @export
#'
#' @examples
#' # calcul_distribution_semaine(mes_donnees)
calcul_distribution_semaine <- function(trajet){
  trajet |>
    count(`Jour de la semaine`, wt = Total, sort = TRUE, name = "trajets")
}





#' Graphique de la distribution hebdomadaire des trajets
#'
#' @param trajet Data frame contenant les données de trajets
#'
#' @returns Un objet ggplot représentant la distribution des trajets par jour de la semaine
#' @importFrom dplyr mutate
#' @importFrom forcats fct_recode
#' @importFrom ggplot2 ggplot aes geom_col
#' @export
#'
#' @examples
#' # plot_distribution_semaine(mes_donnees)
plot_distribution_semaine <- function(trajet) {
  trajet_weekday <- trajet |>
    filtre_anomalie() |>
    calcul_distribution_semaine() |>
    mutate(
      jour = fct_recode(
        factor(`Jour de la semaine`),
        "lundi" = "1",
        "mardi" = "2",
        "mercredi" = "3",
        "jeudi" = "4",
        "vendredi" = "5",
        "samedi" = "6",
        "dimanche" = "7"
      )
    )
  ggplot(trajet_weekday) +
    aes(x = jour, y = trajets) +
    geom_col()
}








