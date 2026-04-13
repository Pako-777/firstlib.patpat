#' Filtrer les trajets par numéro de boucle
#'
#' @param trajet Data frame contenant les données de trajets
#' @param boucle Vecteur de numéros de boucle à sélectionner
#'
#' @returns Data frame filtré avec les boucles sélectionnées
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' # filtrer_trajet(df_velo, c("880", "881"))
filtrer_trajet <- function(df_velo, boucle){
  boucle_num <- as.numeric(boucle)
  df_velo |>
    filter(`Numéro de boucle` %in% (boucle_num))
}
