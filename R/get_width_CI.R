#reading in data
est <- read_csv("est.csv")

#Get_width_CI function
#' Calculates width of uncertainty intervals
#'
#' @param est tibble which contains mCPR estimates and uncertainty intervals. Columns: Country or area, iso, Year, Median, U95, L95, U80, L80
#' @param iso_code country iso code
#' @param coverage confidence intervals. Options are: 80 or 95
#'
#' @return tibble with year and corresponding width of uncertainty intervals
#' @export
#'
#' @examples get_width_ci(est, iso_code = 4, coverage = 95)

get_width_ci <- function(est, iso_code, coverage) {
  est2 <- est %>%
    filter(iso == iso_code)

  if(coverage == 95)
  {est2 %>%
      mutate(width = U95-L95) %>%
      select(Year, width)}
  else
  {est2 %>%
      mutate(width = U80-L80) %>%
      select(Year, width)}
}
