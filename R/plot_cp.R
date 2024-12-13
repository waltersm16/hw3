

#reading in data
est <- read_csv("est.csv")
dat <- read_csv("contraceptive_use.csv") %>%
  rename(iso = division_numeric_code, cp = contraceptive_use_modern) %>%
  mutate(year = (start_date+end_date)/2, cp_perc = cp*100)

#plot_cp function
#' Plot mCPR data and estimates
#'
#' @param dat tibble which contains mCPR observations. Columns: iso, year, cp
#' @param est tibble which contains mCPR estimates. Columns: Country or area, iso, Year, Median, U95, L95, U80, L80
#' @param iso_code country iso code
#' @param CI confidence intervals to be plotted. Options can be: 80, 95, or NA (no CI plotted)
#'
#' @return ggplot object with mCPR observations and estimates over time
#' @export
#'
#' @examples
plot_cp <- function(dat, est, iso_code, CI = 95) {

  if (!("iso" %in% colnames(dat)) | !("iso" %in% colnames(est))) {
    stop("Input data file dat and estimates file est must contain variable iso.")
  }
  if (!("year" %in% colnames(dat)) | !("cp" %in% colnames(dat))) {
    stop("Input data file dat must contain variables year and cp.")
  }
  if (!(is.numeric(dat$cp))) {
    stop("Input cp in data file dat must be numeric.")
  }
  if (!(CI == 80 | CI == 95 | is.na(CI))) {
    stop("CI must be 80, 95, or NA.")
  }

  est2 <- est %>%
    filter(iso == iso_code)

  dat2 <- dat %>%
    filter(iso == iso_code, is_in_union == "Y")

  if (is.na(CI))
  {ggplot() +
      geom_line(data = est2, aes(x = Year, y = Median)) +
      geom_point(data = dat2, aes(x = year, y = cp_perc)) +
      labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1])}
  else if (CI == 95)
  {ggplot() +
      geom_line(data = est2, aes(x = Year, y = Median)) +
      geom_smooth(data = est2,aes(x = Year, y = Median, ymax = U95, ymin = L95), stat = "identity") +
      geom_point(data = dat2, aes(x = year, y = cp_perc)) +
      labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1])
  } else
  {ggplot() +
      geom_line(data = est2, aes(x = Year, y = Median)) +
      geom_smooth(data = est2,aes(x = Year, y = Median, ymax = U80, ymin = L80), stat = "identity") +
      geom_point(data = dat2, aes(x = year, y = cp_perc)) +
      labs(x = "Time", y = "Modern use (%)", title = est2$`Country or area`[1])
  }
}
