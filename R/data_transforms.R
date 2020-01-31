prepare_data <- function(df) {
  transformed_data <- df %>% mutate(
    treatment = factor(
      as.character(`T`),
      levels = c("1", "2", "3", "4"),
      labels = c(
        "Ex Ante Personal",
        "Ex Ante Impersonal",
        "Ex Post Personal",
        "Ex Post Impersonal"
      )
    ),
    left = (polpref < 3),
    nothingtw = 1*(paymentlowworker %in% c(2, 8))
  )

  transformed_data
}
