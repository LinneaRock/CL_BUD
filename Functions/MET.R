
bar <- function(x, y, axistitle) {
  ggplot(data, aes(x, y)) +
    geom_bar(stat = "identity") +
    labs(x = "", 
         y = axistitle) +
    L_theme()
}

linepl <- function(x, y, axistitle) {
  ggplot(data, aes(x, y)) +
    geom_line() +
    labs(x = "", 
         y = axistitle) +
    L_theme()
}

