library("rvest")
library("purrr")
library("ggplot2")
library("ggrepel")

# Function for getting info for each task
get_info <- function(task){
  print(task)
  page <- read_html(paste0("https://cran.r-project.org/web/views/", task, ".html"))
  page <- page %>%
    html_node("table") %>%
    html_table(header = FALSE)
  
  contact <- page$X2[1]
  version <- lubridate::ymd(page$X2[3])
  
  data.frame(contact = contact,
             version = version)
  
}


# Get all tasks
tasks <- read_html("https://cran.r-project.org/web/views/")
tasks <- tasks %>%
  html_node("table") %>%
  html_table(header = FALSE)
tasks <- tibble::as_tibble(tasks)


# Adds info
tasks <- tasks %>% by_row(function(x){
  get_info(x$X1)
})

tasks <- tidyr::unnest(tasks, .out)

# Plot
tasks %>%
  dplyr::mutate(y = 1:nrow(tasks)) %>%
ggplot() +
  geom_label(aes(x = version, y = y , label = X1)) +
  ylab("") +
  xlim(c(min(tasks$version, na.rm = TRUE), 
         max(tasks$version, na.rm = TRUE) + 10))+ theme(
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank()) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  xlab("Latest update") +
  ggtitle("CRAN Task Views") +
  theme(text = element_text(size=20))

ggsave("crantaskviews.png", width = 14, height = 7)