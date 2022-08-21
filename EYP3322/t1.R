library(dplyr)
library(ggplot2)

aux <- read.delim("https://raw.githubusercontent.com/njerezlillo/EPG3322_survival_analysis/main/datos/egyptlives.txt")
aux <- aux[-(1:4),]

datos <- 
  data.frame(time = as.numeric(aux), group = c(rep("man", 82), rep("woman", 59)))
datos

ggplot(datos) + stat_ecdf(aes(x = time, color = group, y = 1 - ..y..), geom = "step") + 
  theme_bw() + labs(y = "survival function")

ggsave("t1_p3_b.pdf")

datos %>% mutate(id = 1:n()) %>% 
  ggplot() + facet_wrap(~ group, scales = "free_y", ncol = 1) +
  geom_errorbar(aes(y = reorder(id, desc(id)),
                    xmin = 0, xmax = time, color = group), width = 0) +
  geom_point(aes(x = time, y = reorder(id, desc(id))), size = 4, shape = 4) +
  labs(x = "time", y = "id") + scale_shape_manual(values = c(1, 4)) +
  theme_bw() + theme(legend.position = "none", axis.text.y = element_blank())

ggsave("t1_p3_c.pdf")

entry <- as.Date(c("2020-06-01", "2020-06-01", "2020-07-01", "2020-09-01", "2020-08-01", "2020-08-01"))
out <- as.Date(c("2020-07-01", "2020-08-01", "2020-09-01", "2020-12-01", "2020-10-01", "2021-01-01"))
p4 <- data.frame(id = 1:6, entry = entry, out = out, status = as.character(c(1, 1, 0, 0, 0, 0)))

ggplot(p4) + geom_errorbar(aes(y = reorder(id, desc(id)),
                    xmin = entry, xmax = out), width = 0) +
  geom_point(aes(x = out, y = reorder(id, desc(id)), shape = status), size = 4) +
  theme_bw() + scale_shape_manual(values = c(1, 4)) + labs(x = "date", y = "id") +
  theme(legend.position = "none") + scale_x_date(date_breaks = "1 month",date_labels = "%b - %y")
  
ggsave("t1_p4_b.pdf")
