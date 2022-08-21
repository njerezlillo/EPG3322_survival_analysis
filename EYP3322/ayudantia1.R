# Paquetes ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(survival)

# Lectura -----------------------------------------------------------------

datos <- readxl::read_xlsx("smoke.xlsx")
#datos <- rio::import("smoke.xlsx")

# Descriptivo -------------------------------------------------------------

datos %>% head

datos$group %>% table

datos$status %>% table

datos %>% group_by(group) %>% 
  summarise(days_prom = mean(days), days_sd = sd(days))

datos %>% group_by(group) %>% filter(status == "dead") %>% 
  summarise(days_prom = mean(days), days_sd = sd(days)) %>% 
  ggplot(., aes(x = group, y = days_prom, color = group)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) + 
  geom_errorbar(aes(ymin = days_prom - days_sd, ymax = days_prom + days_sd, color = group), width = 0.1) +
  coord_flip() + theme_bw() + labs(y = "days", x = "group") +
  scale_color_discrete(NULL) + theme(legend.position="none")

# Surv --------------------------------------------------------------------

datos2 <- datos %>% 
  mutate(status = case_when(
    status == "alive" ~ 0,
    status == "dead" ~ 1
  ))

Surv(datos2$days, datos2$status)

# Tiempos de vida ---------------------------------------------------------

datos %>% mutate(id = 1:n()) %>% 
  ggplot() +
  geom_errorbar(aes(y = reorder(id, desc(id)), xmin = 0, xmax = days, color = group), width = 0) +
  geom_point(aes(x = days, y = reorder(id, desc(id)), shape = status), size = 3) +
  labs(x = "days", y = "id") + scale_shape_manual(values = c(1, 4)) +
  theme_bw()

datos %>% mutate(id = 1:n()) %>% 
  ggplot() + facet_wrap(~ group, scales = "free_y", ncol = 1) +
  geom_errorbar(aes(y = reorder(id, desc(id)), xmin = 0, xmax = days, color = group), width = 0) +
  geom_point(aes(x = days, y = reorder(id, desc(id)), shape = status), size = 2) +
  labs(x = "days", y = "id") + scale_shape_manual(values = c(1, 4)) +
  theme_bw() + theme(legend.position = "none", axis.text.y = element_blank())
