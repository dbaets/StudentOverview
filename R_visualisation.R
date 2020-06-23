source("R_functions.R", local = TRUE)


# Adjustment parameters
global.input_path <- "E:/Punten_Lln_Joke/ExcelOverzicht"
global.output_path <- "E:/Punten_Lln_Joke"
global.vak <- "FYS"


t_input <- f_load_xlsx(path = paste0(global.input_path,"/",global.vak,".xlsx"),sheet = "Blad1")

t_input$klas <- as_factor(t_input$klas)
vis <- ggplot(t_input, aes(x = klas, y = LPD_tot, label = leerling)) +
  geom_boxplot(position = position_dodge()) + 
  geom_jitter(colour = "grey50", alpha = 0.5) +
  geom_text(aes(label=leerling)) +
  ylim(0,1) + 
  theme_bw()

ggplotly(vis)


t_selected <- t_fys %>% 
  filter(klas == "4WEa")
ggplot(t_selected, aes(x = examen, y = klas)) + 
  geom_density_() +
  xlim(0,100)
