library(readxl)
demo <- read_excel("C:/Users/Usuario/Dropbox/Libro An�lisis de datos/Data/demo.xlsx")

ggplot(demo, aes(x = Variables, y = Medias, colour = Grupo)) + 
  geom_line() 
  