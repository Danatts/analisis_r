# Visualización de la información

# Importamos la base de datos
df <- iris
names(iris)
str(iris)

# Gráficos básicos
# Gráfico de dispersión
plot(iris[,1:4])

# Histograma
hist(iris$Sepal.Length)

# Gráficos avanzados (ggplot)
library(ggplot2)

# Gráfico de dispersión
ggplot(data=iris, aes(x=Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Largo del sépalo (Sepal Length)") +
  ylab("Ancho del sépalo (Sepal Width)") +
  ggtitle("Sépalo (largo versus ancho)") +
  geom_point(aes(color = Petal.Width, shape = Species), size = 2, alpha = I(1/2)) +
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "red", linetype = "dashed") +
  geom_hline(aes(yintercept = mean(Sepal.Width)), color = "red", linetype = "dashed") +
  scale_color_gradient(low = "yellow", high = "red") +
  xlab("Largo del sépalo (Sepal Length)") +  ylab("Ancho del sépalo (Sepal Width)") +
  ggtitle("Sépalo (largo versus ancho)")

# Librería Esquisse
# Es una herrramienta gráfica que facilita la creación de gráficos entregando
# entregando el código de ggplot()
grp <- ggplot(df) +
 aes(x = Sepal.Length, y = Sepal.Width, colour = Species) +
 geom_point(size = 3.5, shape = "bullet") +
 scale_color_viridis_d(option = "plasma", direction = 1) +
 labs(x = "Sepal length", y = "Sepal Width", 
 title = "Sepal (Length vs Width)") +
 theme_minimal()

# "Data to Viz" (https://www.data-to-viz.com/)
# Es una herramienta que ayuda a identificar qué tipo de gráfico es el más
# adecuado para el tipo de dato

# Librería Ploty
# Es una herramienta para gráficos interactivos (para HTML)
library(plotly)
plot_ly(iris,x=~Sepal.Length,y=~Sepal.Width,type="scatter",mode = "markers+text",color=~Species)

# Gráfico tridimensional
fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length,
               color = ~Species, mode = "markers", type = "scatter3d")
fig <- fig %>% layout(autosize = TRUE, title = "Gráfico de dispersión tridimensional de iris",
                      scene = list(xaxis = list(title = "Longitud del sépalo"),
                                   yaxis = list(title = "Ancho del sépalo"),
                                   zaxis = list(title = "Longitud del pétalo")))
fig

# Convertir un gráfico de ggplot a plotly
ggplotly(grp)
