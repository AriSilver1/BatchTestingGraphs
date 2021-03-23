library(plotly)

createData = function(filename) {
	data = read.csv(filename, header = FALSE)

	print(head(data))

	x = unique(data$V1)
	y = unique(data$V2)

	z = matrix(NaN, nrow=length(x), ncol=length(y))

	print(length(data))

	for(i in 1:length(data$V3)) {
		z[(data$V1)[i],(data$V2)[i]] = 1 - ((data$V3)[i] / 5000);
	}

	return(list("x"=x,"y"=y,"z"=z))
}

createGraph = function(filename) {
	data = createData(filename)

	fig <- plot_ly(z = data$z, coloraxis = 'coloraxis')
	fig <- fig %>% add_surface()
	fig <- fig %>% layout(coloraxis=list(colorscale='Jet'))


	fig <- fig %>% layout(
    		title = "Multi Two-Level Method Surface Plot at 100% Infection Rate",
    		scene = list(
      			xaxis = list(title = "n"),
      			yaxis = list(title = "r"),
      			zaxis = list(title = "Fraction of Tests Saved")
		)
	)


	fig
}

src = function() {
	source("graph.r")
}