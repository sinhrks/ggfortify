# Avoid false positives in R CMD CHECK:
utils::globalVariables(c("ACF", "plot_group", "Breaks", "variance", "plot_group",
	"Data", "Point_Forecast", ".resid", ".wresid", ".cooksd", "fcst", "variable",
	"value", "se", "Spline", "x", "y", "Frequency"))
