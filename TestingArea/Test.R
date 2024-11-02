# Load ggplot2 library
library(ggplot2)

# Print a basic message
print("Hello, R is working!")

# Perform a basic calculation
result <- 2 + 2
print(paste("2 + 2 equals:", result))

# Create some sample data
data <- data.frame(
  x = 1:10,
  y = c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
)

# Plot the data using ggplot2
ggplot(data, aes(x = x, y = y)) +
  geom_line() +
  geom_point() +
  ggtitle("Simple Plot of Prime Numbers")

# Print a completion message
print("The script has run successfully!")