print("Hello World!")

all_package <- installed.packages(.Library, priority = "high")
print(all_package [, c(1, 3:5)])

x <- seq.int(0, 1, by = 0.01)

hist(rnorm(100))
plot(x, x^2)
