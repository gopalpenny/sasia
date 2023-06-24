library(tidyverse)

x = seq(0, 2, length.out = 100)

df = tibble(x = x,
    y1 = sin(1.5*2*pi*x), 
    y2 = cos(1.5*2*pi*x),
    y3 = sin(3*2*pi*x),
    y4 = cos(3*2*pi*x))

a1 = -1
a2 = -1
a3 = -2
a4 = 1

ggplot(df, aes(x = x)) +
  geom_line(aes(y = y1, color = "sin")) +
  geom_line(aes(y = y2, color = "cos")) +
    geom_line(aes(y = y3, color = "sin3")) +
    geom_line(aes(y = y4, color = "cos3")) +
    geom_line(aes(y = a1*y1 + a2*y2 + a3*y3 + a4*y4, color = "sum"))
