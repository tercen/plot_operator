### Color utilities

## Tercen palettes

rgba_to_int <- Vectorize(function(rgba) {
  red = rgba[1]
  green = rgba[2]
  blue = rgba[3]
  alpha = rgba[4]
  RGBint = bitwShiftL(alpha, 24) + bitwShiftL(red, 16) + bitwShiftL(green, 8) + blue
  RGBint
})

int_to_rgba <- Vectorize(function(int) {
  red = bitwAnd(bitwShiftR(int, 16), 255)
  green = bitwAnd(bitwShiftR(int, 8), 255)
  blue = bitwAnd(int, 255)
  alpha = bitwAnd(bitwShiftR(int, 24), 255)
  rgb(red, green, blue, alpha, maxColorValue = 255)
})

rgb_to_int <- (function(rgb) {
  red = rgb[1]
  green = rgb[2]
  blue = rgb[3]
  RGBint = bitwShiftL(red, 16) + bitwShiftL(green, 8) + blue
  RGBint
})

int_to_rgb <- Vectorize(function(int) {
  red = bitwAnd(bitwShiftR(int, 16), 255)
  green = bitwAnd(bitwShiftR(int, 8), 255)
  blue = bitwAnd(int, 255)
  rgb(red, green, blue, maxColorValue = 255) 
})

