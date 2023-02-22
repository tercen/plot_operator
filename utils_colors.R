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

rgb_to_int <- Vectorize(function(rgb) {
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


COLOR_LIST_1 = c(
  2062516,
  14883356,
  3383340,
  10931939,
  11722634,
  16489113,
  16629615,
  16744192,
  13284054,
  6962586,
  16777113,
  11622696,
  1810039,
  14245634,
  7696563,
  15149450,
  6727198,
  15117058,
  10909213,
  6710886,
  14948892,
  3636920,
  5091146,
  9981603,
  16744192,
  16777011,
  10901032,
  16220607,
  10066329,
  14948892,
  3636920,
  5091146,
  9981603,
  16744192,
  16777011,
  10901032,
  16220607
)

COLOR_LIST_2 = c(
  255,
  65280,
  65535,
  32768,
  8388736,
  128,
  8388608,
  32896,
  8421376,
  16711935,
  4063487,
  4128512,
  16777022,
  4128767,
  32830,
  8404608,
  4063360,
  8404480,
  4096128,
  8421438,
  4079359,
  16776960,
  11534160,
  16711680,
  35327,
  12517185,
  16711936,
  5701545,
  189,
  16711680,
  13565745,
  28671,
  16711680,
  13958955,
  214,
  16732416,
  22015,
  9830250,
  16711680,
  3839,
  10354530,
  16711680,
  5592575,
  5635925,
  5636095,
  5614421,
  11228587,
  5592491,
  11228501,
  5614507,
  11250517,
  16733695,
  8345087,
  8388437,
  16777087,
  8388607,
  5614463,
  11239339,
  8345003,
  11239253,
  8367019,
  11250559,
  8355839,
  16777045,
  13303691,
  16733525,
  5616127,
  13959041,
  16733781,
  9437126,
  5592531,
  16733525,
  14679926,
  5611519,
  16733525,
  14942066,
  5592548,
  16747349,
  5607167,
  12189596,
  16733525,
  5595135,
  12517271,
  16733525
)
