# STICKER

library(hexSticker)
library(magick)

image_read("sticker/tabloid_dessin.png")

sticker("sticker/tabloid_dessin.png",
        package="",
        p_size=0,
        p_y = 1.55,
        s_x=1,
        s_y=1,
        s_width=.65,
        filename="sticker/sticker_tabloid.png",
        h_fill = "#E9967A",
        h_color = "#8B1A1A")

image_read("sticker/sticker_tabloid.png")
