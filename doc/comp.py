#!/usr/bin/env python

from PIL import Image, ImageFont, ImageDraw
import sys

im1 = Image.open(sys.argv[1]+"-"+sys.argv[2]+".png")
im2 = Image.open(sys.argv[1]+"-"+sys.argv[3]+".png")
im3 = Image.open(sys.argv[1]+"-"+sys.argv[4]+".png")
im4 = Image.open(sys.argv[1]+"-"+sys.argv[5]+".png")
im5 = Image.open(sys.argv[1]+"-"+sys.argv[6]+".png")

f = ImageFont.truetype("~/Library/Fonts/Inconsolata-Regular.ttf", 24)

c0 = im1.crop((0,230,200,630))
d0 = ImageDraw.Draw(c0)
d0.text((90,370), "orig", font=f)
c1 = im1.crop((1280,230,1480,630))
d1 = ImageDraw.Draw(c1)
d1.text((90,370), sys.argv[2], font=f)
c2 = im2.crop((1280,230,1480,630))
d2 = ImageDraw.Draw(c2)
d2.text((90,370), sys.argv[3], font=f)
c3 = im3.crop((1280,230,1480,630))
d3 = ImageDraw.Draw(c3)
d3.text((90,370), sys.argv[4], font=f)
c4 = im4.crop((1280,230,1480,630))
d4 = ImageDraw.Draw(c4)
d4.text((90,370), sys.argv[5], font=f)
c5 = im5.crop((1280,230,1480,630))
d5 = ImageDraw.Draw(c5)
d5.text((90,370), sys.argv[6], font=f)

im = Image.new("RGB", (1200,400))

im.paste(c1, (0,0))
im.paste(c2, (200,0))
im.paste(c0, (400,0))
im.paste(c3, (600,0))
im.paste(c4, (800,0))
im.paste(c5, (1000,0))

im.save(sys.argv[1]+"-comp.png")

