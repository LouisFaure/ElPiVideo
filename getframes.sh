#!/usr/bin/env bash
ffmpeg -i video.mp4 -vframes 100 output_%04d.jpg

FOLDER="/"

# max height
WIDTH=360

# max width
HEIGHT=640

#resize jpg only to either height or width, keeps proportions using imagemagick
find . -iname '*.png' -o -iname '*.jpg' -exec convert \{} -verbose -brightness-contrast 10x5 -colorspace Gray -resize $WIDTHx$HEIGHT\> \{} \;