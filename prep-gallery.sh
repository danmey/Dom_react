#!/bin/sh

dir="丽江LIJIANG 2"
mkdir -p db/"$dir"/mini/;rm -f db/"$dir"/mini/*
mkdir -p db/"$dir"/thumbs/;rm -f db/"$dir"/thumbs/*
for f in db/"$dir"/*.jpg; do
    file=`basename "$f"`
    convert -resize '600x600>' -background black -gravity center -extent 600x600 "db/$dir/$file" "db/$dir/mini/$file"
    convert -thumbnail '80x80>' -background black -gravity center -extent 80x80 "db/$dir/$file" "db/$dir/thumbs/$file"
done
cd "db/$dir/thumbs/"

for file in *.jpg; do 
        jhead "$file" | grep "Date/Time" | sed -n 's/[^:]\+: \([^ ]\+\).*/\1/p' "$file"
done
        
