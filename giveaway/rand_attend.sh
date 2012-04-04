#!/bin/bash

### Door prize lottery tool
### Download the participant list, extract the names then draw a winner

URL=$1

wget $URL

grep "<span class=\"D_name\">" index.html | sed 's/<span class=\"D_name\">//g' > attendees.csv

R --no-save --slave < draw.R

