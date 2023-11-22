#!/bin/bash

URL='https://www.google.com/search?q='
QUERY=$(echo '' | dmenu -i -p "Search:" -sf '#d4be98' -nf '#d4be98')
if [ -n "$QUERY" ]; then
  xdg-open "${URL}${QUERY}" 2> /dev/null
fi
