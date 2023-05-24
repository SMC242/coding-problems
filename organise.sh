#!/bin/bash
expr="https\:\/\/(www\.)?(\w+)\.*"
for file in *.hs; do
    contents=$(cat $file)
    if [[ $contents =~ $expr ]];  then
        folder="${BASH_REMATCH[2]}"
        mv "$file" "${folder}/${file}"
    fi
done
