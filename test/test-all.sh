#!/bin/bash

shopt -s nullglob

for file in *.rkt; do
	echo "$file"
	racket "$file"
done
