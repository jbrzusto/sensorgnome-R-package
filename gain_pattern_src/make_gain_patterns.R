#!/usr/bin/Rscript
##
## make the gain patterns.
##

if (0 == system("octave --version") && file.exists("~/libs/Orfanidis_ewa")) {
    ## if possible, regenerate patterns to text files using octave/matlab
    ## code from Orfanidis
    system("octave < yagi_pattern.m")
}

## for each pattern file, the first 3 numbers are:
## - number of rows in matrix; each row is for a fixed value of theta, 0...360
## - number of columns in matrix; each row is for a fixed value of phi, 0...90
## - max gain (dBi)

x = scan("yagi_5_pattern.txt")

dims = x[1:2]
maxgain = x[3]

x = matrix(x[-(1:3)], byrow=TRUE, nrow=dims[1])

## convert to absolute gain in linear units
x = maxgain * x

## only need theta from 90...270, since pattern is symmetric
yagi5pattern = x[91:271,]

save(list="yagi5pattern", file="../data/yagi5pattern.rda")


x = scan("yagi_9_pattern.txt")

dims = x[1:2]
maxgain = x[3]

x = matrix(x[-(1:3)], byrow=TRUE, nrow=dims[1])

## convert to absolute gain in linear units
x = 10^(maxgain/10) * x

## only need theta from 90...270, since pattern is symmetric
yagi9pattern = x[91:271,]

save(list="yagi9pattern", file="../data/yagi9pattern.rda")


