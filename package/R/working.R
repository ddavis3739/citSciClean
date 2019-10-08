#reef = read.csv('data/reefData..csv')

#reef$SpeciesCommon = strsplit(reef$Species, )


install.packages('taxize')
library(taxize)

fish = read.csv('data/fish.csv')
fishNames = read.csv('data/fish_names.csv')
trips = read.csv('data/trips.csv')

