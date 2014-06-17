## getAllSiteFiles.R: get a list of the full paths to files of 
## a specified extension for each site at each project for a given year

getAllSiteFiles = function(year, suffix="_alltags.rds") {
  projs = dir(sprintf("/SG/contrib/%d", year),
    pattern = sprintf("^[[:alpha:]]+$", year),
    full.names = TRUE)
  sites = dir(projs,
    pattern = "^[[:alpha:]0-9_]+$",
    full.names=TRUE)
  files = dir(sites,
    pattern = paste("^", year, "_[[:alpha:]0-9_]+", suffix, "$", sep=""),
    full.names=TRUE)
}
