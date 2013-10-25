## shorthand query function

sql = function(con, ...) {
  dbGetQuery(con, sprintf(...))
}
