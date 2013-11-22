latLonDist = function(lat1, lat2, lon1, lon2) {
  lat1 = rad(lat1)
  lat2 = rad(lat2)
  lon1 = rad(lon1)
  lon2 = rad(lon2)

  a = sin((lat2-lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2
  
  ## a = sin²(Δφ/2) + cos(φ1).cos(φ2).sin²(Δλ/2)

  c = 2 * atan2(sqrt(a), sqrt(1-a))

  return (6371 * c)
}
