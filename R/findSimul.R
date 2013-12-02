## findSimul.R - find simultaneous tag detections in a tag dataframe
##
## two detections are 'simultaneous' if they are within 2 seconds and
## on either different receivers, or different antennas of a single
## receiver

findSimul = function(x, sameRecvOK=TRUE) {
  ord = order(x$ts, x$id, x$recv, x$ant)
  ts = as.numeric(x$ts)[ord]
  id = x$id[ord]
  recv = x$recv[ord]
  ant = x$ant[ord]
  
  i = 1L
  j = i + 1L
  n = nrow(x)
  k = 1L

  simul = matrix(integer(), nrow=n, ncol=2)
  while (j <= n) {
    if (id[i] == id[j] && ts[j] - ts[i] <= 2 && (recv[j] != recv[i] || (sameRecvOK && ant[i] != ant[j]))) {
      simul[k, 1L] = i
      simul[k, 2L] = j
      k = k + 1L
      j = j + 1L
    } else {
      i = i + 1L
    }
    if (i == j)
      j = j + 1L
  }
  return (matrix(ord[simul[1:(k-1),]], ncol=2))
}
