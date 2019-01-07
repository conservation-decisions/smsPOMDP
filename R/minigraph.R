#' @export
minigraph = function(man, sur, size = 1){
  library(imager)
  if (sur >0){
    par(mfrow = c(1,1))
    m = paste0(man,' years')
    s = paste0(sur,' years')
    t = load.image(file=paste(path.package("TigerTest"),"/extdata/im1.jpg",sep="")) 
    plot(t,axes = F)
    text(200,200,'Not seen for', cex = size)
    text(200, 250, m, cex = size)
    text(500,400, 'Seen', cex = size)
    text(600,700, 'Not seen for', cex = size)
    text(600,750, s, cex = size)
    text(1000,200,'Seen', cex = size)
  } else {
    par(mfrow = c(1,1))
    m = paste0(man,' years')
    t = load.image(file=paste(path.package("TigerTest"),"/extdata/im2.jpg",sep="")) 
    plot(t, axes = F)
    text(550,400,'Not seen for', cex = size)
    text(550, 450, m, cex = size)
    text(1000,200,'Seen', cex = size)
  }

}
