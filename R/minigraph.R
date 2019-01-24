#' @export
minigraph = function(tab, size = 1){
  n = nrow(tab)
  if (n ==3){
    graphics::par(mfrow = c(1,1))
    y1 = paste0(tab[1,2],' years')
    y2 = paste0(tab[2,2],' years')
    a1 = switch (as.character(tab[1,1]), 1 = 'MANAGE',2 = 'SURVEY',3 = 'NOTHING')
    a2 = switch (as.character(tab[2,1]), 1 = 'MANAGE',2 = 'SURVEY',3 = 'NOTHING')
    a3 = switch (as.character(tab[3,1]), 1 = 'MANAGE',2 = 'SURVEY',3 = 'NOTHING')
    t = imager::load.image(file=paste(path.package("TigerPOMDP"),"/extdata/im3.jpg",sep=""))
    graphics::plot(t, axes = F)
    graphics::text(200,200,'Not seen for', cex = size)
    graphics::text(200, 250, y1, cex = size)
    graphics::text(500,400, 'Seen', cex = size)
    graphics::text(600,700, 'Not seen for', cex = size)
    graphics::text(600,750, y2, cex = size)
    graphics::text(1000,200,'Seen', cex = size)
    graphics::text(620, 100, a1, cex = size)
    graphics::text(200, 575, a2, cex = size)
    graphics::text(1050, 575, a3, cex = size)
  } else if (n ==2){
    y1 = paste0(tab[1,2],' years')
    a1 = switch (as.character(tab[1,1]), 1 = 'MANAGE',2 = 'SURVEY',3 = 'NOTHING')
    a2 = switch (as.character(tab[2,1]), 1 = 'MANAGE',2 = 'SURVEY',3 = 'NOTHING')
    t = imager::load.image(file=paste(path.package("TigerPOMDP"),"/extdata/im2.jpg",sep=""))
    graphics::plot(t, axes = F)
    graphics::text(550,400,'Not seen for', cex = size)
    graphics::text(550, 450, y1, cex = size)
    graphics::text(1000,200,'Seen', cex = size)
    graphics::text(620, 100, a1, cex = size)
    graphics::text(1050, 575, a2, cex = size)
  } else if (n ==1){
    a1 = switch (as.character(tab[1,1]), 1 = 'MANAGE',2 = 'SURVEY',3 = 'NOTHING')
    t = imager::load.image(file=paste(path.package("TigerPOMDP"),"/extdata/im1.jpg",sep=""))
    graphics::plot(t, axes = F)
    graphics::text(650, 350, a1, cex = size*3)
  }



}
