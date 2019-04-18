if(FALSE){
  load('c:/github/PacFIN_Logbook_Download_and_Cleanup/R/Funcs and Data/EEZ.Polygon.WestCoast.dmp')
  load('c:/github/PacFIN_Logbook_Download_and_Cleanup/R/Funcs and Data/Points.out.of.Dat.and.polygons.dmp')
}

map('worldHires', #regions=c("Canada","Mexico"),
       xlim = c(-130, -116.8), ylim = c(30, 49.5),
       col='grey', fill=TRUE, interior=TRUE, , lwd=1)
polygon(EEZ.Polygon.WestCoast, col=rgb(0,0,1,.1))
polygon(CoastWidePolygon, col=rgb(0,1,0,.2))
polygon(BankPolygon, col=rgb(1,0,0,.2))
legend('right', bg='white', legend=c("EEZ","CoastWidePolygon","BankPolygon"),
       fill=c(rgb(0,0,1,.1), rgb(0,1,0,.2), rgb(1,0,0,.2)))
 

  load('c:/github/PacFIN_Logbook_Download_and_Cleanup/R/Funcs and Data/LogBookBlocks.dmp')
