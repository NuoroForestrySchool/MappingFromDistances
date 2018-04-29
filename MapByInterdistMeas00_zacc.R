#: Title    : Tri2Coo
#: Affiliation    : "NFS" <Nuoro.Forestry.School@gmail.com>
#: Author    :  Roberto Scotti <scotti@uniss.it>
#: Date    : 17/03/2014
#: Version    : -1
#: Description    : Compute tree coordinates from intr-tree distance measurements
#: Usage         : <in progress>
#: Options    : None

#: 17/03/2014   : Initial release

rm(list=ls(all=TRUE))
require(RSQLite)
require(calibrate)

#path_rs <- "/home/rs/Documenti/00-DropBox/scotti_uniss/Dropbox/Tri2Coo/"
#path_rs <- "G:\\Il mio Drive\\DocumentiGDuniss\\08-TOOLS\\Tri2Coo\\Tri2Coo\\"
#path_sc <- "/home/sergio/DropboxUniss/Dropbox/Tri2Coo/"
#lpath <- path_rs

#setwd(lpath)
source("ComputeP3coords.R")

dsn <- "MbIM0rs.sqlite"
con <- dbConnect(SQLite(), dbname="MbIM0rs.sqlite")
dbListTables(con)

intd <- read.csv("InterdistancesRaw.csv")
intd[intd$m_id==90,"left"]<-42
intd[intd$m_id==90,"right"]<-7
intd[intd$m_id==92, "left"] <- NA
intd[intd$m_id==92, "right"] <- 42
intd <- intd[!intd$m_id %in% c(67, 124),]
intd[intd$m_id==90, "dist"] <- 10.05
intd[intd$m_id==116, "dist"] <- 4.85
intd[intd$m_id==116, "right"] <- 98


dbWriteTable(con,"Interdistances",intd, overwrite=T, row.names=F)

# verification of input data
if (nrow(dbReadTable(con, "LonelyP"))>0) stop("There are trees involved in less than 2 measurements! Verify 'LonelyP', STOPPING")
if (nrow(dbReadTable(con, "DoubleMeasures"))>0) stop("There are distances measured more than once! Verify 'DubleMeasures', STOPPING") 
if (nrow(dbReadTable(con, "ContradictoryTris"))>0) stop("There are trees in contradictory positions! Verify 'ContradictoryTris', STOPPING") 
if (nrow(dbReadTable(con, "MissingTopology"))>0) stop("There are trees without relative position! Verify 'MissingTopology', STOPPING") 
if (nrow(dbReadTable(con, "DegeneratedTris"))>0) stop("There are topology problems! Verify 'DegeneratedTris', STOPPING") 


a <- 1 # 39
b <- 2 # 49
id <- dbReadTable(con, "Interdistances")
dist_a_b <- id$dist[(id$from==a & id$to==b)|(id$from==b & id$to==a)]
TreeCoo0 <- data.frame(p=a, local_x=0, local_y=0, n=0, sx=0, sy=0)
TreeCoo0 <- rbind(TreeCoo0, data.frame(p=b, local_x=0, local_y=dist_a_b, n=0, sx=0, sy=0))
dbWriteTable(con,"TreeCoo",TreeCoo0, overwrite=T, row.names=F)

iteration <- 0
repeat {
  iteration <- iteration + 1
  print(paste("ITERATION =", iteration))
  st <- dbReadTable(con,"SolvableTris" )
  print(st)
  n <- nrow(st)
  if(n==0) break
  p3a <- NULL
  for(i in 1:n){
    st0 <- st[i,]
    coo_p3 <- with(st0, ComputeP3coords(P1_x, P1_y, P2_x, P2_y, p1_p3, p2_p3, ifelse(pos=='l', -1, 1)))
    p3a <- rbind(p3a, with(st0, 
                           data.frame(p=p3, p_A=p1, p_B=p2, local_x=coo_p3[1], local_y=coo_p3[2])))
  }
  print(p3a)
  stopifnot (sum(c( is.nan(p3a$local_x), is.nan(p3a$local_y))) == 0) 
  p3x <- aggregate(p3a$local_x,by=list(p=p3a$p),
                   FUN=function(x){c(lx=mean(x),n=length(x),sx=sd(x))})
  p3y <- aggregate(p3a$local_y,by=list(p=p3a$p),
                   FUN=function(x){c(ly=mean(x),n=length(x),sy=sd(x))})
  p3xx <- data.frame(p3x$x)
  p3yx <- data.frame(p3y$x)
  p3 <- data.frame(cbind(p=p3x$p,local_x=p3xx$lx,local_y=p3yx$ly,n=p3xx$n,sx=p3xx$sx,sy=p3yx$sy))
  print(p3)
  dbWriteTable(con, "TreeCoo", p3, append=TRUE, row.names=F)
}

tc <- dbReadTable(con, "TreeCoo")
with(tc, plot(local_x,local_y, pch=""))
with(tc, textxy(local_x,local_y, p, cex=.8, offset=0))
