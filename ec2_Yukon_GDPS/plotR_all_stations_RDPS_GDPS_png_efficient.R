#***This code calls a file (stations.txt) that contains the list of the stations. Please modify this list if needed. Contact Dominique Richard for details at dominique.richard@usask.ca.

Date_Today <- format(Sys.time(), "%Y%m%d")
#Date_Today <- "20180429"

#Creating the directory for R plots of all basins.
Rplots_path <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow/Plots", Date_Today)
dir.create( Rplots_path )

#Reading the names of stations and associated river.
stations <- read.csv ("/home/ec2-user/Yukon_GDPS/scripts/stations.txt", header=FALSE, sep=",")
stations

for (i in 1:length(stations[,1]))
{


##Prepare to plot observed values##
o <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow", stations[i,1], "Streamflow_Observations.csv")
obs <- read.csv (o, skip=1, sep=" ")
date1 <- paste(obs[, 1], obs[, 2])
date2 <- as.POSIXlt(date1)
flow1 <- obs[,3]
fig_obs <- data.frame(date2, flow1)

##Prepare to plot RDPS model values##

file_name_2a <- paste('MESH_output_streamflow_ts_RDPS_', stations[i,1], '_', Date_Today, '.csv', sep='')
m <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow", stations[i,1], "Archive", "RDPS", file_name_2a)
mesh <- read.csv(m, header=TRUE)
date_mesh1 <- paste(mesh[, 1], mesh[, 2], mesh[, 3], mesh[,4])
#Converting day of year date format into the working format for the model streamflow values.
date_mesh2 <- strptime(date_mesh1, format="%Y %j %H %M")
 
#The following if statement deals with mesh model files that contain values from 2 stations (one watershed).
  if (stations[i,1] == "09BC002")
  {
  flow2 <- mesh[,8]
  fig_mesh <- data.frame(date_mesh2, flow2)
  }
  else
  {
  flow2 <- mesh[,6]
  fig_mesh <- data.frame(date_mesh2, flow2) 
  }
#X labels
xrange <- seq(from=fig_mesh[1,1], by="24 hours", length.out=3)
xrange2 <- seq(from=fig_mesh[1,1], by="12 hours", length.out=5)

#Defining the correct x range in observed data column.
#Finding the indices of the first occurence of model day 1 16:00 and the end of the observed data set.
x1 <- match(c(fig_mesh[1,1]),fig_obs[,1])
#print (x1)
x2 <- length(fig_obs$date2)

#Nudging the model values to have both curves start at same y value.
diff <- fig_mesh[1,2] - fig_obs[x1,2]
mesh_nudged <- fig_mesh[,2] - diff

#Position of Y labels
#Finding max and min observed values.
maxy <- max(max(fig_obs$flow1), max(mesh_nudged), na.rm=T)
#miny <- (min(fig_obs$flow1))
y_mesh <- seq(from=0, to=maxy, by=100 )
#maxy <- max(fig_obs$flow1)
#print (maxy)
#maxy <- max(mesh_nudged)
#print (maxy)

if ( maxy <= 200 )
   {
     y_mesh <- seq(from=0, to=maxy, by=50)
   }

if ( maxy <= 100 )
   {
     y_mesh <- seq(from=0, to=maxy, by=10)
   }
else if ( maxy <= 10 )
   {
     y_mesh <- seq(from=0, to=maxy, by=1)
   }

#GDPS

file_name_2b <- paste('MESH_output_streamflow_ts_GDPS_', stations[i,1], '_', Date_Today, '.csv', sep='')
m <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow", stations[i,1], "Archive", "GDPS", file_name_2b)
mesh_GDPS <- read.csv(m, header=TRUE)
date_mesh1_GDPS <- paste(mesh_GDPS[, 1], mesh_GDPS[, 2], mesh_GDPS[, 3], mesh_GDPS[,4])
#Converting day of year date format into the working format for the model streamflow values.
date_mesh2_GDPS <- strptime(date_mesh1_GDPS, format="%Y %j %H %M")

#The following if statement deals with mesh model files that contain values from 2 stations (one watershed).
  if (stations[i,1] == "09BC002")
  {
  flow2_GDPS <- mesh_GDPS[,8]
  fig_mesh_GDPS <- data.frame(date_mesh2_GDPS, flow2_GDPS)
  }
  else
  {
  flow2_GDPS <- mesh_GDPS[,6]
  fig_mesh_GDPS <- data.frame(date_mesh2_GDPS, flow2_GDPS)
  }
#X labels
xrange <- seq(from=fig_mesh_GDPS[1,1], by="24 hours", length.out=10)
xrange2 <- seq(from=fig_mesh_GDPS[1,1], by="24 hours", length.out=10)

#Defining the correct x range in observed data column.
#Finding the indices of the first occurence of model day 1 16:00 and the end of the observed data set.
x1 <- match(c(fig_mesh_GDPS[1,1]),fig_obs[,1])
#print (x1)
x2 <- length(fig_obs$date2)

#Nudging the model values to have both curves start at same y value.
diff <- fig_mesh_GDPS[1,2] - fig_obs[x1,2]
mesh_nudged_GDPS <- fig_mesh_GDPS[,2] - diff

#Position of Y labels
#Finding max and min observed values.
maxy <- max(max(fig_obs$flow1), max(mesh_nudged_GDPS), na.rm=T)
#miny <- (min(fig_obs$flow1))
y_mesh <- seq(from=0, to=maxy, by=100 )
#maxy <- max(fig_obs$flow1)
#print (maxy)
#maxy <- max(mesh_nudged)
#print (maxy)

if ( maxy <= 200 )
   {
     y_mesh <- seq(from=0, to=maxy, by=50)
   }

if ( maxy <= 100 )
   {
     y_mesh <- seq(from=0, to=maxy, by=10)
   }


#Starting to plot
name <- paste('Rplot_', stations[i,1], '_', Date_Today, '.png', sep='')
image_path <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow/Plots", Date_Today, name)
png(image_path, width = 600, height = 400 )
#Plotting model values
par(mar=c(5.1,4.5,4.1,2.1))
plot(fig_mesh_GDPS$date_mesh2_GDPS, mesh_nudged_GDPS, type="l",lwd=c(2), ylim=c(0, maxy), xlab = "Date", ylab=expression ('Streamflow '('m'^3*'s'^-1)), main=paste("Streamflow Forecast for", stations[i,2], "\nStation", stations[i,1], sep=" "), cex=0.5, col="red", xaxt='n', yaxt="n")
#grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
abline(h=y_mesh, col = "lightgray", lty = "dotted")
#Plotting observed values
#lines(fig_mesh_GDPS$date_mesh2_GDPS, mesh_nudged_GDPS, cex=0.5, col="green", lwd=c(2))
lines(fig_mesh[,1],mesh_nudged, cex=0.5, col="green", lwd=c(2))
lines(fig_obs[(x1:x2),], cex=0.5, col="blue", lwd=c(2))

#Creates 5 ticks for the x axis.
axis(1, at=xrange2, labels=FALSE, tick = TRUE, tcl="-0.4")
#text(x=xrange,  par("usr")[3], 
 #   labels = strftime(xrange, format="%d %m %Y %H:%M"), adj = c(1.1,1.5), srt = 25, xpd = TRUE, cex=0.8)

#Places x labels at 3 ticks. 
#mtext("Date and Time (PST)", side=1, line=2)
text(x=xrange,  par("usr")[3], 
     labels = strftime(xrange, format="%d/%m"), adj = c(0.5,2.8), xpd = TRUE, cex=0.9) #instead of adj you can use pos = 1

#Creates ticks every 10 for the y axis, writes labels at y_mesh values and makes labels horizontal.
#y_mesh_tick <- seq(from=0, to=maxy, by=10 )
axis(2, at = y_mesh,  las="1", labels = y_mesh, tcl="0.3")
axis(4, at = y_mesh,  las="1", labels = FALSE, tcl="0.3")

#Creates legend at the bottom left of the graph.
#legend(x="bottomleft", inset=0.05, c("10-day forecast","2-day forecast", "Gauged"), col=c("red", "green", "blue"), lwd=c(2), cex=1.0)
legend(x="bottom", horiz=TRUE, inset=(0.05), c("10-day forecast","2-day forecast", "Gauged"), col=c("red", "green", "blue"), lwd=c(2), cex=1.0)
box()

#Archiving reformatted streamflow observations file.
#file_name_1 <- paste('Streamflow_Observations', stations[i,1], '_', Date_Today, '.csv', sep='')
#Source_path_1 <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow",  stations[i,1], "Streamflow_Observations.csv")
#Archive_path_1 <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow", stations[i,1], "Archive", file_name_1)
#file.rename(Source_path_1, Archive_path_1)

#Archiving RDPS Mesh streamflow output file.
#file_name_2a <- paste('MESH_output_streamflow_ts_RDPS_', stations[i,1], '_', Date_Today, '.csv', sep='')
#Source_path_2a <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow",  stations[i,1], "MESH_output_streamflow_ts_RDPS.csv")
#Archive_path_2a <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow",  stations[i,1], "Archive", "RDPS", file_name_2a)
#file.rename(Source_path_2a, Archive_path_2a)

#Archiving GDPS Mesh streamflow output file.
#file_name_2b <- paste('MESH_output_streamflow_ts_GDPS_', stations[i,1], '_', Date_Today, '.csv', sep='')
#Source_path_2b <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow",  stations[i,1], "MESH_output_streamflow_ts_GDPS.csv")
#Archive_path_2b <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow",  stations[i,1], "Archive", "GDPS", file_name_2b)
#file.rename(Source_path_2b, Archive_path_2b)


#Archiving raw streamflow observations file.
#file_name_3 <- paste('YT_', stations[i,1], '_hourly_hydrometric.csv', sep='')
#file_name_4 <- paste('YT_', stations[i,1], '_hourly_hydrometric', '_', Date_Today, '.csv', sep='')
#Source_path_3 <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow",  stations[i,1], file_name_3)
#Archive_path_3 <- file.path("/home/ec2-user/Yukon_GDPS/Streamflow",  stations[i,1], "Archive", file_name_4)
#file.rename(Source_path_3, Archive_path_3)

dev.off()

}
