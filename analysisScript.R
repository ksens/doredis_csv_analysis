rm(list=ls())
file.pipe <- pipe(" awk '{if (++count%80==1) print $0;} ' < TR_20150611_TEST.csv ")
df = read.csv(file.pipe)

minworkerStartTime = min(df$workerStartTime)
df = transform(df, 
                started_at_num = 
                 as.numeric(strptime(as.character(started_at), "%Y-%m-%d %H:%M:%S ", tz = "UTC")) - minworkerStartTime,
                finished_at_num = 
                 as.numeric(strptime(as.character(finished_at), "%Y-%m-%d %H:%M:%S ", tz = "UTC")) - minworkerStartTime,
                scidbconnectendtime_num = 
                  scidbconnectEndTime-minworkerStartTime,
                getseriesendtime_num = 
                  getSeriesEndTime-minworkerStartTime,
                id = 
                  as.numeric(factor(local_inet)))
print(head(df))

plot(c(-10, 800), c(0, 25), type = "n", xlab ="time (s)", ylab="worker # / CPU util.% div. by 4")
points(df$started_at_num, df$id)
#colorbar = sample(colours(), length(df$id))
#colorbar = rep(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"
#             , "#b15928"), 8)[1:length(df$started_at_num)]
segments(df$getseriesendtime_num, df$id, df$finished_at_num, df$id, col = "black", lwd= 1)
segments(df$started_at_num, df$id, df$getseriesendtime_num, df$id, col = "red", lwd=2)
# 
# CPU utilization
df2 = read.csv("test.cpu.csv")
df2 = subset(df2, CPU=="all")
df2 = transform(df2, time_num =
                        as.numeric(strptime(sprintf("2016-04-30 %s", as.character(TIME)), "%Y-%m-%d %I:%M:%S", tz="EST5EDT")) + 12*60*60 - minworkerStartTime + 60)
print(head(df2))

lines(df2$time_num, (df2$X.user + df2$X.system)/4, lty=2, col="blue", lwd=0.5)
