#load the needed libraries
library(data.table)
library(dplyr)

#read training data and filter the right dates
df <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", 
                 colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric')) %>% 
  tbl_df %>%
  mutate(Date = as.Date(Date, "%d/%m/%Y")) %>%
  filter(Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))

# keep only complete records
df <- df[complete.cases(df),]

# combine date and time
df <- mutate(df, DateTime = paste(Date, Time))

# format DateTime Column
df$DateTime <- as.POSIXct(df$DateTime)

### end of data transformation

#plot 4
with(df, plot(Voltage~DateTime, type="l", ylab="Voltage", xlab="datetime"))
with(df, plot(Global_reactive_power~DateTime, type="l", ylab="Global_reactive_power", xlab="datetime"))

par(mfcol=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(df, {
  plot(Global_active_power~DateTime, type="l", ylab="Global Active Power", xlab="")
  plot(Sub_metering_1~DateTime, type="l", ylab="Energy sub metering", xlab="")
  lines(Sub_metering_2~DateTime,col='Red')
  lines(Sub_metering_3~DateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Voltage~DateTime, type="l", ylab="Voltage", xlab="datetime")
  plot(Global_reactive_power~DateTime, type="l", ylab="Global_reactive_power", xlab="datetime")
})

dev.copy(png,"plot4.png", width=480, height=480)
dev.off()  

