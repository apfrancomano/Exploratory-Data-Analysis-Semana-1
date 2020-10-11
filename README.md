# Exploratory-Data-Analysis-Semana-1
Curso de coursera

## Primero tuve que cargar la información en R usando:
file <- read.table("household_power_consumption.txt", header=TRUE, sep=";",
                   na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
## Después, para confirmar que la información se haya cargado de manera correcta use la función
head(file)

## Cuando confirme que la data estaba bien cargada, recordé que tenía que modificar el formato fecha, por lo que utilice la siguiente fórmula:
file$Date <- as.Date(file$Date, "%d/%m/%Y")

## Y después tocaba filtrar desde  2007-02-01 a 2007-02-02 (We will only be using data from the dates 2007-02-01 and 2007-02-02. One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.)

file <- subset(file,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Saque los datos incompletos

file <- file[complete.cases(file),]

## Uni la columna de fecha y hora

dateTime <- paste(file$Date, file$Time)

## Y después lo que hice fue ponerle un nombre al vector, elimine la columna de fecha y hora del archivo file y agregue la columna DateTime


dateTime <- setNames(dateTime, "DateTime")
file <- file[ ,!(names(file) %in% c("Date","Time"))]
file <- cbind(dateTime, file)
file$dateTime <- as.POSIXct(dateTime)


## Bueno, después de hacer esto hay que empezar a hacer los gráficos. El primero es un histograma rojo, que hay que cambiarle la leyenda del eje x, del eje y & agregar un título. La variable a utilizar es Global_active_power

hist(file$Global_active_power,xlab = "Global Active Powe (kilowatts)", ylab = "Frequency",main="Global Active Power",col="red")

## Y después de hacer esto tenes que guardar el primer gráfico
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

## Gráfico 2. Es cómo de linea, la variable a utilizar es Global_active_power y depende de la variable de tiempo. Es en negro así que no hay que ponerle color y se cambia la leyenda del eje y. Cómo mi R está en español el eje x queda en español
plot(file$Global_active_power~file$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

## Gráfico 3. Es cómo de linea, la variable a utilizar es Global_active_power y depende de la variable de tiempo. Es en negro así que no hay que ponerle color y se cambia la leyenda del eje y. Cómo mi R está en español el eje x queda en español

with(file, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Energy sub metering", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png,"plot3.png", width=480, height=480)
dev.off()

## PLOT 4 son 4 gráficos así que es bidimensional, dos son iguales a los que hice y los otros dos son nuevos. La primer diferencia es que tuve que definir el margen y margen exterior. Después es prestar atención a cada leyenda según el gráfico.

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(file, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage", xlab="datetime")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Energy sub metering", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global_reactive_power",xlab="datetime")
})

dev.copy(png,"plot4.png", width=480, height=480)
dev.off()
