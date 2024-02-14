# Introducción

En este informe, se analiza un conjunto de datos de actividad personal recolectado a través de un dispositivo de monitoreo. El objetivo es explorar los patrones de actividad diaria, imputar datos faltantes y comparar los patrones de actividad entre días de semana y fines de semana.

## 1. Carga y preprocesamiento de datos

Para comenzar, cargamos y preprocesamos los datos necesarios para el análisis.


```r
# Paso 1: Descargar el archivo ZIP  
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"  
download.file(url, destfile = "activity.zip", mode = "wb")


# Paso 2: Extraer el contenido del archivo ZIP

unzip("activity.zip")

# Paso 3: Leer el archivo CSV en R

#Archivo con NAs
activity_data_wNA <- read.csv("activity.csv")

#Archivo sin NAs
activity_data <- na.omit(activity_data_wNA) 
```

## 2. What is mean total number of steps taken per day?

1)  Total number of steps taken per day


```r
total_steps_per_day <- aggregate(steps ~ date, data = activity_data, FUN = sum)
total_steps_per_day$WeekDay <- weekdays(as.Date(total_steps_per_day$date))

total_steps_by_weekday <- aggregate(steps ~ WeekDay, data = total_steps_per_day, FUN = sum)

total_steps_by_weekday$WeekDay <- factor(total_steps_by_weekday$WeekDay, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

total_steps_by_weekday <- total_steps_by_weekday[order(total_steps_by_weekday$WeekDay), ]

# Mostrar los resultados
print(total_steps_by_weekday)
```

```
##     WeekDay steps
## 3     lunes 69824
## 4    martes 80546
## 5 miércoles 94326
## 2    jueves 65702
## 7   viernes 86518
## 6    sábado 87748
## 1   domingo 85944
```

```r
cat("Total steps per day:", sum(total_steps_by_weekday$steps))
```

```
## Total steps per day: 570608
```

2)  Histogram of the total number of steps taken each day


```r
library(ggplot2)

ggplot(total_steps_per_day, aes(x = steps))+geom_histogram(fill = "skyblue", color = "black", bins =8)+labs(
               title = "Histograma de Pasos Totales por Día",
               x = "Pasos Totales por Día", 
               y =      "Frecuencia")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

3)  Mean and Median of the total number of steps taken per day


```r
##### Mean of Total steps each weekday

mean_steps_by_weekday <- aggregate(steps ~ WeekDay, data = total_steps_per_day, FUN = mean)

mean_steps_by_weekday$WeekDay <- factor(mean_steps_by_weekday$WeekDay, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

mean_steps_by_weekday <- mean_steps_by_weekday[order(mean_steps_by_weekday$WeekDay), ]
colnames(mean_steps_by_weekday)[colnames(mean_steps_by_weekday) == "steps"] <- "Mean Steps"


print(mean_steps_by_weekday)
```

```
##     WeekDay Mean Steps
## 3     lunes   9974.857
## 4    martes   8949.556
## 5 miércoles  11790.750
## 2    jueves   8212.750
## 7   viernes  12359.714
## 6    sábado  12535.429
## 1   domingo  12277.714
```

```r
##### Median of Total steps each weekday
median_steps_by_weekday <- aggregate(steps ~ WeekDay, data = total_steps_per_day, FUN = median)

median_steps_by_weekday$WeekDay <- factor(median_steps_by_weekday$WeekDay, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

median_steps_by_weekday <- median_steps_by_weekday[order(median_steps_by_weekday$WeekDay), ]

colnames(median_steps_by_weekday)[colnames(median_steps_by_weekday) == "steps"] <- "Median Steps"

print(median_steps_by_weekday)
```

```
##     WeekDay Median Steps
## 3     lunes      10439.0
## 4    martes       8918.0
## 5 miércoles      12069.5
## 2    jueves       8551.5
## 7   viernes      11829.0
## 6    sábado      12426.0
## 1   domingo      11834.0
```

```r
### Total mean & median per day

mean_steps_per_day <- mean(total_steps_per_day$steps)
median_steps_per_day <- median(total_steps_per_day$steps)

cat("Mean steps per day:", mean_steps_per_day, "\n")
```

```
## Mean steps per day: 10766.19
```

```r
cat("Median steps per day:", median_steps_per_day, "\n")
```

```
## Median steps per day: 10765
```

## 3. What is the average daily activity pattern?

1)  Time series plot


```r
average_steps_per_interval <- aggregate(steps ~ interval, data = activity_data, FUN = mean)

ggplot(average_steps_per_interval, aes(x = interval, y = steps)) +
  geom_line(color = "blue") +
  labs(title = "Promedio de Actividad Diaria",
       x = "Intervalo de 5 Minutos", y = "Promedio de Pasos")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

2)  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_interval <- average_steps_per_interval[which.max(average_steps_per_interval$steps), ]
cat("El intervalo de 5 minutos con el mayor promedio de pasos es:", max_interval$interval, 
    "con", max_interval$steps, "pasos en promedio.\n")
```

```
## El intervalo de 5 minutos con el mayor promedio de pasos es: 835 con 206.1698 pasos en promedio.
```

## 4. Imputing missing values

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
total_missing_values <- sum(is.na(activity_data_wNA$steps))
print(total_missing_values)
```

```
## [1] 2304
```

2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
##################

activity_data_wNA$WeekDay <- weekdays(as.Date(activity_data_wNA$date))


##### Mean of Total steps each weekday without NAs

mean_steps_by_weekday <- aggregate(steps ~ WeekDay, data = total_steps_per_day, FUN = mean)

mean_steps_by_weekday$WeekDay <- factor(mean_steps_by_weekday$WeekDay, levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

mean_steps_by_weekday <- mean_steps_by_weekday[order(mean_steps_by_weekday$WeekDay), ]
colnames(mean_steps_by_weekday)[colnames(mean_steps_by_weekday) == "steps"] <- "Mean Steps"


#Then, each mean of total steps per day will be replaced in the NAs values for each day.
```

3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Imputar los valores faltantes con la media calculada

# Combina los dataframes por la columna WeekDay
# Combina los dataframes por la columna WeekDay
merged_data <- merge(activity_data_wNA, mean_steps_by_weekday, by="WeekDay", all.x=TRUE)

# Reemplaza los valores faltantes en la columna 'steps' con los valores promedio
merged_data$steps <- ifelse(is.na(merged_data$steps), merged_data$`Mean Steps`, merged_data$s)

# Elimina las columnas adicionales
merged_data <- subset(merged_data, select = c("steps", "date", "interval", "WeekDay"))

# Almacena el resultado en una nueva variable o sobrescribe la columna 'steps' en activity_data_wNA
activity_data_wNA$steps <- merged_data$steps
```

4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Crear un histograma de los pasos totales tomados cada día después de la imputación de datos 
total_steps_per_day_imputed <- aggregate(steps ~ date, data = activity_data_wNA, FUN = sum)

hist(total_steps_per_day_imputed$steps, main = "Histograma de Pasos Totales por Día (Imputado)",
     xlab = "Pasos Totales por Día", ylab = "Frecuencia")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
# Calcular y reportar la media y la mediana del número total de pasos tomados por día después de la imputación
mean_steps_per_day_imputed <- mean(total_steps_per_day_imputed$steps)
median_steps_per_day_imputed <- median(total_steps_per_day_imputed$steps)

cat("Media de pasos por día después de la imputación:", mean_steps_per_day_imputed, "\n")
```

```
## Media de pasos por día después de la imputación: 431844.5
```

```r
cat("Mediana de pasos por día después de la imputación:", median_steps_per_day_imputed, "\n")
```

```
## Mediana de pasos por día después de la imputación: 17390
```

```r
# Calcular la diferencia entre las estimaciones antes y después de la imputación de datos faltantes
difference_mean <- mean_steps_per_day_imputed - mean_steps_per_day
difference_median <- median_steps_per_day_imputed - median_steps_per_day

cat("Diferencia en la media de pasos por día antes y después de la imputación:", difference_mean, "\n")
```

```
## Diferencia en la media de pasos por día antes y después de la imputación: 421078.3
```

```r
cat("Diferencia en la mediana de pasos por día antes y después de la imputación:", difference_median, "\n")
```

```
## Diferencia en la mediana de pasos por día antes y después de la imputación: 6625
```

## 5. Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity_data_wNA$day_type <- ifelse(weekdays(as.Date(activity_data_wNA$date)) %in% c("sábado", "domingo"), "weekend", "weekday")
```

2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(gridExtra)

activity_data_imputed <- activity_data_wNA 
average_steps_per_interval <- aggregate(steps ~ interval + day_type, data = activity_data_imputed, FUN = mean)


# Crear dos gráficos separados para días de semana y fines de semana
weekday_plot <- ggplot(average_steps_per_interval[average_steps_per_interval$day_type == "weekday", ], aes(x = interval, y = steps)) +
  geom_line(color = "blue") +
  labs(title = "Promedio de Pasos por Intervalo de 5 Minutos (Días de Semana)",
       x = "Intervalo de 5 Minutos", y = "Promedio de Pasos") +
  ylim(0, max(average_steps_per_interval$steps))

# Crear gráfico para fines de semana
weekend_plot <- ggplot(average_steps_per_interval[average_steps_per_interval$day_type == "weekend", ], aes(x = interval, y = steps)) +
  geom_line(color = "red") +
  labs(title = "Promedio de Pasos por Intervalo de 5 Minutos (Fines de Semana)",
       x = "Intervalo de 5 Minutos", y = "Promedio de Pasos") +
  ylim(0, max(average_steps_per_interval$steps))

# Combinar ambos gráficos en un solo panel
combined_plot <- grid.arrange(weekday_plot, weekend_plot, ncol = 1)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
# Imprimir el panel combinado
print(combined_plot)
```

```
## TableGrob (2 x 1) "arrange": 2 grobs
##   z     cells    name           grob
## 1 1 (1-1,1-1) arrange gtable[layout]
## 2 2 (2-2,1-1) arrange gtable[layout]
```
