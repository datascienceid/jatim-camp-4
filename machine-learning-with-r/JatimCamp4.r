library(twitteR)
library(dplyr)
library(zoo)
library(ggplot2)
library(ggcorrplot)
library(reshape)
#library(VIM)
#library(Hmisc)
#library(mice)
library(datasets)
library(gganimate)
library(gapminder)
library(tidyr)
library(graphics)
library(RCurl)
library(cowplot)
library(forecast)
library(caret)
library(DMwR)
library(MASS)
library(caTools)

#melihat directory anda
getwd()

#Mengimpor Data Ke R#1
#download
download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip", 
              destfile = "C:/Users/user/Documents/household_power_consumption.zip")
#unzip
unzip("C:/Users/user/Documents/household_power_consumption.zip")
#read to R
power = read.table("C:/Users/user/Documents/household_power_consumption.txt", sep=";",
                   header=T, na.strings=c("?",""), stringsAsFactors=FALSE)

#Menampilkan data yang terimpor
View(power)

#Mengimpor data#2 dari local Disk


#tampilkan data telco


#Penjelasan Web Crawling
#Definisikan API anda
consumer_key = 'OlHsruThlEzRyXXXXXXX'
consumer_secret = '0BlKLpggSab5avxoc16A5XXXXXXXXXXXXXXXXXXXXXXXX'
access_token = 'XXXXXXXXXXX-erMkl8PqBDsoeVcMzqD5eKaju9ZcWW90vOMe6qgh' 
access_secret = 'XXXXXXXXXXXXXXNEN634CS0S9rItOaskAg3yQNXH4Pl11'
#Mensetup
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
#Mencari Tweet
tweets = searchTwitter("#AvengersEndgame", n = 1000, lang = "en")
#merubah Tweet ke dalam Dataframe
tweets = twListToDF(tweets)
#Menampilkan data tweet
View(tweets)

#Cleaning Preparing Data
#Memahami data Power
#Melihat struktur data
str(power)
#Menampilkan bagian awal data
head(power)
#Menampilkan bagian akhir data
tail(power)

#Memahami data Telco
#Melihat Struktur data telco

#Menampilkan bagian awal data
head(telco)
#Menampilkan bagian akhir data
tail(telco)

#Cleaning and Preparing Data
#Data Power
#Merubah char ke Date
power$Date = as.Date(power$Date,format="%d/%m/%Y")
#Menambah kolom DateTime
power$Datetime = as.POSIXct(paste(power$Date, power$Time))
#Menambah kolom Month
power$Month = format(power$Date,"%Y-%m")
#Tampilkan Struktur data barunya
str(power)
#Data Telco
telco$************* = as.factor(telco$*************)
str(telco)
#Missing Values Demo
#Buat Data Missing

#Hitung rata-ratanya

#Solusi

#impute missing value
#Hitung nilai untuk imputasi misal median
me = median(nilai,na.rm = TRUE)
#Ganti NA dengan nilai tertentu
nilai[is.na(nilai)] = 999
nilai
#Ganti 999 dengan median
nilai[nilai==999]=me
mean(nilai)
#Missing Values Data Power
#Cara cepat untuk melihat apakah ada missing value di data tersebut
#Menggunakan summary, yang berarti melihat deskripsi keseluruhan data tersebut
summary(power)
#Lihat Nilai Maxnya
max(power$Global_active_power,na.rm = T)
#Hitung Nilai Median
medianpower = median(power$Global_active_power,na.rm=T)
#Rubah Nilai NA kedalam nilai diatas Max bebas
power$Global_active_power[is.na(power$Global_active_power)] = 999
#Rubah Nilai bebas tadi menjadi nilai median
power$Global_active_power[power$Global_active_power==999] = medianpower
#Reactive Power
#Lihat Nilai Maxnya Global reactive power
max(power$Global_reactive_power,na.rm = T)
#Hitung Nilai Median
medianrepower = median(power$Global_reactive_power,na.rm=T)
#Rubah Nilai NA kedalam nilai diatas Max bebas
power$Global_reactive_power[is.na(power$Global_reactive_power)] = 999
#Rubah Nilai bebas tadi menjadi nilai median
power$Global_reactive_power[power$Global_reactive_power==999] = medianrepower
#Voltage
#Lihat Nilai Maxnya
max(power$Voltage,na.rm = T)
#Hitung Nilai Median
medianvoltage = median(power$Voltage,na.rm=T)
#Rubah Nilai NA kedalam nilai diatas Max bebas
power$Voltage[is.na(power$Voltage)] = 999
#Rubah Nilai bebas tadi menjadi nilai median
power$Voltage[power$Voltage==999] = medianvoltage
#Intensity
#Lihat Nilai Maxnya
***(power$*************,na.rm = T)
#Hitung Nilai Median
medianintensity = ******(power$Global_intensity,na.rm=T)
#Rubah Nilai NA kedalam nilai diatas Max bebas
power$Global_intensity[is.na(power$Global_intensity)] = 999
#Rubah Nilai bebas tadi menjadi nilai median
power$Global_intensity[power$Global_intensity==999] = medianintensity
#Sub_Metering_1
#Lihat Nilai Maxnya
max(power$Sub_metering_1,na.rm = T)
#Hitung Nilai mode
modemetering1 = mode(power$Sub_metering_1)
#Rubah Nilai NA kedalam nilai diatas Max bebas
power$Sub_metering_1[is.na(power$Sub_metering_1)] = 999
#Rubah Nilai bebas tadi menjadi nilai mode
power$Sub_metering_1[power$Sub_metering_1==999] = modemetering1
#Sub_Metering_2
#Lihat Nilai Maxnya
max(power$Sub_metering_2,na.rm = T)
#Hitung Nilai mode
modemetering2 = mode(power$Sub_metering_2)
#Rubah Nilai NA kedalam nilai diatas Max bebas
power$Sub_metering_2[is.na(power$Sub_metering_2)] = 999
#Rubah Nilai bebas tadi menjadi nilai mode
power$Sub_metering_2[power$Sub_metering_2==999] = modemetering2
#Sub_Metering_3
#Lihat Nilai Maxnya
max(power$Sub_metering_3,na.rm = T)
#Hitung Nilai Mode
modemetering3 = mode(power$Sub_metering_3)
#Rubah Nilai NA kedalam nilai diatas Max bebas
power$Sub_metering_3[is.na(power$Sub_metering_3)] = 999
#Rubah Nilai bebas tadi menjadi nilai mode
power$Sub_metering_3[power$Sub_metering_3==999] = modemetering3
#Missing value data telco
#Cek missing value dengan summary
summary(is.na(telco))
#Lihat Nilai Maxnya
max(*****$TotalCharges,na.rm = TRUE)
#Hitung Nilai Median
mediantelco = median(telco$************,na.rm=TRUE)
#Rubah Nilai NA kedalam nilai diatas Max bebas
telco$************ [is.na(*****$TotalCharges)] = 99999
#Rubah Nilai bebas tadi menjadi nilai median
telco$TotalCharges[telco$TotalCharges == 99999] = mediantelco
#cek kembali
summary(is.na(telco))

#Data Reshaping Power
#Membentuk grup berdasarkan Bulan
power_group = group_by(power,Month)
#Membentuk data perbulan berisi maksimum pemakaian dan total pemakaian
power_monthly = summarize((power_group),Max_Demand_kW = max(Global_active_power),
                          Total_use_kWh = sum(Global_active_power)/60)
#menghapus partial month dari data frame
power_monthly = power_monthly[2:47,]
#mengkonversi month ke data
power_monthly$Month = as.Date(paste0(power_monthly$Month,"-01"))
#cek data yang sudah direshape
head(power_monthly)

#Data visualization
#Histogram dengan data mpg
data("mpg")


#Barplot dengan data mtcars
data("mtcars")

#Lineplot dengan data sendiri
df <- data.frame(time=c("breakfeast", "Lunch", "Dinner"),
                 bill=c(10, 30, 15))
ggplot(data=df, aes(x=time, y=bill, group=1)) + geom_line()+ geom_point()
#pie chart dengan data sendiri
df <- data.frame(group = c("Male", "Female", "Child"),value = c(25, 25, 50))
#Buat barplot definisikan sebagai misal bp
bp<- ggplot(df, aes(x="", y=value, fill=group))+  geom_bar(width = 1, stat = "identity")
#buat piechartnya
bp + coord_polar("y", start=0)
#boxplot dengan data toothgrowth
data("ToothGrowth")


#Plot korelasi dengan data mtcars
#menghitung matrix korelasi

#membuat plot korelasi

#Plot Bergerak
#Pertama buat plot biasa definisikan misal p
p <- ggplot(gapminder,
  aes(x=gdpPercap,y=lifeExp,size=pop,colour=country)) +
  geom_point(show.legend=F,alpha=0.7)+
  scale_color_viridis_d()+
  scale_size(range=c(2,12)) +
  scale_x_log10()+
  labs(x="GDP per Capita",y="Life Expectancy")
#Tampilkan P
p
#Buat p bergerak
p + transition_time(year) + 
  labs(title="Year: {frame_time}")

#Data Visualisasi Power
#Time Series Plot Total Use kWh


#Time Series Plot Max Demand kWh
ggplot(power_monthly,aes(Month, **********)) + 
  geom_line(col="blue",lwd=1) + labs(y="Max Demand kW",x="Time")
#Data visualisasi Telco
#1.Tampilkan jumlah pelanggan yang beralih dan tidak


#2.Tampilkan proporsinya

#3.Hubungan Churn Rate dengan Jenis Kelamin


#4.Hubungan Churn Rate dengan Senior Citizen
ggplot(telco,aes(x=*************,fill=Churn)) + ****_***() +
  geom_text(aes(label=..count..),stat='count',position=position_stack(0.5),size=5)+
  labs(y="Customer Count",title="Churn Rate by Senior Citizen",size=5)
#5.Tulis Code Untuk Menampilkan Hubungan Churn Rate dengan Partner



#6.Multiplot
#definisikan theme1 untuk merapikan label plot nanti
theme1 = theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
#atur grid
options(repr.plot.width = 12, repr.plot.height = 8)
#bentuk plot
plot_grid(ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar()+theme1, 
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),align = "h")
#7.Multiplot Variabel yang tersisa







#8.Hubungan Churn Rate Dengan Tenure
ggplot(telco, aes(y= tenure, x = "", fill = Churn)) + geom_boxplot()+ theme_bw()+ xlab(" ")
#9.Hubungan Churn Rate Dengan Total Charges
ggplot(telco, aes(y= TotalCharges, x = "", fill = Churn)) + geom_boxplot()+ theme_bw()+ xlab(" ")
#10.Tulis Kode untuk menampilkan hubungan churn rate dengan monthly charge

#Analytic
#Power
#Melakukan agregate

#Meramalkan

#Melihat hasil  model yang terbentuk
  
#Menampilkan plot
plot(total_use_fc) 
#Analytic Telco
#Jangan Lupa Menghilangkan variabel ID
telco = telco[,-1]
#Logistik Regression Model Awal
modelawal = glm(Churn~.,family=binomial,data=telco)
#Lihat Hasil Model
summary(modelawal)

#Deepdown ML
#Membersihkan Kategorikal Feature
#Merubah No internet service menjadi no


#Merubah No phone service menjadi No


#Cek Apakah sudah terganti Gunakan salah satu saja, misal online security

#Standardizing
#Memisah Feature/Variabel Numeric kedalam dataframe baru

#Menggabungkan Numeric Feat kedalam data telco

#Membuat Dataframe telco_int

#Melakukan standardize

#tampilkan Telco_int

#Membuat Feature Baru Berdasarkan Feature yang ada
telco = mutate(telco,tenure_bin = tenure)
telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 73] <- '5-6 years'
telco$tenure_bin <- as.factor(telco$tenure_bin)
#Tampilkan Barchart Variabel Baru
ggplot(telco, aes(tenure_bin, fill = tenure_bin)) + geom_bar()+ theme_bw()
#Membuat Dummy Variabel
#Buat data frame untuk variabel yang kategori
telco_cat = telco[,-c(5,18,19)]
#Buat Variabel Dummynya
dummy = data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
#Tampilkan
View(dummy)
#Membentuk Data Final atau memerging data
#merging data

#Merubah Churn Sebagai Factor

#Tampilkan struktur datanya


#Oversampling Undersampling

#tampilkan barplotnya


#Bagi Data Training dan Testing
indices = sample.split(telco_final$Churn, SplitRatio = 0.8)
train = telco_final[indices,]
test = telco_final[!(indices),]

#Tulis Kode untuk membuat model dengan data train dan semua variabel
model1 = glm(*****~.,train,family = binomial)
summary(model1)
#Pemilihan Feature dengan stepaic
model2 = stepAIC(model1,direction="both")

#Model 2
model2 = glm(Churn ~ tenure + MonthlyCharges + TotalCharges + Partner + Dependents + 
                PhoneService + MultipleLines + InternetService.xFiber.optic + 
                InternetService.xNo + OnlineSecurity + OnlineBackup + DeviceProtection + 
                TechSupport + StreamingTV + StreamingMovies + Contract.xOne.year + 
                Contract.xTwo.year + PaperlessBilling + PaymentMethod.xElectronic.check + 
                PaymentMethod.xMailed.check + tenure_bin.x2.3.years + tenure_bin.x3.4.years + 
                tenure_bin.x4.5.years + tenure_bin.x5.6.years,train,family=binomial)
summary(model2)

#Model 3
model3 = glm(Churn ~ tenure + MonthlyCharges + TotalCharges + Partner + Dependents + 
                PhoneService  + InternetService.xFiber.optic + 
                InternetService.xNo + OnlineSecurity + OnlineBackup + DeviceProtection + 
                TechSupport + StreamingTV + StreamingMovies + Contract.xOne.year + 
                Contract.xTwo.year + PaperlessBilling + PaymentMethod.xElectronic.check + 
                PaymentMethod.xMailed.check + tenure_bin.x2.3.years + tenure_bin.x3.4.years + 
                tenure_bin.x4.5.years + tenure_bin.x5.6.years,train,family='binomial')
summary(model3)
#Model Final
final_model = model3
#Hitung Prediksi atau Yhat
pred = predict(final_model,type='response',newdata = test[,-24])
#buat feature pred pada data test
test$prob = pred
#Lakukan prediksi berdasarkan cutoff 0.5
pred_churn = factor(ifelse(pred>=0.50,"Yes","No"))
#Buat data actual churn
actual_churn = factor(ifelse(test$Churn==1,"Yes","No"))
#buat table klasifikasi
t = table(pred_churn,actual_churn)
#tampilkan table
t
#Hitung Cutoff

#buat confusion matrix

#tampilkan confusion matrix
