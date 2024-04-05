tenis=read.table(file.choose(),header = T , sep=";")
str(tenis)

#data.frame=eşit uznluktaki vektorlerden oluşan liste
#data.frame=list of vectors of equal length
#eğer veri setindeli faktaler ifade olarak cıkarsa nasıl düzeltirim
#if the facts in the data set appear as expressions, how can I fix it?
attach(tenis)
#uyarı! eğer attach ile çağrılan veri seti üzerinde değğişliklik yapılamaz.
#warning! If the data set called with attach cannot be changed.
tenis$havadurumu <- as.factor(tenis$havadurumu)
tenis$derece <- as.factor(tenis$derece)
tenis$nem <- as.factor(tenis$nem)
tenis$ruzgar <- as.factor(tenis$ruzgar)
tenis$tenis_oynanacak_mi<- as.factor(tenis$tenis_oynanacak_mi)
str(tenis)
#özet
#summary
summary(tenis)
#öznitelik 
#attribute
attributes(tenis)
head(tenis)
show(tenis)
View(tenis)

#veri seti faktörlerden oluştuğu için normalirasyon yapılmaz
#normalization is not done because the data set consists of factors
#karar ağacını oluşturalım 
#let's create the decision tree
library(RWeka)
m1<-J48(tenis_oynanacak_mi ~ . , data=tenis)
print(m1)

#istatisksel sonuçları ve confusion matrix: gercek değerler iile tahmini değerlerin karişılaştırılılan taplo
#statistical results and confusion matrix: table comparing actual values with predicted values
summary(m1)

#modelini çizel
#draw your model
plot(m1)

#kuralları yazalım

#1.Eğer dısarısı=güneşli ise ve  nem=normal ise o zaman tenis EVET oynanır 

#2.Eğer dığarısı=güneşli ise ve nem =yüksek ise o zaman tenis HAYIR

#3.Eğer dışarısı =kapalı ise tenis EVET 

#4.Eğer dığarısı = yağışlı ve ruzgar ise tenis HAYIR 

#5.Eğer dısarısı =yağışlı ise ve ruzgar = zayıf ise tenis EVET 

#ongoru yapalım

#let's write the rules

#1.If outside = sunny and humidity = normal then tennis YES can be played

#2.If outside = sunny and humidity = high then tennis is a NO

#3. Tennis if outside = closed YES

#4.If outside = rainy and windy, tennis is NO

#5.If it's rainy outside and the wind = weak, tennis YES

#let's make the prediction

ongoru <- predict(m1) 
ongoru

#gercek veri ile öngüro veriilerini birlikte görelim
#Let's see the real data and prediction data together
data.frame(data=tenis$tenis_oynanacak_mi, Ongoru=ongoru)[1:14,]

#teni vir tane veri geldiğinde sınıflandırması nasıl tahmin edecek bakalım.
#Let's see how #teni will predict the classification when a piece of data arrives.
yeni<- data.frame(havadurumu="Yagisli",derece="Sicak",nem="Yuksek",ruzgar="Zayıf")

yeni2<- data.frame(havadurumu="Kapali",derece="Sicak",nem="Yuksek",ruzgar="Zayıf")

yeni3<- data.frame(havadurumu="Gunesli",derece="Sicak",nem="Yuksek",ruzgar="Guclu")

#şimdi verinin sınıfını tahmini,
#now estimate the class of the data,
predict(m1,yeni)
predict(m1,yeni2)
predict(m1,yeni3)


#modelin karşılık matrisi 
#correspondence matrix of the model
table(tenis$tenis_oynanacak_mi,ongoru)

#isim koyolım 
#let's name it

karisiklilmatrisi <- table(tenis[,5],ongoru)
karisiklilmatrisi

#modeli performansı değerlendirme ölçütleri
#model performance evaluation criteria
#doğru pozisitf
#correct positive
(TP<-karisiklilmatrisi[1])

#yanlış pozitif
#falsepositive
(FP<-karisiklilmatrisi[3])

#yanlış negatif 
#falsenegative
(TN<-karisiklilmatrisi[2])

#doğru negatif 
#truenegative
(FN<-karisiklilmatrisi[4])

#performans değerlendorme olcutleri 
#performance evaluation criteria

paste0("Dogruluk = ",(Dogruluk <- (TP+TN)/sum(karisiklilmatrisi)))

paste0("Hata =",(Hata <- 1-Dogruluk))

#TPR=DUYARLILIK ORANI 
#TPR=SENSITIVITY RATIO
paste0("TPR=",(TPR<-TP/(TP+TN)))
       
#SPC=Belirliyici orani
#SPC=Determinant rate

paste0("SPC=",(SPC<-TN/(FP+TN)))

#PPV= kesikli ya da poziti ongoru degeri
#PPV= discrete or positive predictive value
paste0("PPV=",(PPV<-TP/(TP+FP)))

#NPV= negatif ogoru degeri
#NPV= negative opinion value
paste0("NPV=",(NPV<-TN/(TN+FN)))

#FPR = yanlış pozitif oranı 
#FPR = false positive rate
paste0("FPR=",(FDR<-FP/sum(karisiklilmatrisi)))

#FNR=yanlış negatif oran 
#FNR=false negative rate
paste0("FNP=",(FNP<-FN/(FN+TP)))

#F olcutu kesikli ve duyarlulık olcutlerinin harmonik ortalaması
#F measure is discrete and harmonic mean of sensitivity measures
paste0("F_measure=",(F_measure<-(2*PPV*TPR)/(PPV+TPR)))
