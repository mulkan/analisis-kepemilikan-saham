library(dplyr)
library(pyramid)
library(readr) #untuk parsing number
library(lubridate) #untuk parsing date
library(icesTAF) #untuk mkdir




plot.piramid<-function(emiten,lokasi.file){
    emiten  = toupper(emiten)
    d = read.csv(lokasi.file,sep='|')
    
    dat = d%>%filter(Code==emiten)
    print(head(dat,5))
    saham.public = dat$Total+dat$Total.1
    print(saham.public)
    porsi.asing = round((dat$Total.1/saham.public)*100,digits=2)
    porsi.lokal = round((dat$Total/saham.public)*100,digits=2)
    
    lokal = c(unlist(dat[6:14], use.names = FALSE))/saham.public
    asing = c(unlist(dat[16:24], use.names = FALSE))/saham.public
    kategori = c("asuransi","korporasi","dana pensiun","private bank","retail","reksadana","sekuritas","yayasan","lainnya")
    print(lokal)
    print(asing)
    print(kategori)
    sebaran.saham = data.frame(lokal*100,asing*100,kategori)
    
    
    ##gunakan readr untuk parsing number
    ##gunakana lubridate untuk urusan date
    tahun = year(ymd(parse_number(lokasi.file)))
    bulan = month(ymd(parse_number(lokasi.file)))
    BULAN = c("jan","feb","mar","apr","mei","juni","jul","agt","sep","okt","nov","des")
    f = formatC(saham.public,format="f",big.mark = ",",digits=0)                                            
    
    pyramid(sebaran.saham, 
            Clab ='Jenis', 
            Llab = paste('Lokal',porsi.lokal,"%",sep=" "),
            Rlab = paste('Asing',porsi.asing,"%",sep=" "),
            Cgap=0.2,  Cadj = 0, Ldens=-1, AxisFM='d', AxisBM=".", 
            GL=FALSE, main=paste('Data Kepemilikan Efek Public',emiten,":",toupper(BULAN[bulan]),"-",tahun," saham ",f,sep=" "))
}


pilih.dataset<-function(TAHUN,BULAN){
    folder = "dataset"
    files<-list.files(path=paste(getwd(),folder,sep='/'),pattern='*.txt')
    file = data.frame(files)
    file = file %>% mutate(tahun=year(ymd(parse_number(files))),bulan=month(ymd(parse_number(files))))
    file.selection = file%>%filter(tahun==TAHUN & bulan==BULAN)
    lokasi.file = paste(folder,file.selection$files,sep="/")
    print(lokasi.file)
    return(lokasi.file)
}

#buat folder
result = "out"
icesTAF::mkdir(result)
tahun = 2022
emiten = "BBRI"
for (bulan in c(6)){
    jpeg(file=paste(result,"/",emiten,"-",bulan,".jpg",sep=""),width=1200, height=600)
    plot.piramid(emiten,pilih.dataset(tahun,bulan))
    dev.off()
}





