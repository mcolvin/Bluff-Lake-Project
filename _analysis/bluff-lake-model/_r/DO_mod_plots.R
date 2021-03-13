Fish<-read.csv("_dat/combos-fast2.csv")

Fish2<-Fish%>%dplyr::group_by(elevation, tempC)%>%dplyr::summarize(Fish5=mean(Vol5))

ggplot(Fish2, aes(elevation, Fish5/1000000, group=tempC, color=tempC)) + geom_line()+
  labs(y = bquote('Water Volume'~('million'~m^3)), x = "Elevation")+   
  theme_classic()+ylim(0,10)+theme(axis.title.x=element_blank(), text = element_text(size=8))

Fish3<-Fish%>%dplyr::group_by(elevation, DO_dusk)%>%dplyr::summarize(Fish5=mean(Vol5))

ggplot(Fish3, aes(elevation, Fish5/1000000, group=DO_dusk, color=DO_dusk)) + geom_line()+
  labs(y = bquote('Water Volume'~('million'~m^3)), x = "Elevation")+   
  theme_classic()+ylim(0,10)+theme(axis.title.x=element_blank(), text = element_text(size=8))
#theme(legend.position = "none")++annotate(geom="text", x=64.5, y=10,size=3,label="E") 
#+annotate(geom="text", x=64.5, y=10,size=3,label="E") 