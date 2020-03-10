library(ggplot2)
library(splines)
library(gridExtra)
Spotify <- read_excel("edited_spotify.xlsx")

Spotify$TrackDanceability[which(Spotify$TrackDanceability==0)] = 1e-6


# Step 2

myplot1 <- ggplot(Spotify, aes(x = TrackValence, y = TrackDanceability)) + 
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) + 
  ggtitle("Linear regression fit") +
  theme_bw()

myplot2 <- ggplot(Spotify, aes(x = TrackValence, y = TrackDanceability)) + 
  geom_point() +
  geom_smooth(method = 'glm', formula = y ~ x, se = FALSE,
              method.args = list(
                family = quasi(link = "log") )) + 
  ggtitle("GLM fit with log link") +
  theme_bw()

gy_logit <- betareg(TrackDanceability ~ TrackValence, data = Spotify)
myplot3 <- ggplot(Spotify, aes(x = TrackValence, y = TrackDanceability)) +
  geom_point() +
  geom_line(aes(y = predict(gy_logit, Spotify)),color=c("#3366FF"),size=1.1) +
  ggtitle("Beta regression fit with logit link") +
  theme_bw()



# Step 3
scatter_plot <- ggplot(Spotify, aes(x = TrackValence,
                                    y = TrackDanceability, color=TrackSpeechiness>0.33)) +
  geom_point() +
  ggtitle("") +
  theme_bw() + 
  geom_smooth()

model1 <- glm(TrackDanceability ~ TrackValence, data = Spotify, family = quasi(link = "log",
                                                                                   variance = "mu^2"))

model2 <- glm(TrackDanceability ~ TrackValence + as.numeric(TrackSpeechiness>0.33),
                  data = Spotify, family = quasi(link = "log", variance = "mu^2"))

model3 <- glm(TrackDanceability  ~ -1 + TrackValence + factor(TrackSpeechiness > 0.33)
                                                                 + factor(TrackSpeechiness>0.33):TrackValence, data = Spotify,
                  family = quasi(link = "log", variance = "mu^2"))


MyData = {Spotify %>%
    group_by(Artist, AlbumName, AlbumReleaseDate) %>%
    summarize(AlbumValence = mean(TrackValence),
              AlbumDanceability = mean(TrackDanceability),
              AlbumSpeechiness = mean(TrackSpeechiness))}
MyDFData = as.data.frame(MyData)
MyDFData[,4:6] <- scale(MyDFData[,4:6])
row.names(MyDFData) = paste(MyDFData$Artist, MyDFData$AlbumName)

res.hc <- hclust(dist(MyDFData[,4:6]), method = "ward.D2")
library(factoextra)
fviz_dend(res.hc, cex = 0.2, k = 20, palette = "jco", labels_track_height = 10)


model1 <- glm(TrackDanceability ~ TrackValence, data = Spotify, family = quasi(link = "log",
                                                                               variance = "mu^2"))

plot1 <- ggplot(Spotify, aes(x = TrackValence, y  = TrackDanceability)) +
  geom_smooth(method = 'glm', formula = y ~ x,
              method.args = list(
                family = quasi(link = "log", variance = "mu^2"))) +
  ylab("Time spent looking after family members (minutes)") +
  ggtitle("GLM fit with log link, multiplicative error, and B-spline")

model2 <- glm(TrackDanceability ~ TrackValence + as.numeric(TrackSpeechiness>0.33),
              data = Spotify, family = quasi(link = "log", variance = "mu^2"))

plot2 <- ggplot(Spotify) +
  geom_smooth(data=(model2), aes(x = Spotify$TrackValence, y = model2$fitted.values, color=Spotify$TrackSpeechiness>0.33)) + 
  ylab("Time spent looking after family members (minutes)") +
  ggtitle("GLM fit with log link, multiplicative error, and B-spline")

model3 <- glm(TrackDanceability  ~ -1 + TrackValence + factor(TrackSpeechiness > 0.33)
              + factor(TrackSpeechiness>0.33):TrackValence, data = Spotify,
              family = quasi(link = "log", variance = "mu^2"))
plot3 <- ggplot(Spotify) +
  geom_smooth(data=(model3), aes(x = Spotify$TrackValence, y = model3$fitted.values, color=Spotify$TrackSpeechiness>0.33)) +
  ylab("Time spent looking after family members (minutes)") +
  ggtitle("GLM fit with log link, multiplicative error, and B-spline")



model1 <- glm(TrackDanceability ~ TrackValence, data = Spotify, family = quasi(link = "log",
                                                                               variance = "mu^2"))


model2 <- glm(TrackDanceability ~ TrackValence + as.numeric(TrackSpeechiness>0.33),
              data = Spotify, family = quasi(link = "log", variance = "mu^2"))

model3 <- glm(TrackDanceability  ~ -1 + TrackValence + factor(TrackSpeechiness > 0.33)
              + factor(TrackSpeechiness>0.33):TrackValence, data = Spotify,
              family = quasi(link = "log", variance = "mu^2"))

plot1 <- ggplot(Spotify, aes(x = TrackValence, y  = TrackDanceability)) +
  geom_smooth(method = 'glm', formula = y ~ x, method.args = list(family = quasi(link = "log", variance = "mu^2"))) +
  ylab("TrackDanceability") + xlab("TrackValence")


plot2 <- ggplot(Spotify) + geom_smooth(data=(model2), aes(x = Spotify$TrackValence, y = model2$fitted.values, color=Spotify$TrackSpeechiness>0.33))+ 
  ylab("TrackDanceability") + theme(legend.position = "top") + xlab("TrackValence") + scale_fill_discrete(name = "TrackSpeechiness > 0.33")


plot3 <- ggplot(Spotify) +
  geom_smooth(data=(model3), aes(x = Spotify$TrackValence, y = model3$fitted.values, color=Spotify$TrackSpeechiness>0.33)) +
  ylab("TrackDanceability")  +  theme(legend.position = "top") + xlab("TrackValence") + + scale_fill_discrete(name = "TrackSpeechiness > 0.33")

grid.arrange(plot1, plot2, ncol =2)
plot3