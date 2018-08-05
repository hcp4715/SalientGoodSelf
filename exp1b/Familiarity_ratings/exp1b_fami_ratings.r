## About ####
# this script is used for combined the data from exp1a and exp2 to assessing the stability of the effect
# 
# Initializing ####
source('Initial.r')

#
# load data ####
#df1a <- read.csv("exp1a_personaldistance_r.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b_fam <- read.csv("data_familarity_rating_exp1b.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))

# get the meaningful colnames
df1b_names <- read.csv("colnames.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE,na.strings=c("","NA"))
df1b_names$Names <- paste(df1b_names$Label,df1b_names$Dim, sep = '.')
names_new <- df1b_names$Names
names_new <- c("startTime","endTime","finished",names_new)
colnames(df1b_fam) <- names_new

# wide-to-long
df1b.fam_w <- df1b_fam %>% gather(conditions, rating,ShanRen.Faml:LiuMang.Conc, na.rm = FALSE, convert = FALSE)

# split the conditions
library(stringr)

# split by dot
newCond <- str_split_fixed(df1b.fam_w$conditions,"[.]",2)

df1b.fam_w$label <- newCond[,1]
df1b.fam_w$dim   <- newCond[,2]

df1b.fam_w_sum <- summarySEwithin(df1b.fam_w,measurevar = 'rating', withinvar = c('dim','label'),na.rm = TRUE)
df1b.fam_w_sum$dim <- factor(df1b.fam_w_sum$dim, levels = c("Faml","Freq","Conc"))
df1b.fam_w_sum$label <- factor(df1b.fam_w_sum$label, levels = c("ChangRen","ERen","ShanRen","HaoRen","HuaiRen","FanRen","Junzi","LiuMang"))

write.csv(df1b.fam_w_sum,'df1b.fam_w_sum.csv',row.names = F)

df1b_fam_p <- ggplot(data = df1b.fam_w_sum,aes(y = rating, x = label, group = dim,shape = dim, fill = dim)) +
  geom_bar(position = position_dodge(),stat = "identity",colour = "black", size=.3,width = .6) +    # Thinner lines
  geom_errorbar(aes(ymin = rating - se, ymax = rating + se),
                #geom_errorbar(aes(ymin = 1, ymax = 4),
                size = 1,
                width = .2,
                position=position_dodge(.6)) +
  labs(x = 'Label',y = 'ratings') +
  ggtitle("Ratings for each label") +
  coord_cartesian(ylim=c(1,7))+
  scale_y_continuous(breaks = seq(1,7,1),expand = c(0, 0)) +
  scale_fill_grey (start=0.2, end=0.8) +   # using grey scale, start from darker, end to lighter. 
  #theme_classic()
  apatheme
ggsave("df1b_fami_rating.pdf", df1b_fam_p, scale = 1,height = 6, width = 8, dpi = 300, family = "Times")

