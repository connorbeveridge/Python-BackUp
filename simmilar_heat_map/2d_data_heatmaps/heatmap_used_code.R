
library(ggplot2)
library(reshape2)

df_expression <- read.csv("bms_incyte_similarities1024.csv", sep = " " ,header = FALSE)


#dat <- df_expression[,1:88]  # numerical columns
dat <- df_expression

row.order <- hclust(dist(dat))$order # clustering
col.order <- hclust(dist(t(dat)))$order
dat_new <- dat[row.order, col.order] # re-order matrix accoring to clustering

df_molten_dat <- melt(as.matrix(dat_new)) # reshape into dataframe
names(df_molten_dat)[c(1:2)] <- c("Incyte", "BMS")

write.csv(df_molten_dat, 'df_molten_dat.csv')

ggplot(data = df_molten_dat,
       aes(x = BMS, y = Incyte, fill = value)) + 
  geom_raster() +
  scale_fill_distiller(palette = "RdYlBu", trans = "log10") +
  ggtitle("Tanimoto Similarities of BMS Molecules")


#  theme(axis.text.x = element_text(angle = 90, hjust = 1),
#axis.text.y = element_blank()) + 