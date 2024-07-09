
library(ggplot2)

ggplot()+
  geom_line(aes(x = timestep, y = MgHa, color = as.factor(Unit)), data = MS_cwd_sl)+
  geom_point(aes(x = timestep, y = MgHa, color = as.factor(Unit)), data = FS_cwd_sl)
