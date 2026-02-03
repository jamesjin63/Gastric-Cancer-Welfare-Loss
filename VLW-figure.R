setwd("/Users/anderson/Desktop/VSL-GBD/Gas/1.0")
df=read.csv("Region21_summary.csv")

# Regional summary (median, lower, upper)
region_summary=df %>% 
  select(1,3) %>%
  extract(Total_VLW_No_Discount_Billion,
          into = c("val", "lower", "upper"),
          regex = "([0-9.]+)\\(([0-9.]+)-([0-9.]+)\\)",
          convert = TRUE)

# Inspect results
head(region_summary)
p1=ggplot(region_summary, aes(x = reorder(region, -val), y = val)) +
  geom_col(fill = "#6a0572", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "", y = "VLW (Billion USD)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


region_summary=df %>% 
  select(1,Weighted_Avg_GDP_Ratio_No_Discount) %>%
  extract(Weighted_Avg_GDP_Ratio_No_Discount,
          into = c("val", "lower", "upper"),
          # Match pattern like 2.428%(2.137–2.719%)
          regex = "([0-9.]+)%\\(([0-9.]+)–([0-9.]+)%\\)",
          convert = TRUE)

p2=ggplot(region_summary, aes(x = reorder(region, -val), y = val)) +
  geom_col(fill = "#ff677d", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "", y = "VLW / GDP (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


p1/p2+ 
  plot_annotation(tag_levels = 'A')

ggsave("21 region.pdf",width = 10,height = 8)







df=read.csv("sdi_summary.csv") %>% rename("region"=1)

# Regional summary (median, lower, upper)
region_summary=df %>% 
  select(1,3) %>%
  extract(Total_VLW_No_Discount_Billion,
          into = c("val", "lower", "upper"),
          regex = "([0-9.]+)\\(([0-9.]+)-([0-9.]+)\\)",
          convert = TRUE)

# Inspect results
head(region_summary)
p1=ggplot(region_summary, aes(x = reorder(region, -val), y = val)) +
  geom_col(fill = "#6a0572", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "", y = "VLW (Billion USD)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


region_summary=df %>% 
  select(1,Weighted_Avg_GDP_Ratio_No_Discount) %>%
  extract(Weighted_Avg_GDP_Ratio_No_Discount,
          into = c("val", "lower", "upper"),
          # Match pattern like 2.428%(2.137–2.719%)
          regex = "([0-9.]+)%\\(([0-9.]+)–([0-9.]+)%\\)",
          convert = TRUE)

p2=ggplot(region_summary, aes(x = reorder(region, -val), y = val)) +
  geom_col(fill = "#ff677d", width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "", y = "VLW / GDP (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


p1/p2+ 
  plot_annotation(tag_levels = 'A')

ggsave("5 SDI.pdf",width = 10,height = 8)





mapfigure1_small=function(GBDdf27=x,titlex="Pre",color_scheme=1){
  #load("Map204.Rdata")
  ################################################################################################
  ### 1. Print table Reginal level
  ################################################################################################
  df= GBDdf27  %>% #filter(location_id %in% namex$location_id ) %>% 
    select(location_id,val) %>% left_join(.,namex)
  color1 <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
              "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac")
  # Define the base colors for each gradient
  base_colors2 <- c("#457B9D", "#800080")
  base_colors3 <- c("#d6604d", "#92c5de")
  base_colors4 <- c("#fddbc7", "#4393c3")
  base_colors5 <- c("#d1e5f0", "#FA8072")
  color1 <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
              "#f7f7f7", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac")
  
  color2 <- c("#6a51a3", "#54278f", "#2c2c92", "#253494", "#225ea8",
              "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4", "#ffffcc")
  
  # Scheme 3: red-orange-green (RdYlGn style, good for reversed trends)
  color3 <- c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee08b",
              "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850", "#006837")
  
  # Reversed color4 (blue-gray-red -> red-gray-blue)
  color4 <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#f7f7f7",
              "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
  
  # Scheme 5: techy blue-purple-orange (ggsci::nejm-inspired)
  color5 <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087",
              "#f95d6a", "#ff7c43", "#ffa600", "#ffd54f", "#dcedc8")
  # Create color gradients using colorRampPalette
  #color2 <- colorRampPalette(base_colors2)(10)
  #color3 <- colorRampPalette(base_colors3)(10)
  #color4 <- colorRampPalette(base_colors4)(10)
  #color5 <- colorRampPalette(base_colors5)(10)
  colorx=list(color1,color2,color3,color4,color5)
  colorx1=colorx[as.numeric(color_scheme)]
  ################################################################################################
  ########################
  # 1.1 Incidence
  ########################
  # Incidence
  index="Incidence"
  ASR=df %>%  select(location_id,location,val) %>% 
    mutate(val=val/1)
  df_asrmap=left_join(mapdf_2024_China,ASR)
  x <- mapdf_2024_China %>% filter(!is.na(location_id))
  df_asr=left_join(mapdf_2024_China,ASR) %>% select(location_id,val) %>% distinct(location_id,val) #%>% filter(location_id %in% x$location_id)
  print(dim(df_asr))
  xmin = min(na.omit(df_asr$val))
  xmax = max(na.omit(df_asr$val))
  # Adjust bounds dynamically based on interval values
  if (xmin > 0) {
    xmin <- xmin - 0.01 * xmin  # If xmin > 0, extend downward by 1%
  } else {
    xmin <- xmin + 0.01 * xmin  # If xmin < 0, extend upward by 1%
  }
  
  if (xmax > 0) {
    xmax <- xmax + 0.01 * xmax  # If xmax > 0, extend upward by 1%
  } else {
    xmax <- xmax - 0.01 * xmax  # If xmax < 0, extend downward by 1%
  }
  dim(df_asr)
  # Calculate the quantile breaks
  breaks <- quantile(df_asr$val, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
  # Ensure breaks are unique and sorted
  values <- sort(unique(breaks))
  # Replace the first and last values with xmin and xmax
  values[1] <- xmin
  values[length(values)] <- xmax
  
  # Generate formatted strings for each interval
  strings <- sapply(1:(length(values)-1), function(i) {
    paste(sprintf("%.2f", values[i]), "-", sprintf("%.2f", values[i+1]), sep=" ")
  })
  # Print the formatted interval strings
  print(strings)
  # Add the group column to dfx based on the intervals
  df_asr$asr_cut <- cut(df_asr$val, breaks = values, labels = strings, include.lowest = TRUE)
  
  df_asrmapx=left_join(df_asrmap,df_asr %>% select(location_id,asr_cut)) %>% 
    filter(!is.na(val))
  
  fig <- ggplot(data = df_asrmapx %>% filter(region != "China")) +
    geom_polygon(aes(long, lat, group = group, fill = asr_cut),
                 colour = "#3A3A3A", size = 0.3) +
    geom_polygon(data = df_asrmapx %>% filter(region == "China"),
                 aes(long, lat, group = group, fill = asr_cut),
                 colour = "#3A3A3A", size = 0.3) +
    # 1) Keep coordinates unexpanded
    coord_quickmap(expand = FALSE) +
    scale_fill_manual(values = rev(colorx1[[1]]),
                      guide = guide_legend(reverse = TRUE)) +
    guides(fill = guide_legend(ncol = 2, title = titlex)) +
    theme_bw() +
    theme(
      axis.title       = element_blank(),
      axis.text        = element_blank(),
      axis.ticks       = element_blank(),
      panel.border     = element_blank(),
      panel.grid       = element_blank(),
      panel.background = element_blank(),
      legend.text      = element_text(size = 10, face = "bold"),
      legend.title     = element_text(size = 12, face = "bold"),
      legend.position  = c(0.14, 0.2),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    )
  
  fxp2<- fig+ labs(x=" ",y="",title="Caribbean and central America")+
    coord_cartesian(xlim = c(-92,-60),ylim = c(5,27),expand = FALSE)+theme(panel.border = element_rect(color='black',fill=NA,size = 0.5))+
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),plot.margin  = margin(0, 0.2, 0,0.2, "cm"))  # Emphasize title
  
  fxp3  <- fig+ labs(x=" ",y="",title="Persian Gulf")+
    coord_cartesian(xlim = c(45,55),ylim = c(19,31),expand = FALSE) +theme(panel.border = element_rect(color='black',fill=NA,size = 0.5))+
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),plot.margin  = margin(0, 0.2, 0,0.2, "cm"))  # Emphasize title
  
  fxp4  <- fig+ labs(x=" ",y="",title="Balkan Peninsula")+
    coord_cartesian(xlim = c(12,32),ylim = c(35,53),expand = FALSE) +theme(panel.border = element_rect(color='black',fill=NA,size = 0.5))+
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),plot.margin  = margin(0, 0.2, 0,0.2, "cm"))  # Emphasize title
  
  fxp5  <- fig+ labs(x=" ",y="",title="Sotheast Asia")+
    coord_cartesian(xlim = c(98,123),ylim = c(-10,8),expand = FALSE) + 
    theme(panel.border = element_rect(color='black',fill=NA,size = 0.5))+
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),plot.margin  = margin(0, 0.2, 0,0.2, "cm"))  # Emphasize title
  
  fxp6  <- fig+ labs(x=" ",y="",title="West Africa") +
    coord_cartesian(xlim = c(-17,-7),ylim = c(7,20),expand = FALSE) + 
    theme(panel.border = element_rect(color='black',fill=NA,size = 0.5))+
    theme(
      panel.border = element_rect(color = 'black', fill = NA, size = 0.5),
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),plot.margin  = margin(0, 0.2, 0,0.2, "cm")  # Emphasize title
    )
  
  fxp7  <- fig+ labs(x=" ",y="",title="Eastern \nMediterranean")+
    coord_cartesian(xlim = c(32,37),ylim = c(29,35),expand = FALSE) + 
    theme(panel.border = element_rect(color='black',fill=NA,size = 0.5))+
    theme(
      panel.border = element_rect(color = 'black', fill = NA, size = 0.5),
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),plot.margin  = margin(0, 0.2, 0,0.2, "cm")  # Emphasize title
    )
  
  fxp8  <- fig+ labs(x=" ",y="",title="Northern Europe",size=4) +
    coord_cartesian(xlim = c(5,25),ylim = c(48,60),expand = FALSE) + 
    theme(panel.border = element_rect(color='black',fill=NA,size = 0.5))+
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),plot.margin  = margin(0, 0.2, 0,0.2, "cm"))  # Emphasize title
  
  A=(fxp6|fxp7)/fxp8
  plot<- fig +(fxp2+fxp3+fxp4+fxp5+A+plot_layout(ncol = 5,widths=c(1.5,1,1.1,1.2,1)))+
    plot_layout(ncol = 1,heights = c(9, 3))
  
  return(plot)
}
load("/Users/anderson/Desktop/big/2024/GBD/0718/new/GBD.Rdata")
library(patchwork)

dfa=read.csv("/Users/anderson/Desktop/VSL/204.csv")
namex <- dfa %>%
  select(location_id, location_name) %>%
  unique() %>%
  mutate(location = location_name)

# Read data
df <- read.csv("Country 204.csv")

# Custom plot save function
dfx <- df %>%
  select(location_id, VLW_No_Discount.billion.) %>%
  extract(VLW_No_Discount.billion.,
          into = c("val", "VLW_no_discount_lower", "VLW_no_discount_upper"),
          regex = "([0-9.]+)\\(([0-9.]+)-([0-9.]+)\\)",
          convert = TRUE) %>% 
  left_join(namex, by = "location_id")

xa <- mapfigure1_small(GBDdf27 = dfx, titlex = "VLW (Billion USD)", color_scheme = 3)
print(xa)

ggsave(
  filename = "204 VLW_no_discount.pdf",
  plot = xa,
  width = 16,
  height = 9
)

# Custom plot save function
dfx <- df %>% 
  select(location_id, GDP_Ratio_No_Discount...) %>%
  extract(GDP_Ratio_No_Discount...,
          into = c("val", "VLW_no_discount_lower", "VLW_no_discount_upper"),
          # Match pattern like 2.428%(2.137–2.719%)
          regex = "([0-9.]+)%\\(([0-9.]+)%?-([0-9.]+)%?\\)",
          convert = TRUE) %>% 
  left_join(namex, by = "location_id")

xa <- mapfigure1_small(GBDdf27 = dfx, titlex = "VLW / GDP (%)", color_scheme = 4)
print(xa)

ggsave(
  filename = "204 VLW_no_discount-GDP.pdf",
  plot = xa,
  width = 16,
  height = 9
)
