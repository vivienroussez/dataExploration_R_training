require(tidyverse)
require(Rtsne)
require(plotly)

source("ZZ2-data_preparation.R")

tsne <- select(dat_bike_imp,where(is.numeric)) %>%  
  Rtsne(dims=2,max_iter=500,check_duplicates = F)

# tsne$Y %>% as.data.frame() %>% 
#   ggplot(aes(V1,V2)) + geom_point() + theme_minimal() + 
#   labs(title="Low dimensional representation by t-SNE")

### /!\ This clustering algo scales poorly (hierarchical clustering)
cluster <- tsne$Y %>% 
  dist() %>% 
  hclust("single") %>% 
  cutree(14) ## I counted the number of disjoint spaguettis

gg <- tsne$Y %>% as.data.frame() %>% 
  mutate(cluster=as.character(cluster)) %>% 
  ggplot(aes(V1,V2,color=cluster)) + geom_point() + theme_minimal() + 
  labs(title="Identification of clusters")

ggplotly(gg)

# Now view the stats by cluster to understand what is which cluster
## aggregate al numeric colum by cluster (simple mean)
summary <- mutate(dat_bike_imp,cluster=as.character(cluster)) %>% 
  group_by(cluster) %>% 
  summarise(across(where(is.numeric),
                   function(xx) mean(xx,na.rm=T)))

## Visualize (though there's too much variables, we should have selected a bunch of them)
## Not super helpful and super slow to produce
# summary %>% 
#   pivot_longer(-cluster,names_to="variable",values_to="average") %>% 
#   ggplot(aes(cluster,weight=average)) + geom_bar() + facet_wrap(.~variable,scales = "free")

## Better but we see that some feature should have not been integrated in t-sne (NA)
## => re-run the t-sne on a subset of meaningful columns !!!!!!!
## Conclusion : start over again by selecting features
summary %>% 
  pivot_longer(-cluster,names_to="variable",values_to="average") %>% 
  group_by(variable) %>% ### Check the difference with removing this line and the following => unreadable
  mutate(average=scale(average)) %>%  ### Scale by variable to exhibit differences between clusters
  ggplot(aes(cluster,variable,fill=average)) + geom_tile() + 
  scale_fill_continuous(type = "viridis")


