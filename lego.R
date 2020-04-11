library(dplyr)
library(stringr)
sets = read.csv('sets.csv')%>% mutate(set_num=as.character(set_num))
inventories = read.csv('inventories.csv') %>% mutate(set_num=as.character(set_num))
inventory_parts = read.csv('inventory_parts.csv') %>% mutate(part_num=as.character(part_num))

# List set number of existing sets
mysets = c(60243,
           31034,
           60222,
           60214,
           60110,
           60212,
           75890,
           31075,
           60243,
           60181,
           30359,
           10706)

# retrive part numbers from all sets
mysetall = as_tibble(list(setname=character(0)))
for (i in 1:length(mysets)){
  wid = inventories %>% filter(str_detect(set_num,paste('^',as.character(mysets[i]),sep=""))) 
  tmp =inventory_parts %>% filter(inventory_id == wid$id)
  tmp$setname=wid$set_num
  mysetall = bind_rows(mysetall,tmp)
}

# summarise parts
part_summary = mysetall %>% group_by(part_num) %>% summarise(n=sum(quantity))

# check overlap of pieces with other sets
wid = inventory_parts %>% distinct(inventory_id)
for (i in 1:dim(wid)[1]){
  tmp = inventory_parts %>% filter(inventory_id==wid$inventory_id[i]) %>% left_join(part_summary,by=c("part_num"="part_num"))
  tmp2 = tmp$quantity-tmp$n
  tmp2[tmp2<0] = 0
  tmp2[is.na(tmp2)] = as.numeric(tmp$quantity[is.na(tmp2)])
  tmp$final = tmp2
  wid[i,'complete']=1-sum(tmp$final)/sum(tmp$quantity)
  print(i)
}

# list all sets by overlap with your pieces in percent (complete column)
wid = wid %>% arrange(desc(complete))
wid = wid %>% left_join(inventories,by=c("inventory_id"="id"))
wid = wid %>% left_join(sets, by=c("set_num"="set_num"))

