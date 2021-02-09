library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(leaflet)
library(rvest)
library(geojsonio)
library(ggrepel)
library(rsconnect)

hist_orders <- read_csv(file.choose())
hist_trans <- read_csv(file.choose())
rates <- read_csv(file.choose())

colnames(hist_orders)
hist_orders %>%
  group_by(Supplier, `Product Id`) %>%
  summarize(sum.total_orders = sum(`Order Qty`),
            sum.total_weight = sum(`Order Weight`),
            count = n())
#Hammer orders are predicted to be abouth the same as wrench orders
#need to figure out how much it cost last year for each product with each carrier to each store
#need to do specific cost for each order
transportation_costs <- hist_trans %>%
  mutate(cost.with.X = ifelse(hist_trans$`Str Nbr` == "1" & hist_trans$Supplier == "A",
                              2540,
                              ifelse(hist_trans$`Str Nbr` == "2" & hist_trans$Supplier == "A",
                                     1640,
                                     ifelse(hist_trans$Supplier == "B",
                                            1200,
                                            0)))) %>%
  mutate(cost.with.y = ifelse(hist_trans$`Str Nbr` == "1" & hist_trans$Supplier == "A",
                              Weight*.12,
                              ifelse(hist_trans$`Str Nbr` == "2" & hist_trans$Supplier == "B",
                                     Weight*.04,
                                     Weight*.07))) %>%
  mutate(cheaper = ifelse(cost.with.y < cost.with.X,
                          cost.with.y,
                          cost.with.X)) #this adds a column for the cheapest option

#this graph groups by shipping date to get the weeks'orders, sums upp all orders for that day, and plots them.
#graph is weekly transportation cost for shipments. 
transportation_costs %>%
  group_by(`Shipment Date`) %>%
  summarize(shipping.cost.per.week = sum(cheaper)) %>%
  ggplot(aes(x = `Shipment Date`,
              y = shipping.cost.per.week)) +
  geom_point()
#orders increase 10%
projected.orders <- hist_orders %>%
  mutate(next.yr.orders = `Order Qty`* 1.1,
         next.yr.weight = next.yr.orders * `Product Weight`)
#cost of hammers for supplier A and B
projected.orders %>%
  filter(`Product Id` == "1") %>%
  summarize(hammer.costA = sum(next.yr.orders)*.80,
            hammer.costB = sum(next.yr.orders)*.82,
            hammer.costA.perwk = hammer.costA / 52,
            hammer.costB.perwk = hammer.costB /52)
#total cost of hammer production with total order weight
#question: to calculate shipping, do I need to add the weight of each hypothetical order of hammers to the total weight of the order?

projected.orders %>%
  filter(`Product Id` == "1") %>%
  mutate(order.hammer.weight = next.yr.orders * 2) %>% #this is how much each order will weigh
  summarize(total.hammer.weight = sum(order.hammer.weight),
            amt.shipped.per.week = total.hammer.weight/52)
#this gives the projected weight of orders for next year, and which carrier will be cheapest in a given week. 
projected_transportation <- transportation_costs %>%
  mutate(new_weight = Weight + (17374/4)) %>%
  mutate(newcost.with.X = ifelse(hist_trans$`Str Nbr` == "1" & hist_trans$Supplier == "A",
                              2540,
                              ifelse(hist_trans$`Str Nbr` == "2" & hist_trans$Supplier == "A",
                                     1640,
                                     ifelse(hist_trans$Supplier == "B",
                                            1200,
                                            0)))) %>%
  mutate(newcost.with.y = ifelse(hist_trans$`Str Nbr` == "1" & hist_trans$Supplier == "A",
                              new_weight*.12,
                              ifelse(hist_trans$`Str Nbr` == "2" & hist_trans$Supplier == "B",
                                     new_weight*.04,
                                     new_weight*.07))) %>%
  mutate(newcheaper = ifelse(newcost.with.y < newcost.with.X,
                          newcost.with.y,
                          newcost.with.X)) %>%
  mutate(which.carrier = ifelse(newcost.with.y < newcost.with.X,
                                "Y",
                                "X"))
#supplier A
supplierA <- projected_transportation[,c(1:4,6)]
supplierA <- supplierA %>%
  filter(Supplier == "A") %>%
  mutate(new_weight = Weight + (17374/2)) %>% #divied by 2 because each week has two shipments from store 2
  mutate(newcost.with.X = ifelse(`Str Nbr` == "1" & Supplier == "A",
                                 2540,
                                 ifelse(`Str Nbr` == "2" & Supplier == "A",
                                        1640,
                                        ifelse(Supplier == "B",
                                               1200,
                                               0)))) %>%
  mutate(newcost.with.y = ifelse(`Str Nbr` == "1" & Supplier == "A",
                                 new_weight*.12,
                                 ifelse(`Str Nbr` == "2" & Supplier == "B",
                                        new_weight*.04,
                                        new_weight*.07))) %>%
  mutate(newcheaper = ifelse(newcost.with.y < newcost.with.X,
                             newcost.with.y,
                             newcost.with.X)) %>%
  mutate(which.carrier = ifelse(newcost.with.y < newcost.with.X,
                                "Y",
                                "X"))
supplierA %>%
  summarize(tranportation.new.cost.withA = sum(newcheaper))

#supplier B
supplierB <- projected_transportation[,c(1:4,6)]
supplierB <- supplierB %>%
  
  filter(Supplier == "B") %>%
  mutate(new_weight = Weight + (17374/2)) %>% #divied by 2 because each week has two shipments from store 2
  mutate(newcost.with.X = ifelse(`Str Nbr` == "1" & Supplier == "A",
                                 2540,
                                 ifelse(`Str Nbr` == "2" & Supplier == "A",
                                        1640,
                                        ifelse(Supplier == "B",
                                               1200,
                                               0)))) %>%
  mutate(newcost.with.y = ifelse(`Str Nbr` == "1" & Supplier == "A",
                                 new_weight*.12,
                                 ifelse(`Str Nbr` == "2" & Supplier == "B",
                                        new_weight*.04,
                                        new_weight*.07))) %>%
  mutate(newcheaper = ifelse(newcost.with.y < newcost.with.X,
                             newcost.with.y,
                             newcost.with.X)) %>%
  mutate(which.carrier = ifelse(newcost.with.y < newcost.with.X,
                                "Y",
                                "X"))
supplierB %>%
  summarize(tranportation.new.cost.withB = sum(newcheaper))

         
#table
table <- read_csv("final.csv")
table1 <- table[1:3,]
as.table(as.matrix(table1))
#cheapest carrier by week ofr supplier B
x <- supplierB %>%
  group_by(`Shipment Date`) %>%
  mutate(cheapest.carrier = ifelse(sum(newcost.with.X) < sum(newcost.with.y),
                                   "X",
                                   "Y"))

x$`Shipment Date` <- as.Date(x$`Shipment Date`, format="%m/%d/%Y")

x %>%
  ggplot(aes(x = `Shipment Date`,
             y = newcheaper)) +
  geom_point(aes(shape = factor(`Str Nbr`), 
                 color = which.carrier)) +
  theme(axis.text.x=element_blank()) +
  xlab("Shipment Week") +
  ylab("Cost in $") +
  labs(shape = "Store Number", color = "Carrier")
#need to figure out average hammer order per week; how much will that weight? so take the total 
  #amount of wrenches ordered over the year, then divide that up to get an average amount of hammers each week; 
  #then see how much that will cost for each supplier, then choose cheapest of the two

