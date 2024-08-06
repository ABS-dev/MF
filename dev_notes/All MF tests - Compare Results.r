library(MF)
setwd("~/CVB Packages/")
load("test.3.5.3.rdata")
load("test.4.0.3.rdata")

test1.3$HLstat      	==	test1.4$HLstat      
test1.3$MFstat      	==	test1.4$MFstat      
test1.3$QDIFstat    	==	test1.4$QDIFstat    
test1.3$QXstat      	==	test1.4$QXstat      
test1.3$QYstat      	==	test1.4$QYstat      
test1.3$alpha       	==	test1.4$alpha       
test1.3$compare     	==	test1.4$compare     
test1.3$nboot       	==	test1.4$nboot       
test1.3$rng         	==	test1.4$rng         
test1.3$seed        	==	test1.4$seed        

test2.3$alpha == test2.4$alpha
test2.3$compare == test2.4$compare
test2.3$nboot == test2.4$nboot
test2.3$rng == test2.4$rng
test2.3$seed == test2.4$seed
test2.3$stat == test2.4$stat

test3.3$All == test3.4$All
test3.3$byCluster == test3.4$byCluster
test3.3$call == test3.4$call
test3.3$compare == test3.4$compare
test3.3$excludedClusters == test3.4$excludedClusters

test4.3$All == test4.3$All
test4.3$alpha == test4.3$alpha
test4.3$call == test4.3$call
test4.3$compare == test4.3$compare
test4.3$excludedClusters == test4.3$excludedClusters
test4.3$nboot == test4.3$nboot
test4.3$rng == test4.3$rng
test4.3$seed == test4.3$seed
test4.3$stat == test4.3$stat
test4.3$what == test4.3$what


#test5...

n = names(test7)
n = sort(n[!grepl("\\.", n)])
cat(paste0("test7.3$", n, " == ", "test7.3$", n, "\n"))
