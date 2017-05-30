###############################################################################
###############################################################################
###############################################################################

## zkouším model mnohorozměrné lineární regrese -------------------------------

summary(
    lm(
	    order_delay ~ quantity * durability_days + title +
		transfer_our_price_currency + system_id_country + lang +
		has_FB + has_ISIC + dioptrie + zakriveni + prumer,
		tidy_data
	)
)

xtable(summary(
    lm(
	    order_delay ~ quantity * durability_days + title +
		transfer_our_price_currency + system_id_country + lang +
		has_FB + has_ISIC + dioptrie + zakriveni + prumer,
		tidy_data
	)
))


## ----------------------------------------------------------------------------

###############################################################################

## zkouším model mnohorozměrné lineární regrese, tentokrát ale pouze na
## podmnožině, kde je zpoždění objednávky maximálně tři dny -------------------

summary(
    lm(
	    order_delay ~ quantity * durability_days + title +
		transfer_our_price_currency + system_id_country + lang +
		has_FB + has_ISIC + dioptrie + zakriveni + prumer,
		tidy_data[tidy_data$order_delay <= 3, ]
	)
)

xtable(summary(
    lm(
	    order_delay ~ quantity * durability_days + title +
		transfer_our_price_currency + system_id_country + lang +
		has_FB + has_ISIC + dioptrie + zakriveni + prumer,
		tidy_data[tidy_data$order_delay <= 3, ]
	)
))


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím diagram počtu odeslaných a přečtených teazovacích emailů ----------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("pocty_emailu.jpg", width = 4, height = 5, units = "in", res = 800)

par(mar = c(4, 4, 1, 1))

barplot(
    c(37520, 11362),
	xlab = "teazovací emaily",
	names = c("odeslané", "přečtené"),
	col = c("darkgrey", "lightgrey"),
	ylab = "počet emailů / daný dataset"
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## zkouším rozhodovací stromy -------------------------------------------------

set.seed(2016)

train_set_indices <- sample(levels(tidy_data$user_id), 415, replace = FALSE)

train_set <- tidy_data[tidy_data$user_id %in% train_set_indices, ]
test_set <- tidy_data[!tidy_data$user_id %in% train_set_indices, ]

for(my_dataset_name in c("train_set", "test_set")){
    
	my_dataset <- get(my_dataset_name)
	
	suppressWarnings(my_dataset[, "order_delay"][
	    which(as.numeric(my_dataset[, "order_delay"]) <= 7)
	] <- "7 a méně")
		
    suppressWarnings(my_dataset[, "order_delay"][
	    which(as.numeric(my_dataset[, "order_delay"]) > 7 &
		      as.numeric(my_dataset[, "order_delay"]) <= 30)] <-
	    "8 až 30")
	
	suppressWarnings(my_dataset[, "order_delay"][
	    which(as.numeric(my_dataset[, "order_delay"]) > 30)
	] <- "31 a víc")
	
	my_dataset[, "order_delay"] <- as.factor(
	    as.character(my_dataset[, "order_delay"])
	)
	
	assign(
	    my_dataset_name,
	    my_dataset
	)
    
}



#### pokus o strom ------------------------------------------------------------

my_tree <- tree(
    order_delay ~ .,
    data = train_set[, -c(1, 2, 8, 13, 17, 24)]
)

summary(my_tree)


setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("rozhodovaci_strom.jpg", width = 7, height = 5, units = "in", res = 800)

par(mar = c(1, 1, 1, 1))

plot(my_tree)
text(my_tree)

dev.off()

setwd(mother_working_directory)


## konfuzní matice a přesnost -------------------------------------------------

predict(object = my_tree, newdata = test_set, type = "class")

xtable(
	table(
		test_set$order_delay,
		predict(object = my_tree, newdata = test_set, type = "class")
	)[c(2, 3, 1), c(2, 3, 1)]
)

getMyAccuracy(
    table(test_set$order_delay,
          predict(my_tree, test_set, type = "class"))
)


## ----------------------------------------------------------------------------

###############################################################################

## zkouším neuronovou síť -----------------------------------------------------

my_net <- neuralnet(
    order_delay ~ quantity + durability_days + dioptrie + zakriveni + prumer +
	cylindr + osa + transfer_our_price + transfer_days + loyalty_points,
	data = train_set[complete.cases(train_set), ],
	hidden = 10,
	lifesign = "minimal",
	linear.output = TRUE,
	threshold = 0.1
)


my_prediction <- compute(my_net,
                         test_set[, c(4, 5, 14, 15, 16, 18, 19, 25, 27, 37)][
						     complete.cases(test_set),
						 ])


true <- test_set[, "order_delay"][complete.cases(test_set)]
predicted <- unlist(apply(my_prediction$net.result, 1, which.max))

table(
    true,
	predicted
)



setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("neuronova_sit.jpg", width = 7, height = 5, units = "in", res = 800)

par(mar = c(1, 0, 1, 0))

plot(my_net, rep = "best", information = FALSE, fontsize = 10)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







