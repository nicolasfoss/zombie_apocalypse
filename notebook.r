
# Read in the data
zombies <- read.csv("datasets/zombies.csv")

# Examine the data with summary()
summary(zombies)

# Create water-per-person
zombies$water.person <- zombies$water / zombies$household

# Examine the new variable 
summary(zombies$water.person)

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

test_zombies <- read.csv("datasets/zombies.csv")
test_zombies$water.person <- test_zombies$water / test_zombies$household

run_tests(
    test_that("the dataset is correct", {
              expect_identical(zombies, 
                               test_zombies,
                               info = "The data frame is not correct. Did you load it correctly and divide water by household?")
              })
)

# Load ggplot2 and gridExtra
library(ggplot2) 
library(gridExtra)

# Create the ageZombies graph
ageZombies <- ggplot(data = zombies, aes(x = age, fill = zombie)) +
  geom_density(alpha = 0.3) +  
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Create the waterPersonZom graph
waterPersonZom <- ggplot(data = zombies, aes(x = water.person, fill = zombie)) +
  geom_density(alpha = 0.3) +  
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Display plots side by side
grid.arrange(ageZombies, waterPersonZom, ncol = 2)

# Create the test ageZombies graph
test_ageZombies <- ggplot(data = test_zombies, aes(x = age, fill = zombie)) +
  geom_density(alpha = 0.3) + 
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Create the test waterPersonZom graph
test_waterPersonZom <- ggplot(data = test_zombies, aes(x = water.person, fill = zombie)) +
  geom_density(alpha = 0.3) + 
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

run_tests({
    test_that("packages are loaded", {
        expect_true("ggplot2" %in% .packages(), info = "Did you load the ggplot2 package?")
        expect_true("gridExtra" %in% .packages(), info = "Did you load the gridExtra package?")
})
    
    test_that("ageZombie is correct", {
        expect_identical(ageZombies$data,
                         test_ageZombies$data,
                         info = 'The data used in ageZombie is incorrect. Did you use zombies?')
        expect_identical(deparse(ageZombies$mapping$x),
                         deparse(test_ageZombies$mapping$x),
                         info = 'The x aesthetic in ageZombie is incorrect. Did you use age?')
        expect_identical(ageZombies$layers[[1]]$aes_params$alpha, 
                         test_ageZombies$layers[[1]]$aes_params$alpha,
                         info = "alpha is incorrect. Please check its value.")
})
    
    test_that("waterPersonZom is correct", {
        expect_identical(waterPersonZom$data,
                         test_waterPersonZom$data,
                         info = 'The data used in waterPersonZom is incorrect. Did you use zombies?')
        expect_identical(deparse(waterPersonZom$mapping$x),
                         deparse(test_waterPersonZom$mapping$x),
                         info = 'The x aesthetic in ageZombie is incorrect. Did you use water.person?')
        expect_identical(deparse(waterPersonZom$mapping$fill),
                         deparse(test_waterPersonZom$mapping$fill),
                         info = 'The fill aesthetic in ageZombie is incorrect. Did you map it to zombie?')
        expect_identical(waterPersonZom$layers[[1]]$aes_params$alpha, 
                         test_waterPersonZom$layers[[1]]$aes_params$alpha,
                         info = "alpha is incorrect. Please check its value.")
        expect_identical(waterPersonZom$theme,
                         test_waterPersonZom$theme,
                         info = "The theme is not correct. Please check it.")
    })
})

# Make a subset of the zombies data with only factors
zombies.factors <- zombies[ , sapply(zombies, is.factor)]

# Write a function to get percent zombies
perc.zombies <- lapply(zombies.factors, 
                       function(x){ 
                           return(prop.table(table(x, zombies.factors$zombie),
                                             margin = 1))
                           })
# Print the data
perc.zombies

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# Make a subset of data with only factors
test_zombies.factors <- test_zombies[ , sapply(test_zombies, is.factor)]

# Write a function to get percent zombies
test_perc.zombies <- lapply(test_zombies.factors, 
                       function(x){ 
                           return(prop.table(table(x, test_zombies.factors$zombie),
                                                 margin = 1))
                           })

run_tests({
    test_that("the subset is correct", {
        expect_identical(zombies.factors,
                         test_zombies.factors,
                         info = "The zombies.factors subset is incorrect. Did you supply the zombies data frame name to the sapply command?"
        )
    })
    
    test_that("the function is correct", {
        expect_equal(perc.zombies,
                         test_perc.zombies,
                         info = "The perc.zombies object is incorrect. Did you provide the zombies.factors subset to the lapply command?"
        )
    })
})

# Add new level and recode NA to "No clothing"
levels(zombies$clothing) <- c(levels(zombies$clothing), "No clothing")
zombies$clothing[is.na(zombies$clothing)] <- "No clothing"

# Add new level and recode NA to "No documents"
levels(zombies$documents) <- c(levels(zombies$documents), "No documents")
zombies$documents[is.na(zombies$documents)] <- "No documents"

# Check recoding
summary(zombies)

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

levels(test_zombies$clothing) <- c(levels(test_zombies$clothing), "No clothing")
test_zombies$clothing[is.na(test_zombies$clothing)] <- "No clothing"

# Add new level and recode NA to "No documents"
levels(test_zombies$documents) <- c(levels(test_zombies$documents), "No documents")
test_zombies$documents[is.na(test_zombies$documents)] <- "No documents"

run_tests({
    test_that("the clothing variable is correct", {
        expect_identical(zombies$clothing,
                         test_zombies$clothing,
                         info = "The clothing variable recoding is incorrect. Did you supply the right data frame and variable names?"
                        )
    })

    test_that("the documents variable is correct", {
        expect_identical(zombies$documents,
                         test_zombies$documents,
                         info = "The documents variable recoding is incorrect. Did you supply the right data frame and variable names?"
                        )
    })
})

# Update subset of factors
zombies.factors <- zombies[ , sapply(zombies, is.factor)]

# Chi-squared for factors
chi.zombies <- lapply(zombies.factors, 
                       function(x){
                           return(chisq.test(x, zombies.factors$zombie))
                           })

# T-tests for numeric
ttest.age <- t.test(zombies$age ~ zombies$zombie)
ttest.water <- t.test(zombies$water.person ~ zombies$zombie)    

# Examine the results
chi.zombies 
ttest.age 
ttest.water

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# Update subset of factors
test_zombies.factors <- test_zombies[ , sapply(test_zombies, is.factor)]

# Chi-squared for factors
test_chi.zombies <- lapply(test_zombies.factors, 
                       function(x){
                           return(chisq.test(x, zombies.factors$zombie))
                           })

# T-tests for numeric
test_ttest.age <- t.test(test_zombies$age ~ test_zombies$zombie)
test_ttest.water <- t.test(test_zombies$water.person ~ test_zombies$zombie)   

run_tests({
    test_that("the factor subset is correct", {
        expect_identical(zombies.factors,
                         test_zombies.factors,
                         info = "The zombies.factors subset is incorrect. Did you supply the right data frame?"
                        )
    })
    test_that("the chi squareds are correct", {
        expect_equal(chi.zombies,
                         test_chi.zombies,
                         info = "The chi-squared analyses are incorrect. \n Did you supply zombies.factors to both the lapply and chisq.test commands?"
                        )
    })
    test_that("the age t-test is correct", {
        expect_equal(ttest.age$statistic,
                         test_ttest.age$statistic,
                         info = "The t-statistic for the age t-test is incorrect. \n Did you put the age variable first and the zombie variable second in the command?"
                        )
    })
    test_that("the water t-test is correct", {
        expect_equal(ttest.water$statistic,
                         test_ttest.water$statistic,
                         info = "The t-statistic for the water t-test is incorrect. \n Did you put the water variable first and the zombie variable second in the command?"
                        )
    })
})

# Create zombie model
zombie.model <- glm(zombie ~ age + water.person + food + rurality + medication + sanitation,
                   data = zombies, family = binomial(logit))

# Model significance, fit, and odds ratios with 95% CI
library(odds.n.ends)
zombie.model.fit <- odds.n.ends(zombie.model)

# Print the results of the odds.n.ends command
zombie.model.fit

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# Create zombie model
test_zombie.model <- glm(zombie ~ age + water.person + food + rurality + medication + sanitation,
                   data = test_zombies, family = binomial(logit))

# Model significance, fit, and odds ratios with 95% CI
test_zombie.model.fit <- odds.n.ends(test_zombie.model)


run_tests({
    test_that("odds.n.ends is loaded", {
        expect_true("odds.n.ends" %in% .packages(), info = "Did you load the odds.n.ends package?")
})
    
    test_that("the model is correct", {
        expect_equal(zombie.model$coefficient[1],
                     test_zombie.model$coefficient[1], 
                     info = "The intercept for zombie.model is incorrect. \n Did you add the medication variable to the model? Is the data frame spelled correctly as zombies?"
        )
        
    })
    test_that("the odds.n.ends output is correct", {
        expect_identical(zombie.model.fit,
                         test_zombie.model.fit,
                         info = "The odds.n.ends results are incorrect. \n Did you enter the zombie.model object into the command?"
        )
    })
})

# Compute GVIF 
library(car)
vif(zombie.model)

# Make a variable of the logit of the outcome
zombies$logitZombie <- log(zombie.model$fitted.values/(1-zombie.model$fitted.values))

# Graph the logit variable against age and water.person
ageLinearity <- ggplot(data = zombies, aes(x = age, y = logitZombie))+
  geom_point(color = "gray") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") + 
  geom_smooth(method = "lm", se = FALSE, color = "gray") + 
  theme_bw() 

waterPersonLin <- ggplot(data = zombies, aes(x = water.person, y = logitZombie))+
  geom_point(color = "gray") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") + 
  geom_smooth(method = "lm", se = FALSE, color = "gray") + 
  theme_bw() 

# View both plots side-by-side
grid.arrange(ageLinearity, waterPersonLin, ncol = 2)

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# Make a variable of the logit of the outcome
test_zombies$logitZombie <- log(test_zombie.model$fitted.values/(1-test_zombie.model$fitted.values))

# Graph the logit variable against age and water.person
test_ageLinearity <- ggplot(data = test_zombies, aes(x = age, y = logitZombie))+
  geom_point(color = "gray") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") + 
  geom_smooth(method = "lm", se = FALSE, color = "gray") + 
  theme_bw() 

test_waterPersonLin <- ggplot(data = test_zombies, aes(x = water.person, y = logitZombie))+
  geom_point(color = "gray") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") + 
  geom_smooth(method = "lm", se = FALSE, color = "gray") + 
  theme_bw() 


run_tests({
    test_that("package car was loaded", {
        expect_true("car" %in% .packages(), info = "Did you load the car package?")
    })
    test_that("the logit variable is correct", {
        expect_identical(zombies$logitZombie,
                         test_zombies$logitZombie,
                         info = "The logitZombie is incorrect. Did you use zombie.model to compute the logit values?")
    })
    test_that("the age graph is correct", {
        expect_equal(ageLinearity$data,
                     test_ageLinearity$data,
                     info = "The data in ageLinearity are incorrect. Did you use zombies?")
        expect_identical(ageLinearity$mapping$x,
                         test_ageLinearity$mapping$x,
                         info = "The x aesthetic in ageLinearity is incorrect. Did you use age?")
    })
    
    get_layers <- function(p) {
        unlist(c(list(p$layers), purrr::map(p$layers, "layers")))
    }
    
    test_that("the water graph is correct", {
        usr_lyrs <- get_layers(waterPersonLin)
        test_lyrs <- get_layers(test_waterPersonLin)
        
        expect_equal(waterPersonLin$data,
                     test_waterPersonLin$data,
                     info = "The data in waterPersonLin are incorrect. Did you use zombies?")
        expect_identical(waterPersonLin$mapping$x,
                         test_waterPersonLin$mapping$x,
                         info = "The x aesthetic in waterPersonLin is incorrect. Did you use age?")
        expect_equal(usr_lyrs[[2]],
                     test_lyrs[[2]],
                     info = "The second geom layer is incorrect. \n Did you use geom_smooth() with method = 'loess' and se = FALSE?")
        expect_equal(usr_lyrs[[3]],
                     test_lyrs[[3]],
                     info = "The third geom layer is incorrect. \n Did you use geom_smooth() with method = 'lm' and se = FALSE?")  
    })
})

# Make a new data frame with the relatives data in it 
newdata <- data.frame(age = c(71, 40), 
                      water.person = c(5, 3),
                      food = c("Food", "Food"),
                      rurality = c("Suburban", "Urban"),
                      medication = c("Medication", "Medication"),
                      sanitation = c("Sanitation", "Sanitation"))

# Use the new data frame to predict 
predictions <- predict(zombie.model, newdata, type = "response")

# Print the predicted probabilities
predictions

# One or more tests of the student's code
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# Make a new data frame with the relatives data in it 
test_newdata <- data.frame(age = c(71, 40), 
                      water.person = c(5, 3),
                      food = c("Food", "Food"),
                      rurality = c("Suburban", "Urban"),
                      medication = c("Medication", "Medication"),
                      sanitation = c("Sanitation", "Sanitation"))

# Use the new data frame to predict 
test_predictions <- predict(test_zombie.model, test_newdata, type = "response")

run_tests({
    test_that("the data frame is accurate", {
        expect_identical(newdata,
                         test_newdata,
                         info = "The newdata data frame is incorrect. Did you fill in the correct lines with 40, 3, Food, Urban, Medication, and Sanitation?"
                        )
    })
    test_that("the predicted probabilities are correct", {
        expect_equal(predictions,
                         test_predictions,
                         info = "The predicted probabilities are incorrect. Did you enter zombie.model into the predict command?"
                        )
    })
})

# Add your data to the newdata data frame
newdata <- data.frame(age = c(71, 40, 38), 
                      water.person = c(5, 3, 10),
                      food = c("Food", "Food", "Food"),
                      rurality = c("Suburban", "Urban", "Rural"),
                      medication = c("Medication", "Medication", "Medication"),
                      sanitation = c("Sanitation", "Sanitation", "Sanitation"))

# Use the new data frame to predict 
predictions <- predict(zombie.model, newdata, type = "response")

# Print the predictions
predictions

test_newdata_length <- nrow(newdata)

run_tests({
    test_that("newdata has three observations", {
        expect_true(test_newdata_length == 3,
                    info = "The newdata data frame is incorrect. Did you fill in appropriate values for all the variables?"
                   )
    })
    test_that("age is numeric", {
        expect_true(is.numeric(newdata$age),
                    info = "The age variable must be a number."
                   )
    })
    test_that("water.person is numeric", {
        expect_true(is.numeric(newdata$water.person),
                    info = "The water.person variable must be a number."
                   )
    })
    test_that("food is correctly entered", {
        expect_true(newdata$food[3] %in% c("Food", "No food"),
                    info = "The food variable must Food or No food."
                   )
    })
    test_that("rurality is correctly entered", {
        expect_true(newdata$rurality[3] %in% c("Urban", "Suburban", "Rural"),
                    info = "The rurality variable value must be Urban, Suburban, or Rural."
                   )
    })
    test_that("medication is correctly entered", {
        expect_true(newdata$medication[3] %in% c("Medication", "No medication"),
                    info = "The medication variable value must be Medication or No medication."
                   )
    })
     test_that("sanitation is correctly entered", {
         expect_true(
             newdata$sanitation[3] %in% c("Sanitation", "No sanitation"),
             info = "The sanitation variable value must be Sanitation or No sanitation."
         )
    })
})

# What is your probability of becoming a zombie?
me <- 0.00115066627592415

# How prepared are you for a real emergency?
preparedness_level  <- "I got this!"

run_tests({
    test_that("me is numeric ", {
        expect_true(is.numeric(me),
                    info = "Did you assign your probability of becoming a zombie to me? It should be a number."
                   )
    })
    test_that("preparedness_level is a string", {
        expect_true(is.character(preparedness_level),
                    info = "preparedness_level should be a character string."
                   )
    })
})
