# load the dataset
dataset <- iris
# create the model
model <- lm(Petal.Length ~ Petal.Width, data = dataset)
# # example: run the model once
# input_data <- data.frame(Petal.Width=1)
# predict(model,input_data)