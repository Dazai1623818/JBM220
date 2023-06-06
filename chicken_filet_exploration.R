Sys.setlocale("LC_ALL", "English")
install.packages(c("caret", "pls", "e1071"))
library(caret)
library(pls)
library(e1071)
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(123)

data_chicken <- read.csv("Chicken_filet.csv", sep=";", header=TRUE)

head(data_chicken)

# Summary statistics
summary(data_chicken)

ggplot(data_chicken, aes(x=X908.1)) +
  geom_histogram(bins=30, alpha=0.5, fill='blue') +
  theme_minimal() +
  labs(x='X908.1', y='Count', title='Distribution of X908.1')

# Reshape the data so that it's long-format
spectral_data <- data_chicken %>%
  tidyr::pivot_longer(cols = starts_with("X"), names_to = "wavelength", values_to = "intensity") 

# Convert wavelengths to numeric
spectral_data$wavelength <- as.numeric(gsub("X", "", spectral_data$wavelength))

# Now, subset the spectral_data into fresh and thawed
spectral_data_fresh <- spectral_data[spectral_data$Freshness == 'FR',]
spectral_data_thawed <- spectral_data[spectral_data$Freshness == 'TH',]


# mean intensity for each wavelength
mean_spectrum <- spectral_data %>%
  group_by(wavelength) %>%
  summarize(mean_intensity = mean(intensity))

# mean spectrum plot --> average spectral response across all samples in the dataset (x axis: wavelengths, y-axis: avg intensity of all samples at that particular wavelength)
ggplot(mean_spectrum, aes(x=wavelength, y=mean_intensity)) +
  geom_line() +
  theme_minimal() +
  labs(x='Wavelength', y='Mean intensity', title='Mean Spectrum')



mean_spectrum_fresh <- spectral_data_fresh %>%
  group_by(wavelength) %>%
  summarize(mean_intensity = mean(intensity))

mean_spectrum_thawed <- spectral_data_thawed %>%
  group_by(wavelength) %>%
  summarize(mean_intensity = mean(intensity))

# plots
par(mfrow = c(1, 2))
plot(mean_spectrum_fresh$wavelength, mean_spectrum_fresh$mean_intensity, type = 'l', 
     xlab = 'Wavelength', ylab = 'Mean Intensity', main = 'Fresh Chicken')
plot(mean_spectrum_thawed$wavelength, mean_spectrum_thawed$mean_intensity, type = 'l', 
     xlab = 'Wavelength', ylab = 'Mean Intensity', main = 'Thawed Chicken')


# All intensities for each wavelength in spectral_data
ggplot(spectral_data, aes(x=wavelength, y=intensity)) +
  geom_line() +
  theme_minimal() +
  labs(x='Wavelength', y='Intensity', title='All Wavelengths')

# All intensities for each wavelength in spectral_data_fresh
ggplot(spectral_data_fresh, aes(x=wavelength, y=intensity)) +
  geom_line() +
  theme_minimal() +
  labs(x='Wavelength', y='Intensity', title='All Wavelengths - Fresh Chicken')

# All intensities for each wavelength in spectral_data_thawed
ggplot(spectral_data_thawed, aes(x=wavelength, y=intensity)) +
  geom_line() +
  theme_minimal() +
  labs(x='Wavelength', y='Intensity', title='All Wavelengths - Thawed Chicken')
