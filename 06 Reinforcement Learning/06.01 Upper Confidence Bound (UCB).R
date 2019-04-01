### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    UPPER CONFIDENCE BOUND (UCB) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/31/2019

#   IMPORT DATASET
dataset = read.csv('Ads_CTR_Optimisation.csv')

#   IMPLEMENT RANDOM SELECTION (FOR COMPARISON)
N = dim(dataset)[1]
d = dim(dataset)[2]
ads_selected = integer(0)
total_reward = 0
for (n in 1:N) {
  ad = sample(1:10, 1)
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  total_reward = total_reward + reward
}

#   VISUALIZE RANDOM SELECTION (FOR COMPARISON)
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of Ad Selections (Random)',
     xlab = 'Ad',
     ylab = 'Number of Times Each Ad was Selected')

#   IMPLEMENT UCB
N = dim(dataset)[1]
d = dim(dataset)[2]
ads_selected = integer(0)
numbers_of_selections = integer(d)
sums_of_rewards = integer(d)
total_reward = 0
for (n in 1:N) {
  ad = 0
  max_upper_bound = 0
  for (i in 1:d) {
    if (numbers_of_selections[i] > 0) {
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
        upper_bound = 1e400
    }
    if (upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

#   VISUALIZE UCB
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of Ad Selections (UCB)',
     xlab = 'Ad',
     ylab = 'Number of Times Each Ad was Selected')