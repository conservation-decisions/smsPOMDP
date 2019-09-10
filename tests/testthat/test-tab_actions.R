context("smsPOMDP")

test_that("Computes the best policy to follow if the species is not seen. Summarized in a data.frame", {
  #values for Sumatran tigers
  pen = 0.1
  p0 = 1-pen
  pem = 0.05816
  pm = 1 - pem
  V = 175.133
  Cm = 18.784
  Cs = 10.840
  d0 = 0.01
  d = 0.78193
  
  t = smsPOMDP::tr(p0, pm, d0, d, V, Cm, Cs) #transition matrix
  o = smsPOMDP::obs(p0, pm, d0, d, V, Cm, Cs)#observation matrix
  r = smsPOMDP::rew(p0, pm, d0, d, V, Cm, Cs)#reward matrix
  state_prior = c(1,0)
  Tmax = 100
  tab = smsPOMDP::tab_actions(t, o, r, state_prior)
  
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  
})