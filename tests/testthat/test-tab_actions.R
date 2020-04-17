context("smsPOMDP")

test_that("Computes the best policy to follow if the species is not seen. Summarized in a data.frame", {
  #values for Sumatran tigers
  pen <- 0.1
  p0 <- 1-pen
  pem <- 0.05816
  pm <- 1 - pem
  V <- 175.133
  Cm <- 18.784
  Cs <- 10.840
  d0 <- 0.01
  dm <- 0.01
  ds <- 0.78193
  
  #buiding the matrices of the problem
  t <- smsPOMDP::tr(p0, pm, d0, dm, ds, V, Cm, Cs) #transition matrix
  o <- smsPOMDP::obs(p0, pm, d0, dm, ds, V, Cm, Cs)#observation matrix
  r <- smsPOMDP::rew(p0, pm, d0, dm, ds, V, Cm, Cs)#reward matrix
  Tmax <- 100
  
  #different values for the state prior
  state_prior <- c(1,0)
  tab <- smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions <- tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior <- c(0.9,0.1)
  tab <- smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions <- tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior <- c(0.8,0.2)
  tab <- smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions <- tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior <- c(0.7,0.3)
  tab <- smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions <- tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior<-c(0.6,0.4)
  tab<-smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions<-tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior<-c(0.5,0.5)
  tab<-smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions<-tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior<-c(0.4,0.6)
  tab<-smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions<-tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior<-c(0.3,0.7)
  tab<-smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions<-tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior<-c(0.2,0.8)
  tab<-smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions<-tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior<-c(0.1,0.9)
  tab<-smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions<-tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
  state_prior<-c(0,1)
  tab<-smsPOMDP::tab_actions(t, o, r, state_prior)
  expect_equal(dim(tab)[2],2)#2 rows
  expect_equal(sum(tab[,2]), Tmax+1)#Tmax + 1 years planning
  actions<-tab$action
  expect_true(unique(actions %in% c(1,2,3))) #the list of actions is in c(1,2,3)
  
})