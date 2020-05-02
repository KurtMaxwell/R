set.seed(123)

m = 10000
Family_size_mean = 4
Family_size_sd = 2

Family_comp = 0.1
Initial_infection = 0.08
recovery_rate = 0.05

FamiliyMatrix = matrix(0, m, 7)

proportion_risk_composition = 0.5 

#------------------------------------------
# Simulate the social structure
#------------------------------------------

Family_risk=round((proportion_risk_composition+0.5)*runif(m))

for(i in 1:m){

	FamiliyMatrix[i,1] = max(1, round(rnorm(1, Family_size_mean, Family_size_sd)))
	FamiliyMatrix[i,3] = round(FamiliyMatrix[i,1]*max(0, rnorm(1, Family_comp, 0.01)))
	FamiliyMatrix[i,2] = FamiliyMatrix[i,1] - FamiliyMatrix[i,3]
	FamiliyMatrix[i,4] = round(FamiliyMatrix[i,2]*rnorm(1, Initial_infection, 0.01))
	FamiliyMatrix[i,5] = round(FamiliyMatrix[i,3]*rnorm(1, Initial_infection, 0.01))
} 

#----------------
#
# Description of FamiliyMatrix 
#
# Col 1: total number of people in a given family
# Col 2: total number of non-fragile member of a given family
# Col 3: total number of fragile member of a given family
# Col 4: total number of non-fragile member of a given family who are already infected
# Col 5: total number of fragile member of a given family who are already infected
#col 6: total number of recovered non fragile members of the family
#col 7: total number of recovered fragile members of the family
#----------------

Population = sum(FamiliyMatrix[,1])

#------------------------------------------
# Simulate the contagion and death process
#------------------------------------------

P_g = 0.9	# probability of being infected if a member of the family is infected
P_e = 0.01	# probability of being infected from outside the family
P_d0 = 0.5	# probability of death for fragile subjects
P_d1 = 0.5 	# probability of death for non-fragile subjects
P_r0 = 0.9 # probabilty of recovery for fragile infected members 
P_r1 = 0.8 # probability of recovery for non fragile members

SilulList = list()
SilulList[[1]] = FamiliyMatrix

FragileInfected = sum(SilulList[[1]][,5])
NonFragileInfected = sum(SilulList[[1]][,4])

N_days = 21

for(day in 2:N_days){

	FamiliyMatrix_0 = matrix(0, m, 7)

	InfectedFraction = sum(SilulList[[day-1]][,4] + SilulList[[day-1]][,5])/Population 

	for(i in 1:m){	

		#----------------------------------
		# Simulate the survival/death process
		#----------------------------------

		TossCoin_d0 = P_d0*runif(1)
		FamiliyMatrix_0[i,3] = max(0, SilulList[[day-1]][i,3] - round(TossCoin_d0*SilulList[[day-1]][i,5]) )
		FamiliyMatrix_0[i,5] = max(0, SilulList[[day-1]][i,5] - round(TossCoin_d0*SilulList[[day-1]][i,5]) )

		TossCoin_d1 = P_d1*runif(1)
		FamiliyMatrix_0[i,2] = max(0, SilulList[[day-1]][i,2] - round(TossCoin_d1*SilulList[[day-1]][i,4]) )
		FamiliyMatrix_0[i,4] = max(0, SilulList[[day-1]][i,4] - round(TossCoin_d0*SilulList[[day-1]][i,4]) )

		FamiliyMatrix_0[i,1] = FamiliyMatrix_0[i,2] + FamiliyMatrix_0[i,3]

		#----------------------------------
		# Potential ricovery
		#----------------------------------
		TossCoin_r1 = P_r1*runif(1)
		FamiliyMatrix_0[i,6] = max(0, SilulList[[day-1]][i,6] + round(TossCoin_r1*SilulList[[day-1]][i,4]))
		FamiliyMatrix_0[i,2] = max(0, SilulList[[day-1]][i,2] - round(TossCoin_r1*SilulList[[day-1]][i,4]) )
		FamiliyMatrix_0[i,4] = max(0, SilulList[[day-1]][i,4] - round(TossCoin_r1*SilulList[[day-1]][i,4]) )
	
		
		TossCoin_r0 = P_r0*runif(1)
		FamiliyMatrix_0[i,7] = max(0, SilulList[[day-1]][i,7] + round(TossCoin_r0*SilulList[[day-1]][i,5]))
		FamiliyMatrix_0[i,3] = max(0, SilulList[[day-1]][i,3] - round(TossCoin_r0*SilulList[[day-1]][i,5]) )
		FamiliyMatrix_0[i,5] = max(0, SilulList[[day-1]][i,5] - round(TossCoin_r0*SilulList[[day-1]][i,5]) )
		
    #Total suseptibe member of a family	
		FamiliyMatrix_0[i,1] = FamiliyMatrix_0[i,2] + FamiliyMatrix_0[i,3]
    

		#----------------------------------
		# Simulate the contagion process
		#----------------------------------
		
		# Randomly select whether fragile vs non-fragile will be infected

		TC_inf_e = round(P_e*InfectedFraction *runif(1))
		TC_inf_g = round(P_g*(SilulList[[day-1]][i,4] + SilulList[[day-1]][i,5])*runif(1))
		

		F = round(runif(1))

		if(F > 0){

			FamiliyMatrix_0[i,4] = min(SilulList[[day-1]][i,2], SilulList[[day-1]][i,4] + max(TC_inf_e,  TC_inf_g))
      
			FamiliyMatrix_0[i,2] = max(0, SilulList[[day-1]][i,2] - max(TC_inf_e,  TC_inf_g))
			
			FamiliyMatrix_0[i,5] = SilulList[[day-1]][i,5]

		}else{
			FamiliyMatrix_0[i,5] = min(SilulList[[day-1]][i,3], SilulList[[day-1]][i,5] + max(TC_inf_e,  TC_inf_g))

			FamiliyMatrix_0[i,3] = max(0, SilulList[[day-1]][i,3] - max(TC_inf_e,  TC_inf_g))
			
			FamiliyMatrix_0[i,4] = SilulList[[day-1]][i,4]

		}
		

		#print(c(sum(SilulList[[day-1]][1:i,4]), sum(FamiliyMatrix_0[1:i,4])))

	}

	SilulList[[day]] = FamiliyMatrix_0

	FragileInfected = c(FragileInfected, sum(SilulList[[day]][,5]))
	NonFragileInfected = c(NonFragileInfected, sum(SilulList[[day]][,4]))

	#par(mfrow=c(2,1))
	plot(1:day, FragileInfected, type = 'o',  ylim = c(0, Population/4))
	lines(1:day, NonFragileInfected, type = 'o', col = 2)
	abline(h = 3000, col = 4, lwd = 3)


}










