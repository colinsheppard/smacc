      mark <- inputs[['MarketStructure']]
      setkey(mark,County,FuelType,VehicleType,VehicleStatus)
      mark[,Intensity:=GHG*1e6/(EnergyDemand*1e3)] # carbon intensity of the fuels in g/MJ
      ghg.goal <- sum(mark$GHG) - (sum(mark[FuelType=='Gas']$EnergyDemand*1e3)*get.param('TargetGasolineIntensity') + sum(mark[FuelType=='Diesel']$EnergyDemand*1e3)*get.param('TargetDieselIntensity'))/1e6 # 238K tons

      pens <- inputs[['AltFuelPenetrationLimits']]
      fuel.markup <- inputs[['FuelRegionalMarkup']]
      setkey(fuel.markup,Replaces)
      station.costs <- inputs[['DistributionCosts']]
      station.costs[is.na(DistCostSD),DistCostSD:=0.25*DistCostAvg]

      # blend the carbon intensities and EERs
      carb <- copy(inputs[['CarbonIntensity']])
      gas.carb <- carb[AltFuel=='Gas']$Intensity
      gas.mj.per.gal <- carb[AltFuel=='Gas']$ConversionFactor
      diesel.carb <- carb[AltFuel=='Diesel']$Intensity
      flex.carb <- carb[AltFuelType=='Ethanol']
      flex.blend.by.energy <- cbind(flex.carb$ConversionFactor * get.param('FlexBlend'),(1-get.param('FlexBlend'))* gas.mj.per.gal)
      flex.blend.by.energy <- as.data.frame(t(apply(flex.blend.by.energy,1,function(x){ x/sum(x) })))
      names(flex.blend.by.energy) <- c('flex','gas')
      flex.carb[,':='(AltFuelType='Flex',Intensity=Intensity*flex.blend.by.energy$flex + gas.carb*flex.blend.by.energy$gas)]
      eth.blend.by.energy <- cbind(carb[AltFuelType=='Ethanol']$ConversionFactor * get.param('EthanolBlend'),(1-get.param('EthanolBlend'))* gas.mj.per.gal)
      eth.blend.by.energy <- as.data.frame(t(apply(eth.blend.by.energy,1,function(x){ x/sum(x) })))
      names(eth.blend.by.energy) <- c('eth','gas')
      carb[AltFuelType=='Ethanol',':='(Intensity=Intensity*eth.blend.by.energy$eth + gas.carb*eth.blend.by.energy$gas)]
      phev.carb<- carb[AltFuelType=='Electricity']
      phev.carb[,':='(AltFuelType='PHEV',AltFuel='PHEV',Intensity=Intensity*get.param('PHEVBlend')+gas.carb*(1-get.param('PHEVBlend')),EER=1/((1/EER)*get.param('PHEVBlend')+(1-get.param('PHEVBlend'))))]
      carb[AltFuelType=='Biodiesel',':='(Intensity=Intensity*get.param('BiodieselBlend')+diesel.carb*(1-get.param('BiodieselBlend')))]
      carb <- rbindlist(list(carb,flex.carb,phev.carb),use.names=T,fill=T)
      carb[AltFuelType=='Electricity',':='(AltFuelType='BEV',AltFuel='BEV')]

      # get the correlation matrix on the fuels
      fuel.cor <- cor(as.data.frame(inputs[['FuelTimeSeries']])[,3:ncol(inputs[['FuelTimeSeries']])],use='pairwise.complete.obs')
      fuel.var <- var(na.omit(as.data.frame(inputs[['FuelTimeSeries']])[,3:ncol(inputs[['FuelTimeSeries']])],use='pairwise.complete.obs'))

      file.name <- list.files(scenario.dir,pattern=".csv$")[1]

      ####################################################################################################
      # Useful Plots
      ####################################################################################################
      if(F){
        ggplot(melt(inputs[['FuelTimeSeries']],id.vars=c('Year','Quarter')),aes(x=Year+(Quarter-1)/4,y=value,colour=variable))+geom_line()
      }

      ####################################################################################################
      # Cost Calcs
      ####################################################################################################

      ###
      # For now take the average price of fuel since 2010 to be our default fuel price scenario
      ###
      fuel.price.m <- melt(inputs[['FuelTimeSeries']],id.vars=c('Year','Quarter'))
      setkey(fuel.price.m,variable)
      fuel.price <- fuel.price.m[Year>=2010,list(PriceAvg=mean(value,na.rm=T)),by='variable']
      fuel.price[,FuelType:=carb$AltFuelType[match(variable,carb$AltFuel)]]
      fuel.price[,':='(AltFuel=variable,variable=NULL)]
      fuel.price[,Replaces:='None']
      fuel.price[FuelType=='Gas' | FuelType=='Ethanol',Replaces:='Gas']
      fuel.price[FuelType=='Diesel' | FuelType=='Biodiesel',Replaces:='Diesel']
      fuel.price[FuelType=='H2',Replaces:='H2']
      setkey(fuel.price,Replaces)
      fuel.price.rvs <- rmvnorm(n,fuel.price$PriceAvg,sigma=fuel.var)
      
      station.cost <- 1
      overnight.cap <- (1-get.param('FractionFinancedDist')) * station.cost
      loan.principal <- get.param('FractionFinancedDist') * station.cost
      loan.payments <- rep(loan.principal * get.param('FinanceRateDist') / (1 - (1+get.param('FinanceRateDist'))^-get.param('FinanceTermDist')),get.param('FinanceTermDist')) 
      loan.payments.disc <- loan.payments / (1 + get.param('DiscountRate'))^(0:(get.param('FinanceTermDist')-1))
      amort.dist.ratio <- (overnight.cap + sum(loan.payments.disc))*(get.param('DiscountRate')*(1+get.param('DiscountRate'))^get.param('DistributionLifetime'))/((1+get.param('DiscountRate'))^get.param('DistributionLifetime') - 1) / 1e3 # 1e3 converts from GJ to MJ

      ###
      # Vehicle amortization
      # find the ratio of amortized vehicle cost to the original
      ###
      # testing:
      # veh.cost <- 11758
      # tot.vmt<-171536.0
      # seg.vmt<-11376.84
      # mj.per.mile<-16.33729

      # We will be making a unique cost estimate for each combination of new alt vehicle, county, and 
      # fuel type so we do a cartesian join between market and vehicle cost tables
      veh.costs <- inputs[['VehicleCosts']]
      setkey(veh.costs,VehicleType)
      setkey(mark,VehicleType)
      veh.costs <- mark[VehicleStatus=='New'][veh.costs, allow.cartesian=T]
      veh.costs[,SegmentVMT.per.veh.per.yr:=SegmentVMT/NumVehicles]
      tot.vmt <- inputs[['VehicleLifetimeAndVMT']]
      # we need to base lifetime vmt off of the appropriate fuel type
      veh.costs[,FuelTypeForLifetime:=FuelType]
      veh.costs[AltFuelType%in%c('BEV','PHEV','Flex'),FuelTypeForLifetime:='Gas']
      tot.vmt[,FuelTypeForLifetime:=FuelType]
      setkey(tot.vmt,FuelTypeForLifetime,VehicleType)
      setkey(veh.costs,FuelTypeForLifetime,VehicleType)
      veh.costs <- tot.vmt[veh.costs]
      veh.costs[,':='(FuelType=i.FuelType,i.FuelType=NULL)]
      # we need to get the segment vmt aggregated across vehicle status
      setkey(mark,County,FuelType,VehicleType)
      agg.seg.vmt <- mark[,list(AggSegmentVMTPerVeh=sum(SegmentVMT)/sum(NumVehicles)),by=c('County','FuelType','VehicleType')]
      setkey(veh.costs,County,FuelType,VehicleType)
      veh.costs <- agg.seg.vmt[veh.costs,allow.cartesian=T]
      setkey(veh.costs,AltFuelType,VehicleType)
      setkey(carb,AltFuelType,VehicleType)
      veh.costs <- unique(carb)[,list(AltFuelType,VehicleType,EER)][veh.costs]
      veh.costs[is.na(EER),EER:=1]
      veh.costs[,MJ.EER.per.mile:=EnergyDemand / SegmentVMT * 1000]
      veh.costs[,cost.per.MJ.EER := amort.veh.cost(VehicleCostAvg,VMTPerLifetime,AggSegmentVMTPerVeh,MJ.EER.per.mile)]
      veh.costs[,cost.per.MJ.EER.ratio := cost.per.MJ.EER / VehicleCostAvg]

