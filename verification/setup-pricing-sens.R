setup.pricing <- function(){
  station.cost <<- 1
  overnight.cap <<- (1-get.param('FractionFinancedDist')) * station.cost
  loan.principal <<- get.param('FractionFinancedDist') * station.cost
  loan.payments <<- rep(loan.principal * get.param('FinanceRateDist') / (1 - (1+get.param('FinanceRateDist'))^-get.param('FinanceTermDist')),get.param('FinanceTermDist')) 
  loan.payments.disc <<- loan.payments / (1 + get.param('DiscountRate'))^(0:(get.param('FinanceTermDist')-1))
  amort.dist.ratio <<- (overnight.cap + sum(loan.payments.disc))*(get.param('DiscountRate')*(1+get.param('DiscountRate'))^get.param('DistributionLifetime'))/((1+get.param('DiscountRate'))^get.param('DistributionLifetime') - 1) / 1e3 # 1e3 converts from GJ to MJ
  pens <<- inputs[['AltFuelPenetrationLimits']]
  fuel.markup <<- inputs[['FuelRegionalMarkup']]
  setkey(fuel.markup,Replaces)
  carb <<- copy(inputs[['CarbonIntensity']])
  gas.carb <<- carb[AltFuel=='Gas']$Intensity
  gas.mj.per.gal <<- carb[AltFuel=='Gas']$ConversionFactor
  diesel.carb <<- carb[AltFuel=='Diesel']$Intensity
  flex.carb <<- carb[AltFuelType=='Ethanol']
  flex.blend.by.energy <<- cbind(flex.carb$ConversionFactor * get.param('FlexBlend'),(1-get.param('FlexBlend'))* gas.mj.per.gal)
  flex.blend.by.energy <<- as.data.frame(t(apply(flex.blend.by.energy,1,function(x){ x/sum(x) })))
  names(flex.blend.by.energy) <<- c('flex','gas')
  flex.carb[,':='(AltFuelType='Flex',Intensity=Intensity*flex.blend.by.energy$flex + gas.carb*flex.blend.by.energy$gas)]
  eth.blend.by.energy <<- cbind(carb[AltFuelType=='Ethanol']$ConversionFactor * get.param('EthanolBlend'),(1-get.param('EthanolBlend'))* gas.mj.per.gal)
  eth.blend.by.energy <<- as.data.frame(t(apply(eth.blend.by.energy,1,function(x){ x/sum(x) })))
  names(eth.blend.by.energy) <<- c('eth','gas')
  carb[AltFuelType=='Ethanol',':='(Intensity=Intensity*eth.blend.by.energy$eth + gas.carb*eth.blend.by.energy$gas)]
  phev.carb<<- carb[AltFuelType=='Electricity']
  phev.carb[,':='(AltFuelType='PHEV',AltFuel='PHEV',Intensity=Intensity*get.param('PHEVBlend')+gas.carb*(1-get.param('PHEVBlend')),EER=1/((1/EER)*get.param('PHEVBlend')+(1-get.param('PHEVBlend'))))]
  carb[AltFuelType=='Biodiesel',':='(Intensity=Intensity*get.param('BiodieselBlend')+diesel.carb*(1-get.param('BiodieselBlend')))]
  carb <<- rbindlist(list(carb,flex.carb,phev.carb),use.names=T,fill=T)
  carb[AltFuelType=='Electricity',':='(AltFuelType='BEV',AltFuel='BEV')]

  mark <<- inputs[['MarketStructure']]
  ghg.goal <<- sum(mark$GHG) - (sum(mark[FuelType=='Gas']$EnergyDemand*1e3)*get.param('TargetGasolineIntensity') + sum(mark[FuelType=='Diesel']$EnergyDemand*1e3)*get.param('TargetDieselIntensity'))/1e6
      
  fuel.price.m <<- melt(inputs[['FuelTimeSeries']],id.vars=c('Year','Quarter'))
  setkey(fuel.price.m,variable)
  fuel.price <<- fuel.price.m[Year>=2010,list(PriceAvg=mean(value,na.rm=T)),by='variable']
  fuel.price[,FuelType:=carb$AltFuelType[match(variable,carb$AltFuel)]]
  fuel.price[,':='(AltFuel=variable,variable=NULL)]
  fuel.price[,Replaces:='None']
  fuel.price[FuelType=='Gas' | FuelType=='Ethanol',Replaces:='Gas']
  fuel.price[FuelType=='Diesel' | FuelType=='Biodiesel',Replaces:='Diesel']
  fuel.price[FuelType=='H2',Replaces:='H2']
  setkey(fuel.price,Replaces)
  fuel.price[,PriceRV:=PriceAvg]
  elec.price <<- fuel.price[AltFuel=='Electricity']$PriceRV
  gas.price <<- fuel.price[AltFuel=='Gas']$PriceRV
  diesel.price <<- fuel.price[AltFuel=='Diesel']$PriceRV
  # Blend Flex and Ethanol
  setkey(fuel.price,FuelType,AltFuel)
  flex.prices <<- copy(fuel.price[FuelType=='Ethanol'])
  flex.prices[,':='(FuelType='Flex',PriceRV=PriceRV*flex.blend.by.energy$flex+gas.price*flex.blend.by.energy$gas)]
  fuel.price[FuelType=='Ethanol',':='(PriceRV=PriceRV*eth.blend.by.energy$eth+gas.price*eth.blend.by.energy$gas)]
  # Blend PHEV
  phev.price <<- data.table(AltFuel='PHEV',FuelType='PHEV',Replaces='None',PriceRV=elec.price * get.param('PHEVBlend') + gas.price * (1 - get.param('PHEVBlend')))
  # Blend Biodiesel
  fuel.price[FuelType=='Biodiesel',':='(PriceRV=PriceRV*get.param('BiodieselBlend')+diesel.price*(1-get.param('BiodieselBlend')))]
  # Rename Elec to BEV
  fuel.price[AltFuel=='Electricity',':='(FuelType='BEV',AltFuel='BEV')]
  fuel.price <<- rbindlist(list(fuel.price,flex.prices,phev.price),use.names=T,fill=T)

  setkey(fuel.price,Replaces)
  fuel.price.by.county <<- fuel.markup[fuel.price,allow.cart=T]
  fuel.price.by.county[is.na(Markup),Markup:=0]
  fuel.price.by.county[,FuelCost:=PriceRV+Markup]

  # Now EER weight the cost using the LDV value for H2
  setkey(carb,AltFuel,IntensityEER) # need this to get H2 in correct order
  setkey(carb,AltFuel)
  setkey(fuel.price.by.county,AltFuel)
  fuel.price.by.county <<- unique(carb)[,list(AltFuel,EER)][fuel.price.by.county]
  fuel.price.by.county[,FuelCost:=FuelCost/EER]

  # Join the conventional prices to market segments for later use in making the fuel cost incremental
  mark <<- copy(mark)
  setkey(mark,County,FuelType)
  setkey(fuel.price.by.county,County,FuelType)
  mark <<- fuel.price.by.county[mark,list(County,FuelType,VehicleType,VehicleStatus,NumVehicles,SegmentVMT,EnergyDemand,GHG,travel.demand,FuelCost)]
  mark[,':='(ConventionalFuelCost=FuelCost,FuelCost=NULL)]

  # Now drop the conventionals
  fuel.price.by.county <<- fuel.price.by.county[FuelType!="Gas" & FuelType!="Diesel"]
  fuel.price.by.county[,':='(AltFuelType=FuelType,FuelType=NULL)]

  ###
  # Sample and amortized the station cost into 2014$ / MJ
  ###
  station.costs <<- inputs[['DistributionCosts']]
  station.costs[is.na(DistCostSD),DistCostSD:=0.25*DistCostAvg]
  station.costs[,DistCostRV:=DistCostAvg]
  station.costs[,StationCost:=DistCostRV * amort.dist.ratio]
  setkey(carb,AltFuelType,IntensityEER) # need this to get H2 in correct order
  setkey(carb,AltFuelType)
  setkey(station.costs,AltFuelType)
  station.costs <<- unique(carb)[,list(AltFuelType,EER)][station.costs]
  station.costs[,StationCost:=StationCost/EER]

  ###
  # Sample and amortize the vehicle costs into 2014$ / MJ
  ###
  veh.costs <<- inputs[['VehicleCosts']]
  setkey(veh.costs,VehicleType)
  setkey(mark,VehicleType)
  veh.costs <<- mark[VehicleStatus=='New'][veh.costs, allow.cartesian=T]
  veh.costs[,SegmentVMT.per.veh.per.yr:=SegmentVMT/NumVehicles]
  tot.vmt <<- inputs[['VehicleLifetimeAndVMT']]
  # we need to base lifetime vmt off of the appropriate fuel type
  veh.costs[,FuelTypeForLifetime:=FuelType]
  veh.costs[AltFuelType%in%c('BEV','PHEV','Flex'),FuelTypeForLifetime:='Gas']
  tot.vmt[,FuelTypeForLifetime:=FuelType]
  setkey(tot.vmt,FuelTypeForLifetime,VehicleType)
  setkey(veh.costs,FuelTypeForLifetime,VehicleType)
  veh.costs <<- tot.vmt[veh.costs]
  veh.costs[,':='(FuelType=i.FuelType,i.FuelType=NULL)]
  # we need to get the segment vmt aggregated across vehicle status
  setkey(mark,County,FuelType,VehicleType)
  agg.seg.vmt <<- mark[,list(AggSegmentVMTPerVeh=sum(SegmentVMT)/sum(NumVehicles)),by=c('County','FuelType','VehicleType')]
  setkey(veh.costs,County,FuelType,VehicleType)
  veh.costs <<- agg.seg.vmt[veh.costs,allow.cartesian=T]
  setkey(veh.costs,AltFuelType,VehicleType)
  setkey(carb,AltFuelType,VehicleType)
  veh.costs <<- unique(carb)[,list(AltFuelType,VehicleType,EER)][veh.costs]
  veh.costs[is.na(EER),EER:=1]
  veh.costs[,MJ.EER.per.mile:=EnergyDemand / SegmentVMT * EER * 1000]
  veh.costs[,cost.per.MJ.EER := amort.veh.cost(VehicleCostAvg,VMTPerLifetime,AggSegmentVMTPerVeh,MJ.EER.per.mile)]
  veh.costs[,cost.per.MJ.EER.ratio := cost.per.MJ.EER / VehicleCostAvg]
  veh.costs[,':='(VehicleCost=VehicleCostAvg * cost.per.MJ.EER.ratio)]
  return(NULL)
}

