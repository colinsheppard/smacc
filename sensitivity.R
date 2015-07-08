
####################################################################################################
# Load libs and data
####################################################################################################

load.libraries(c('stringr','truncnorm','reshape2','gdata','mvtnorm','gtools','yaml','GGally','stringr'))
installXLSXsupport()

#scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/base/')
scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/gas-experiment/')
base.scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/base/')
plot.dir.base <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/plots/')

#scens <- c('fuel-gasoline-price')
#scens <- c('fuel-canola-price','fuel-elec-price','fuel-h2-price','fuel-sorghum-price','fuel-sugarcane-price')
#scens <- c('discount-rate')
#scens <- pp('distrib-',c('h2','bev','phev','flex','ethanol'))
#scens <- pp('vehicle-',c('h2','bev','phev','flex'))
#scens <- c('bev-penetration')
scens <- c('blend-wall')
#scens <- c('base','fuel-gasoline-price','fuel-canola-price','fuel-elec-price','fuel-h2-price','fuel-sorghum-price','fuel-sugarcane-price','discount-rate',pp('distrib-',c('h2','bev','phev','flex','ethanol')),pp('vehicle-',c('h2','bev','phev','flex')),'bev-penetration','blend-wall')
#scens <- c('base')

####################################################################################################
# Configurations
####################################################################################################

make.plots <- F

# tool for pulling economic params
get.param <- function(param.name){
  inputs[['EconomicAssumptions']][Name==param.name,list(Value)]$Value
}
load.inputs <- function(sheet){
  inputs[[sheet]] <<- data.table(read.xls(input.file,sheet,stringsAsFactors=F,skip=1))
}
sheets <- c("MarketStructure","AltFuelPenetrationLimits","FuelTimeSeries","FuelRegionalMarkup","DistributionCosts","VehicleCosts","CarbonIntensity","EconomicAssumptions","CPI","VehicleLifetimeAndVMT")

# my fav color scheme
my.colors <- c(blue='#377eb8',green='#227222',orange='#C66200',purple='#470467',red='#B30C0C',yellow='#C6A600',light.green='#C0E0C0',magenta='#D0339D',dark.blue='#23128F',brown='#542D06',grey='#8A8A8A',light.yellow='#FFE664',light.purple='#9C50C0',light.orange='#FFB164')
alt.colors <- c(BEV='blue',PHEV='dark.blue',H2='red',
                CanolaBiodiesel='light.green',SoyBiodiesel='green',SoyRenewableDiesel='grey',TallowRenewableDiesel='brown',UCOBiodiesel='light.purple',
                SugarcaneEthanol='yellow',CornEthanolDryMill='grey',FlexCornEthanolDryMill='magenta',FlexSorghumEthanol='light.orange',FlexSugarcaneEthanol='light.yellow',SorghumEthanol='orange')
alt.colors <- data.frame(AltKey=names(alt.colors),color=alt.colors,color.hex=my.colors[alt.colors])
alt.type.colors <- c(PEV='blue',H2='red',Biodiesel='green',Ethanol='yellow',Flex='light.yellow')
alt.type.colors <- data.frame(AltKey=names(alt.type.colors),color=alt.type.colors,color.hex=my.colors[alt.type.colors])

amort.veh.cost <- function(veh.cost,tot.vmt,seg.vmt,mj.per.mile){
  lifetime.in.segment <- tot.vmt / seg.vmt
  overnight.cap <- (1-get.param('FractionFinancedVeh')) * veh.cost
  loan.principal <- get.param('FractionFinancedVeh') * veh.cost
  amort.npv <- apply(cbind(loan.principal,overnight.cap,lifetime.in.segment),1,amort.the.loan)
  amort.npv.per.MJ <- amort.npv / seg.vmt / mj.per.mile
  amort.npv.per.MJ
}
amort.the.loan <- function(x){
  loan.principal <- x[1]
  overnight.cap <- x[2]
  lifetime.in.segment <- x[3]
  loan.payments <- rep(loan.principal * get.param('FinanceRateVeh') / (1 - (1+get.param('FinanceRateVeh'))^-get.param('FinanceTermVeh')),get.param('FinanceTermVeh')) 
  loan.payments.disc <- loan.payments / (1 + get.param('DiscountRate'))^(1:(get.param('FinanceTermVeh')))
  amort.npv <- (overnight.cap + sum(loan.payments.disc))*(get.param('DiscountRate')*(1+get.param('DiscountRate'))^lifetime.in.segment)/((1+get.param('DiscountRate'))^lifetime.in.segment - 1)
  amort.npv
}
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
  veh.costs[,MJ.EER.per.mile:=EnergyDemand / SegmentVMT * 1000]
  veh.costs[,cost.per.MJ.EER := amort.veh.cost(VehicleCostAvg,VMTPerLifetime,AggSegmentVMTPerVeh,MJ.EER.per.mile)]
  veh.costs[,cost.per.MJ.EER.ratio := cost.per.MJ.EER / VehicleCostAvg]
  veh.costs[,':='(VehicleCost=VehicleCostAvg * cost.per.MJ.EER.ratio)]
  return(NULL)
}

##########################
# Plot Functions
##########################

plot.mac <- function(the.macc,col.to.plot='MAC',errorbars=F,the.title='',y.lim=500){
    to.use <- streval(pp('the.macc[!is.nan(',col.to.plot,') & !',col.to.plot,'==Inf]'))
    to.use[,AbatementPotential:=AbatementPotential/1e3]

    streval(pp('setkey(to.use,',col.to.plot,')'))
    to.use[,key:=factor(pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel,' - ',County,' - ',VehicleStatus,FuelType,VehicleType))]
    to.use[,key:=factor(to.use$key, to.use$key)] # reorders the levels to match their rank
    to.use[,x.pos:=0.5*(cumsum(AbatementPotential)+cumsum(c(0, head(AbatementPotential,-1))))]

    
    to.use <- streval(pp('to.use[',col.to.plot,'<',y.lim,']'))
    p <- streval(pp('ggplot(to.use,aes(x=x.pos,label=key,width=AbatementPotential,y=',col.to.plot,',fill=AltFuel))'))
    p <- p + geom_bar(stat='identity',position='identity',colour='black',linetype='blank')+
      theme_bw()+
      geom_vline(xintercept=ghg.goal/1e3,color=my.colors['red'],linetype='longdash')+
      scale_fill_manual(values=as.character(alt.colors$color.hex[match(sort(u(to.use$AltFuel)),alt.colors$AltKey)]))

    if(errorbars & pp(col.to.plot,'.sd') %in% names(to.use)){
      max.y <- streval(pp('max(to.use$',col.to.plot,' + to.use$',col.to.plot,'.sd)'))
      p <- p + geom_point()+
      geom_errorbar(aes(ymin=MAC-MAC.sd,ymax=MAC+MAC.sd,width=0))+
      scale_y_continuous(limit=c(-750,max.y))
    }
    if(col.to.plot == 'MAC'){
      p <- p + labs(x='GHG Abatement (1000 tons CO2eq)',y='Marginal Abatement Cost ($/ton CO2eq)',fill='County',title=the.title)  
    }else{
      p <- p + labs(x='GHG Abatement (1000 tons CO2eq)',y='Marginal Intensity Abatement Cost ($1000/(g/MJ))',fill='County',title=the.title)
    }
    print(p)
    p
}

all.stats <- data.table()

scen <- scens[1]
for(scen in scens){
  scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/',scen,'/')
  exper <- list(Factors=list(list(Name="Base",Levels=c('Base'))))
  if(file.exists(pp(scenario.dir,'exper.yaml'))){
    exper <- yaml.load(readChar(pp(scenario.dir,'exper.yaml'),file.info(pp(scenario.dir,'exper.yaml'))$size))
  }
  if("InputFile" %in% names(exper)){
    input.file <- pp(scenario.dir,exper$InputFile)
  }else{
    input.file <- pp(scenario.dir,'AFCI_Portfolio_Inputs.xlsx')
  }
  plot.dir <- pp(plot.dir.base,scen,'/')
  make.dir(plot.dir)

  load(file=pp(scenario.dir,'results.Rdata'))

  level.combs <- results$level.combs
  names(level.combs) <- str_replace_all(names(level.combs)," ","_")
  num.factors <- ncol(level.combs)
  all.avg.maccs <- data.table()

  level.i <- 1
  ####################################################################################################
  # Factorial loop
  ####################################################################################################
  for(level.i in 1:nrow(level.combs)){
    my.cat(pp(pp(names(level.combs),":",level.combs[level.i,]),collapse=' X '))
    inputs<-list()
    for(sheet in sheets){
      load.inputs(sheet)
    }
    for(fac.i in 1:num.factors){
      if('Scale' %in% names(exper$Factors[[fac.i]])){
        sheet <- names(exper$Factors[[fac.i]]$Scale)
        lev.1d.i <- which(exper$Factors[[fac.i]]$Levels == level.combs[level.i,fac.i])

        if('Row' %in% names(exper$Factors[[fac.i]]$Scale[[sheet]])){
          column <- names(exper$Factors[[fac.i]]$Scale[[sheet]])[!names(exper$Factors[[fac.i]]$Scale[[sheet]])=='Row']
          col.names <- names(exper$Factors[[fac.i]]$Scale[[sheet]]$Row)
          row.keys  <- sapply(exper$Factors[[fac.i]]$Scale[[sheet]]$Row,function(x){ x })
          streval(pp('inputs[["',sheet,'"]][',pp(col.names,'=="',row.keys,'"',collapse="&"),',',column,':=',column,'*',exper$Factors[[fac.i]]$Scale[[sheet]][[column]][[lev.1d.i]],']'))
        }else{
          column <- names(exper$Factors[[fac.i]]$Scale[[sheet]])
          streval(pp('inputs[["',sheet,'"]][,',column,':=',column,'*',exper$Factors[[fac.i]]$Scale[[sheet]][[column]][[lev.1d.i]],']'))
        }
      }
    }
    setup.pricing()

    the.macc <- results$the.macc[[level.i]]
    mac.results <- results$mac.results[[level.i]]

    ####################################################
    # Summarize and store the avg MACC and key stats
    ####################################################

    # for incomplete results, constrain the inds
    inds <- which(!is.na(mac.results[1,,2]))

    # Find average cost/rank
    setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel)
    avg.results <- the.macc[,list(VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel,EnergyDemand,GHG)]
    avg.results[,':='(MAC=apply(mac.results[,inds,2],1,mean,na.rm=T),MAC.sd=apply(mac.results[,inds,2],1,sd),MIAC=apply(mac.results[,inds,4],1,mean,na.rm=T),MIAC.sd=apply(mac.results[,inds,4],1,sd),AbatementPotential=apply(mac.results[,inds,3],1,mean))]
    avg.results <- avg.results[!is.nan(MAC) & !MAC==Inf]
    avg.results[,AbatementPotential:=AbatementPotential/1e3]
    avg.results[,rank:=rank(MAC,ties.method='random')]

    n.trials <- dim(mac.results)[2]

    # Report the total cost of the solution and the conventional cost by comparison
    setkey(avg.results,MAC)
    avg.results[,CumulativeAbatement:=cumsum(AbatementPotential)]
    avg.results[,TotalCost:=MAC * AbatementPotential / 1e3] # AbatementPotential is in K-tons, so this puts cost into $M
    mark[,TotalCost:=EnergyDemand*ConventionalFuelCost * 1e3 / 1e6] # puts cost into $M
    avg.results[,Penetration:=AbatementPotential*1e3/GHG]
    avg.results[,EnergyPenetrated:=EnergyDemand*Penetration]
    avg.results[,MJToGalFactor:=c('Gas'=115.63,'Diesel'=134)[FuelType]]
    avg.results[,GalPenetrated:=EnergyPenetrated*1e3/MJToGalFactor]
    stats <- data.table(WtAvgMAC=avg.results[CumulativeAbatement<ghg.goal/1e3,list(WtAvg=weighted.mean(MAC,AbatementPotential))]$WtAvg,NetCost=sum(avg.results[CumulativeAbatement <= ghg.goal/1e3]$TotalCost),BAUCost=sum(mark$TotalCost))
    setkey(avg.results,MAC)
    avg.results[,x.pos:=0.5*(cumsum(AbatementPotential)+cumsum(c(0, head(AbatementPotential,-1))))]
    avg.results[,WtAvgMAC:=stats$WtAvgMAC]

    for(fac.i in 1:num.factors){
      fac.name <- names(level.combs)[fac.i]
      streval(pp('avg.results[,',fac.name,':="',level.combs[level.i,fac.i],'"]'))
      streval(pp('stats[,":="(Factor="',fac.name,'",Level="',level.combs[level.i,fac.i],'")]'))
    }
    all.avg.maccs <- rbindlist(list(all.avg.maccs,avg.results),use.names=T,fill=T)
    all.stats <- rbindlist(list(all.stats,stats),use.names=T,fill=T)
  }

  all.avg.maccs[,key:=factor(pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel))]
  for(fac.i in 1:num.factors){
    fac.name <- names(level.combs)[fac.i]
    streval(pp('all.avg.maccs[,',fac.name,':=factor(',fac.name,',level.combs[,',fac.i,'])]'))
  }

  if(make.plots){
    to.use <- all.avg.maccs[AbatementPotential>0]
    factor.name <- names(level.combs)[1]
    streval(pp('setkey(to.use,',factor.name,',MAC)'))
    to.use[,first.over.target:=CumulativeAbatement >= ghg.goal/1e3]
    streval(pp('to.use[,first.over.target:=(first.over.target==T & c(F,head(first.over.target,-1))==F),by="',factor.name,'"]'))
    to.use <- to.use[CumulativeAbatement <= ghg.goal/1e3 | first.over.target==T]
    setkey(to.use,MAC)
    alt.order <- to.use[,list(key=key[1]),by='key']
    to.use[,key:=factor(key,alt.order$key)]
    #to.use[abs(MAC)>1000,MAC:=1000*sign(MAC)]
    p <- ggplot(to.use,aes(x=x.pos,label=key,width=AbatementPotential,y=MAC,fill=key))+
      geom_bar(stat='identity',position='identity',colour='black',linetype='blank')+
      theme_bw()+
      geom_hline(aes(yintercept=WtAvgMAC),colour=my.colors['grey'],linetype='longdash')+
      geom_vline(xintercept=ghg.goal/1e3,color=my.colors['red'],linetype='longdash')+
      coord_cartesian(xlim = c(-10,ghg.goal/1e3+10),ylim=c(max(-1000,min(to.use$MAC))-20,min(1000,max(to.use$MAC))+20))+
      scale_fill_manual(values=as.character(alt.colors$color.hex[match(sort(u(to.use$key)),alt.colors$AltKey)]))+
      labs(x='GHG Abatement (kt CO2eq)',y='Marginal Abatement Cost ($/t CO2eq)',fill='Alternative',title=pp(exper$Name,': Average MACCs (',n.trials,' Trials)'))
    p <- p + streval(pp('facet_wrap(~',factor.name,')'))
    if(nrow(level.combs)==3){
      ggsave(file=pp(plot.dir,'macc-avg.pdf'),plot=p,width=12,height=4)
    }else if(nrow(level.combs)==4 | nrow(level.combs)==1){
      ggsave(file=pp(plot.dir,'macc-avg.pdf'),plot=p,width=12,height=8)
    }
  }
}

# stats in slightly more useable form for comparing
#cast(melt(all.stats,measure.vars=c('WtAvgMAC','NetCost')),Factor + variable ~ Level)
#cast(melt(all.stats,measure.vars=c('WtAvgMAC','NetCost')),Factor ~ variable + Level)
