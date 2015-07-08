# Also known as SMACC model
####################################################################################################
# Load libs and data
####################################################################################################

load.libraries(c('stringr','truncnorm','reshape2','gdata','mvtnorm','gtools','yaml','GGally'))
installXLSXsupport()

#scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/base/')
scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/gas-experiment/')
base.scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/base/')

#scens <- c('fuel-canola-price','fuel-elec-price')
#scens <- c('fuel-h2-price','fuel-sorghum-price','fuel-sugarcane-price')
#scens <- c('base')
#scens <- c('blend-wall')
#scens <- c('fuel-gasoline-price')
#scens <- c('discount-rate')
#scens <- pp('distrib-',c('h2','bev','phev','flex','ethanol'))
#scens <- pp('vehicle-',c('h2','bev','phev','flex'))
#scens <- c(pp('distrib-',c('h2','bev','phev','flex','ethanol')),pp('vehicle-',c('h2','bev','phev','flex')))
scens <- c('bev-penetration')
#scens <- c('new-scenario')
#scens <- c('fuel-gasoline-price','fuel-canola-price','fuel-elec-price','fuel-h2-price','fuel-sorghum-price','fuel-sugarcane-price','discount-rate',pp('distrib-',c('h2','bev','phev','flex','ethanol')),pp('vehicle-',c('h2','bev','phev','flex')),'bev-penetration','blend-wall')
#scens <- c('test-multi-value')

####################################################################################################
# Configurations and functions
####################################################################################################

n <- 500

# tool for pulling economic params # I'm updating this so it gives numeric values.
get.param <- function(param.name){
  as.numeric(inputs[['EconomicAssumptions']][Name==param.name,list(Value)]$Value)
}
load.inputs <- function(sheet){
  inputs[[sheet]] <<- data.table(read.xls(input.file,sheet,stringsAsFactors=F,skip=1))
}

# This function is used to compare 'next best' alternatives if the LCFS goal isn't met
#winner.i <- which(the.macc$FuelType=='Gas' & the.macc$VehicleType=='LDV' & the.macc$County=='Humboldt' & the.macc$VehicleStatus=='New')
#AltFuel <- the.macc[winner.i]$AltFuel
#MAC <- the.macc[winner.i]$MAC
#PenetrationLimit <- the.macc[winner.i]$PenetrationLimit
#RawGHGA <- the.macc[winner.i]$RawGHGA
#Penetration <- the.macc[winner.i]$Penetration
#EnergyDemand <- the.macc[winner.i]$EnergyDemand

tradeoffs.permute <- function(AltFuel,MAC,PenetrationLimit,RawGHGA,Penetration){
  remaining.inds <- c()
  if(length(MAC)<5){
    perm <- permutations(length(MAC),length(MAC))
  }else{
    perm <- permute.5
    if(length(MAC)>5)remaining.inds <- 6:length(MAC)
  }
  perm <- apply(perm,1,function(inds){ c(inds,remaining.inds) })
  new.opts <- unique(data.table(perm.i=1:ncol(perm),GHGA=apply(perm,2,function(inds){ sum(resolve.pens(PenetrationLimit[inds])*RawGHGA[inds]) }),MAC=apply(perm,2,function(inds){ weighted.mean(MAC[inds],resolve.pens(PenetrationLimit[inds])*RawGHGA[inds]) }),key=c('GHGA','MAC')))
  new.opts
}
tradeoffs.permute.order <- function(perm.i){
  remaining.inds <- c()
  if(length(perm.i)<5){
    perm <- permutations(length(perm.i),length(perm.i))
  }else{
    perm <- permute.5
    if(length(perm.i)>5)remaining.inds <- 6:length(perm.i)
  }
  perm <- apply(perm,1,function(inds){ c(inds,remaining.inds) })
  order(perm[,perm.i[1]])
}
shift.use <- function(use){
  use[which(use)[1]] <- F
  use
}
which.consider <- function(use){
  i <- which(use)
  if(i < length(use)){
    use[i+1]<-T
    use[i]<-F
  }else{
    use[1:length(use)] <- F
  }
  use
}
to.n.tile <- function(x,n.tile=4){
  findInterval(ecdf(x)(x),seq(0,1,by=1/n.tile),rightmost.closed=T)
}
###
# Within each segment we resolve competition for overlapping MJs, setting Penetration to the final value
###
resolve.pens <- function(lims){
  constrained <- lims
  if(length(lims)>1){
    for(j in 2:length(lims)){
      constrained[j] <- (1-sum(constrained[1:(j-1)]))*lims[j]
    }
  }
  constrained
}
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

##############################################################
# End functions, start processing
##############################################################

sheets <- c("MarketStructure","AltFuelPenetrationLimits","FuelTimeSeries","FuelRegionalMarkup","DistributionCosts","VehicleCosts","CarbonIntensity","EconomicAssumptions","CPI","VehicleLifetimeAndVMT")

# my fav color scheme
my.colors <- c(blue='#377eb8',green='#227222',orange='#C66200',purple='#470467',red='#B30C0C',yellow='#C6A600',light.green='#C0E0C0',magenta='#D0339D',dark.blue='#23128F',brown='#542D06',grey='#8A8A8A',light.yellow='#FFE664',light.purple='#9C50C0',light.orange='#FFB164')
alt.colors <- c(BEV='blue',PHEV='dark.blue',H2='red',
                CanolaBiodiesel='light.green',SoyBiodiesel='green',SoyRenewableDiesel='grey',TallowRenewableDiesel='brown',UCOBiodiesel='light.purple',
                SugarcaneEthanol='yellow',CornEthanolDryMill='grey',FlexCornEthanolDryMill='magenta',FlexSorghumEthanol='light.orange',FlexSugarcaneEthanol='light.yellow',SorghumEthanol='orange')
alt.colors <- data.frame(AltKey=names(alt.colors),color=alt.colors,color.hex=my.colors[alt.colors])
alt.type.colors <- c(PEV='blue',H2='red',Biodiesel='green',Ethanol='yellow',Flex='light.yellow')
alt.type.colors <- data.frame(AltKey=names(alt.type.colors),color=alt.type.colors,color.hex=my.colors[alt.type.colors])

permute.5 <- permutations(5,5)

scen <- scens[1]
for(scen in scens){
  scenario.dir <- pp(afrp.shared,'Task-2.1-AFI-Deployment-Assessment/AFCI_Porfolio_Model/',scen,'/')

  exper <- list(Factors=list(list(Name="Base",Levels=c('Base'))))
  if(file.exists(pp(scenario.dir,'exper.yaml'))){
    exper <- yaml.load(readChar(pp(scenario.dir,'exper.yaml'),file.info(pp(scenario.dir,'exper.yaml'))$size))
  }
  num.factors <- length(exper$Factors)
  level.combs <- expand.grid(lapply(exper$Factors,function(fac){ fac$Levels }),stringsAsFactors=F)
  names(level.combs) <- unlist(lapply(exper$Factors,function(fac){ fac$Name }))

  if("InputFile" %in% names(exper)){
    input.file <- pp(scenario.dir,exper$InputFile)
  }else{
    input.file <- pp(scenario.dir,'AFCI_Portfolio_Inputs.xlsx')
  }
  inputs <- list()
  results <- list(level.combs=level.combs,the.macc=list(),mac.results=list())
  ####################################################################################################
  # Factorial loop: ANDY - LOOK HERE
  ####################################################################################################
  level.i <- 1
  for(level.i in 1:nrow(level.combs)){
  
    ####################################################################################################
  	# As it stands, this loop will handle multiple factors. However, it needs to also handle if we change
  	# up the 'values' part of the experiment.
 	####################################################################################################

    my.cat(pp(pp(names(level.combs),":",level.combs[level.i,]),collapse=' X '))

    if(all(level.combs[level.i,]=='Base') & file.exists(pp(base.scenario.dir,'base-results.Rdata'))){
      # Save 10 minutes and load the results instead of redoing them
      load(file=pp(base.scenario.dir,'base-results.Rdata'))
      
      results$the.macc[[level.i]] <- base.results$the.macc[[1]]
      results$mac.results[[level.i]] <- base.results$mac.results[[1]]

    }else{

      for(sheet in sheets){
        load.inputs(sheet)
      }

      for(fac.i in 1:num.factors){
        #fac.name <- names(level.combs)[fac.i]
        
        # If we want to include multiple scales in any factors, we will split them among 'values'.
        # Unfortunately, this will add another layer to the file structure that does not otherwise exist.
        # If there is a way to accomodate this without a separate 'scale/row' code block, I am all ears.
        
        if('Values' %in% names(exper$Factors[[fac.i]])){
          # Subcategories in 'Values' won't have names, so we iterate of length
          for(value.i in 1:length(exper$Factors[[fac.i]]$Values)){
            if('Scale' %in% names(exper$Factors[[fac.i]]$Values[[value.i]])){
              sheet <- names(exper$Factors[[fac.i]]$Values[[value.i]]$Scale)
              lev.1d.i <- which(exper$Factors[[fac.i]]$Levels == level.combs[level.i,fac.i])
              # What is going on here, and how do we translate to the new structure
              if('Row' %in% names(exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]])){
                column <- names(exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]])[!names(exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]])=='Row']
                col.names <- names(exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]]$Row)
                row.keys  <- sapply(exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]]$Row,function(x){ x })
                streval(pp('inputs[["',sheet,'"]][',pp(col.names,'=="',row.keys,'"',collapse="&"),',',column,':=',column,'*',exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]][[column]][[lev.1d.i]],']'))
              }else{
                column <- names(exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]])
                streval(pp('inputs[["',sheet,'"]][,',column,':=',column,'*',exper$Factors[[fac.i]]$Values[[value.i]]$Scale[[sheet]][[column]][[lev.1d.i]],']'))
              } # end 'row' if/else
            } # end 'scale' if
          } # end for loop
        } # end 'Values' if
        
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
          } # end 'row' if/else
        } # end 'scale' if
      }

      ####################################################################################################
      # Preprocessing inputs
      ####################################################################################################

      mark <- inputs[['MarketStructure']]
      setkey(mark,County,FuelType,VehicleType,VehicleStatus)
 	  
      mark[,':='(NumVehicles=as.numeric(gsub(',','',NumVehicles)),SegmentVMT=as.numeric(gsub(',','',SegmentVMT)),EnergyDemand=as.numeric(gsub(',','',EnergyDemand)))]
      mark[,Intensity:=GHG*1e6/(EnergyDemand*1e3)] # carbon intensity of the fuels in g/MJ
      ghg.goal <- sum(mark$GHG) - (sum(mark[FuelType=='Gas']$EnergyDemand*1e3)*as.numeric(get.param('TargetGasolineIntensity')) + sum(mark[FuelType=='Diesel']$EnergyDemand*1e3)*as.numeric(get.param('TargetDieselIntensity')))/1e6 # 238K tons

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
      ####################################################################################################
      # Determine the energy of each of the components in the given flex fuel blend; update flex.carb
      ####################################################################################################
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


	  ####################################################################################################
	  # To delete unless we realize what this was for....
	  ####################################################################################################
      #file.name <- list.files(scenario.dir,pattern=".csv$")[1]

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
	  ####################################################################################################
	  # Check out use of 'list' in the next line
      ####################################################################################################
      
      fuel.price <- fuel.price.m[Year>=2010,list(PriceAvg=mean(value,na.rm=T)),by='variable']
      
      fuel.price[,FuelType:=carb$AltFuelType[match(variable,carb$AltFuel)]]
      fuel.price[,':='(AltFuel=variable,variable=NULL)]
      fuel.price[,Replaces:='None']
      fuel.price[FuelType=='Gas' | FuelType=='Ethanol',Replaces:='Gas']
      fuel.price[FuelType=='Diesel' | FuelType=='Biodiesel',Replaces:='Diesel']
      fuel.price[FuelType=='H2',Replaces:='H2']
      setkey(fuel.price,Replaces)
      fuel.price.rvs <- rmvnorm(n,fuel.price$PriceAvg,sigma=fuel.var)

      ###
      # Station amortization
      # Find the ratio of station capacity cost to amortized station cost in 2014$ / MJ
      # (note that this relationship is linear so we do the calc once and use the ratio during the sim)
      ###
      ####################################################################################################
	  # Quick explanation of these variables would probably be helpful. What they are, not how they are calculated.
      ####################################################################################################
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
      ####################################################################################################
	  # Get rid of commas in veh.costs
      ####################################################################################################      
      veh.costs[,':='(VehicleCostAvg=as.numeric(gsub(',','',VehicleCostAvg)),VehicleCostSD=as.numeric(gsub(',','',VehicleCostSD)))]
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

      i<-1
      for(i in 1:n){
        if(i%%50==0)my.cat(i)
        ###
        # Blend Fuels and Dissagregate Costs to the County
        ###
        fuel.price.rv <- copy(fuel.price)
        #############################################################################
        # Take 'i'th row of fuel.price.rvs. Presuming this is time-dependent price?
        #############################################################################
        fuel.price.rv[,PriceRV:=fuel.price.rvs[i,]]
		elec.price <- fuel.price.rv[AltFuel=='Electricity']$PriceRV
        gas.price <- fuel.price.rv[AltFuel=='Gas']$PriceRV
        diesel.price <- fuel.price.rv[AltFuel=='Diesel']$PriceRV
        # Blend Flex and Ethanol
        setkey(fuel.price.rv,FuelType,AltFuel)
        flex.prices <- copy(fuel.price.rv[FuelType=='Ethanol'])
        flex.prices[,':='(FuelType='Flex',PriceRV=PriceRV*flex.blend.by.energy$flex+gas.price*flex.blend.by.energy$gas)]
        fuel.price.rv[FuelType=='Ethanol',':='(PriceRV=PriceRV*eth.blend.by.energy$eth+gas.price*eth.blend.by.energy$gas)]
        # Blend PHEV
        phev.price <- data.table(AltFuel='PHEV',FuelType='PHEV',Replaces='None',PriceRV=elec.price * get.param('PHEVBlend') + gas.price * (1 - get.param('PHEVBlend')))
        # Blend Biodiesel
        fuel.price.rv[FuelType=='Biodiesel',':='(PriceRV=PriceRV*get.param('BiodieselBlend')+diesel.price*(1-get.param('BiodieselBlend')))]
        # Rename Elec to BEV
        fuel.price.rv[AltFuel=='Electricity',':='(FuelType='BEV',AltFuel='BEV')]
        fuel.price.rv <- rbindlist(list(fuel.price.rv,flex.prices,phev.price),use.names=T,fill=T)

        setkey(fuel.price.rv,Replaces)
        fuel.price.by.county <- fuel.markup[fuel.price.rv,allow.cart=T]
        fuel.price.by.county[is.na(Markup),Markup:=0]
        fuel.price.by.county[,FuelCost:=PriceRV+Markup]

        # Now EER weight the cost using the LDV value for H2
        setkey(carb,AltFuel,IntensityEER) # need this to get H2 in correct order
        ################################################################
        # Twin 'setkey' commands seem superfluous
        ################################################################
        setkey(carb,AltFuel)
        setkey(fuel.price.by.county,AltFuel)
        fuel.price.by.county <- unique(carb)[,list(AltFuel,EER)][fuel.price.by.county]
        fuel.price.by.county[,FuelCost:=FuelCost/EER]

        # Join the conventional prices to market segments for later use in making the fuel cost incremental
        mark.rv <- copy(mark)
        setkey(mark.rv,County,FuelType)
        setkey(fuel.price.by.county,County,FuelType)
        mark.rv <- fuel.price.by.county[mark.rv,list(County,FuelType,VehicleType,VehicleStatus,NumVehicles,SegmentVMT,EnergyDemand,GHG,travel.demand,FuelCost)]
        mark.rv[,':='(ConventionalFuelCost=FuelCost,FuelCost=NULL)]

        # Now drop the conventionals
        fuel.price.by.county <- fuel.price.by.county[FuelType!="Gas" & FuelType!="Diesel"]
        fuel.price.by.county[,':='(AltFuelType=FuelType,FuelType=NULL)]

        ###
        # Sample and amortized the station cost into 2014$ / MJ
        ###
        ################################################################
        # So station.costs will be random based on rnorm, correct?
        ################################################################        
        station.costs[,DistCostRV:=rnorm(length(AltFuelType),DistCostAvg,DistCostSD)]
        station.costs[,StationCost:=DistCostRV * amort.dist.ratio]
        setkey(carb,AltFuelType,IntensityEER) # need this to get H2 in correct order
        setkey(carb,AltFuelType)
        setkey(station.costs,AltFuelType)
        station.costs <- unique(carb)[,list(AltFuelType,EER)][station.costs]
        station.costs[,StationCost:=StationCost/EER]

        ###
        # Sample and amortize the vehicle costs into 2014$ / MJ
        ###
        veh.costs[,':='(VehicleCost=rnorm(length(County),VehicleCostAvg,VehicleCostSD) * cost.per.MJ.EER.ratio)]
        
        ###
        # Sum the costs together within the context of each market segment
        # note that conventional fuel cost will be reletive to the segment and subtracted out after the join
        ###
        setkey(station.costs,AltFuelType) 
        setkey(veh.costs,AltFuelType) 
        all.costs <- station.costs[,list(AltFuelType,StationCost)][veh.costs[,list(AltFuelType,County,FuelType,VehicleType,VehicleCost)]]
        setkey(all.costs,AltFuelType,County)
        setkey(fuel.price.by.county,AltFuelType,County)
        all.costs <- all.costs[fuel.price.by.county[,list(AltFuelType,County,AltFuel,FuelCost)],allow.cart=T]
        all.costs[is.na(StationCost),StationCost:=0]
        all.costs[is.na(VehicleCost),VehicleCost:=0]
        all.costs[,SubtotalCost:=StationCost+VehicleCost+FuelCost]

        all.costs.with.FuelType <- all.costs[!is.na(FuelType)]
        all.costs.without.FuelType <- all.costs[is.na(FuelType)]
        setkey(all.costs.with.FuelType,FuelType,VehicleType,County)
        setkey(mark.rv,FuelType,VehicleType,County)

        all.costs.with.FuelType <- mark.rv[,list(FuelType,VehicleType,County,VehicleStatus,ConventionalFuelCost,EnergyDemand,GHG)][all.costs.with.FuelType[,list(AltFuelType,FuelType,VehicleType,County,AltFuel,StationCost,VehicleCost,FuelCost,SubtotalCost)],allow.cart=T]
        setkey(all.costs.without.FuelType,County)
        setkey(mark.rv,County)
        all.costs.without.FuelType <- mark.rv[,list(FuelType,VehicleType,County,VehicleStatus,ConventionalFuelCost,EnergyDemand,GHG)][all.costs.without.FuelType[,list(AltFuelType,FuelType,VehicleType,County,AltFuel,StationCost,VehicleCost,FuelCost,SubtotalCost)],allow.cart=T]

        all.costs <- rbindlist(list(all.costs.with.FuelType,all.costs.without.FuelType),use.names=T,fill=T)[,list(AltFuelType,AltFuel,FuelType,VehicleType,County,VehicleStatus,ConventionalFuelCost,StationCost,VehicleCost,FuelCost,SubtotalCost,EnergyDemand,GHG)]
        all.costs[,':='(TotalCost=SubtotalCost - ConventionalFuelCost)]

        # At this point, NA's mean we have incomplete data in the inputs, drop now
        all.costs <- na.omit(all.costs)

        ###
        # Now we can join the penetration limits
        # the 'ALL' keyword is used to indicate a join NOT across a particular factor, to deal with this we unfortunately have to 
        # build up from the bottom in terms of number of rows with ALL
        ###
        
        ################################################
        # I'm not super clear what this segment does
        ################################################
        facts <- c('County','FuelType','VehicleType','VehicleStatus','AltFuel')

        setkey(all.costs,County,FuelType,VehicleType,VehicleStatus,AltFuel,AltFuelType)
        setkey(pens,County,FuelType,VehicleType,VehicleStatus,AltFuel,AltFuelType)
        pen.rows <- which(streval(pp('pens$',facts,' != \'ALL\'',collapse=' & ')))
        all.costs.and.pens <- all.costs[pens[pen.rows]]

        for(fact in facts){
          facts.left <- facts[!fact==facts]
          key.str <- pp(facts.left,collapse=',')
          streval(pp('setkey(all.costs,AltFuelType,',key.str,')'))
          streval(pp('setkey(pens,AltFuelType,',key.str,')'))
          pen.rows <- which(streval(pp('pens$',fact,' == \'ALL\' & ',pp('pens$',facts.left,' != \'ALL\'',collapse=' & '))))
          if(length(pen.rows)>0){
            all.costs.and.pens <- rbindlist(list(all.costs.and.pens,all.costs[pens[pen.rows]]),use.names=T,fill=T)
          }
        }
        combs.of.2 <- combinations(5,2,facts)
        for(fact.i in 1:nrow(combs.of.2)){
          fact <- combs.of.2[fact.i,1]
          fact.2 <- combs.of.2[fact.i,2]
          facts.left.2 <- facts[-match(c(fact,fact.2),facts)]
          key.str <- pp(facts.left.2,collapse=',')
          streval(pp('setkey(all.costs,AltFuelType,',key.str,')'))
          streval(pp('setkey(pens,AltFuelType,',key.str,')'))
          pen.rows <- which(streval(pp(pp('pens$',c(fact,fact.2),' == \'ALL\'',collapse=' & '),' & ',pp('pens$',facts.left.2,' != \'ALL\'',collapse=' & '))))
          if(length(pen.rows)>0){
            all.costs.and.pens <- rbindlist(list(all.costs.and.pens,all.costs[pens[pen.rows]]),use.names=T,fill=T)
          }
        }
        combs.of.3 <- combinations(5,3,facts)
        for(fact.i in 1:nrow(combs.of.3)){
          fact <- combs.of.3[fact.i,1]
          fact.2 <- combs.of.3[fact.i,2]
          fact.3 <- combs.of.3[fact.i,3]
          facts.left.3 <- facts[-match(c(fact,fact.2,fact.3),facts)]
          key.str <- pp(facts.left.3,collapse=',')
          streval(pp('setkey(all.costs,AltFuelType,',key.str,')'))
          streval(pp('setkey(pens,AltFuelType,',key.str,')'))
          pen.rows <- which(streval(pp(pp('pens$',c(fact,fact.2,fact.3),' == \'ALL\'',collapse=' & '),' & ',pp('pens$',facts.left.3,' != \'ALL\'',collapse=' & '))))
          if(length(pen.rows)>0){
            all.costs.and.pens <- rbindlist(list(all.costs.and.pens,all.costs[pens[pen.rows]]),use.names=T,fill=T)
          }
        }
        combs.of.4 <- combinations(5,4,facts)
        for(fact.i in 1:nrow(combs.of.4)){
          fact <- combs.of.4[fact.i,1]
          fact.2 <- combs.of.4[fact.i,2]
          fact.3 <- combs.of.4[fact.i,3]
          fact.4 <- combs.of.4[fact.i,4]
          facts.left.4 <- facts[-match(c(fact,fact.2,fact.3,fact.4),facts)]
          key.str <- pp(facts.left.4,collapse=',')
          streval(pp('setkey(all.costs,AltFuelType,',key.str,')'))
          streval(pp('setkey(pens,AltFuelType,',key.str,')'))
          pen.rows <- which(streval(pp(pp('pens$',c(fact,fact.2,fact.3,fact.4),' == \'ALL\'',collapse=' & '),' & ',pp('pens$',facts.left.4,' != \'ALL\'',collapse=' & '))))
          if(length(pen.rows)>0){
            all.costs.and.pens <- rbindlist(list(all.costs.and.pens,all.costs[pens[pen.rows]]),use.names=T,fill=T)
          }
        }
        setkey(all.costs,AltFuelType)
        setkey(pens,AltFuelType)
        pen.rows <- which(streval(pp('pens$',facts,' == \'ALL\'',collapse=' & ')))
        if(length(pen.rows)>0){
          all.costs.and.pens <- rbindlist(list(all.costs.and.pens,all.costs[pens[pen.rows]]),use.names=T,fill=T)
        }
        all.costs.and.pens[,':='(i.AltFuel=NULL,i.FuelType=NULL,i.County=NULL,i.VehicleType=NULL,i.VehicleStatus=NULL)]


#########################################################
        ###
        # Calc MAC
        ###
        setkey(carb,AltFuelType,AltFuel,VehicleType,County)
        setkey(all.costs.and.pens,AltFuelType,AltFuel,VehicleType,County)
        carb.rows <- which(carb$County != 'ALL' & carb$VehicleType != 'ALL' & !carb$AltFuelType %in% c('Gas','Diesel'))
        if(length(carb.rows)>0){
          the.macc <- na.omit(all.costs.and.pens[carb[,list(AltFuelType,AltFuel,VehicleType,County,Intensity,EER)][carb.rows]])
        }
        setkey(carb,AltFuelType,AltFuel,VehicleType)
        setkey(all.costs.and.pens,AltFuelType,AltFuel,VehicleType)
        carb.rows <- which(carb$County == 'ALL' & carb$VehicleType != 'ALL' & !carb$AltFuelType %in% c('Gas','Diesel'))
        if(length(carb.rows)>0){
          the.macc <- rbindlist(list(the.macc,na.omit(all.costs.and.pens[carb[,list(AltFuelType,AltFuel,VehicleType,County,Intensity,EER)][carb.rows]])),use.names=T,fill=T)
        }
        setkey(carb,AltFuelType,AltFuel,County)
        setkey(all.costs.and.pens,AltFuelType,AltFuel,County)
        carb.rows <- which(carb$County != 'ALL' & carb$VehicleType == 'ALL' & !carb$AltFuelType %in% c('Gas','Diesel'))
        if(length(carb.rows)>0){
          the.macc <- rbindlist(list(the.macc,na.omit(all.costs.and.pens[carb[,list(AltFuelType,AltFuel,VehicleType,County,Intensity,EER)][carb.rows]])),use.names=T,fill=T)
        }
        setkey(carb,AltFuelType,AltFuel)
        setkey(all.costs.and.pens,AltFuelType,AltFuel)
        carb.rows <- which(carb$County == 'ALL' & carb$VehicleType == 'ALL' & !carb$AltFuelType %in% c('Gas','Diesel'))
        if(length(carb.rows)>0){
          the.macc <- rbindlist(list(the.macc,na.omit(all.costs.and.pens[carb[,list(AltFuelType,AltFuel,VehicleType,County,Intensity,EER)][carb.rows]])),use.names=T,fill=T)
        }
        the.macc[,':='(i.VehicleType=NULL,i.County=NULL)]

        the.macc[,RawGHGA := GHG - EnergyDemand*1e3*Intensity/EER/1e6]
        the.macc[RawGHGA<0,RawGHGA := 0]
        # MIAC is Marginal Intensity Abatement Cost
        market.energy.demand <- sum(mark$EnergyDemand)
        #the.macc[,MIAC := TotalCost*EnergyDemand*1e3/(RawGHGA*1e6/(EnergyDemand*1e3))/1e3] # units are 1000 $ / (g/MJ abated)
        the.macc[,MIAC := TotalCost*EnergyDemand*1e3/(RawGHGA*1e6/(market.energy.demand*1e3))/1e3] # units are 1000 $ / (g/MJ abated)
        the.macc[MIAC==-Inf,MIAC := Inf]
        the.macc[,MAC := TotalCost*EnergyDemand*1e3/RawGHGA]
        the.macc[MAC==-Inf,MAC := Inf]

        setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,MAC)
        the.macc[,Penetration:=resolve.pens(PenetrationLimit),by=c('VehicleStatus','VehicleType','FuelType','County')]
        the.macc[,AbatementPotential:=Penetration*RawGHGA]

        if(sum(the.macc$AbatementPotential) < ghg.goal){
          #if(sum(the.macc$AbatementPotential) < 80e3)break
          the.metrics <- the.macc[,tradeoffs.permute(AltFuel,MAC,PenetrationLimit,RawGHGA,Penetration),by=c('VehicleStatus','VehicleType','FuelType','County')]
          the.metrics[,key:=pp(VehicleStatus,VehicleType,FuelType,County)]
          setkey(the.metrics,MAC,key)
          the.metrics <- the.metrics[GHGA>0 & !is.nan(MAC)]
          the.metrics[,':='(use=c(T,rep(F,length(MAC)-1)),consider=T,order=1:length(MAC)),by=key]

          #prev <- sum(the.metrics[the.metrics$use]$GHGA)
          #while(sum(the.metrics[the.metrics$use]$GHGA)<ghg.goal & any(the.metrics$consider)){
          while(sum(the.metrics[the.metrics$use]$GHGA)<ghg.goal & any(the.metrics$consider)){
            the.metrics[,consider:=which.consider(use),by=key]
            the.metrics[the.metrics$consider,use:=c(T,rep(F,length(MAC)-1))]
            winner.key <- the.metrics[the.metrics$consider & the.metrics$use]$key
            winner.inds <- which(the.metrics$key==winner.key)
            num.true <- nrow(the.metrics[winner.inds][use==T])
            if(num.true==2 && diff(the.metrics[winner.inds][use==T]$GHGA)>0){
              # this actually increased GHGA, otherwise remove 
              the.metrics[winner.inds,use:=shift.use(use)]
            }else if(num.true==2){
              #my.cat('no improvement')
              winner.order <- the.metrics[winner.inds]$order[which(the.metrics[winner.inds]$use)[2]]
              the.metrics <- the.metrics[!(key==winner.key & order==winner.order)] 
            }
            #my.cat(round(sum(the.metrics[the.metrics$use]$GHGA))/1e3)
            #if(sum(the.metrics[the.metrics$use]$GHGA)<prev)break
            #if(sum(the.metrics[the.metrics$use]$GHGA)>129e3)break
            #prev <- sum(the.metrics[the.metrics$use]$GHGA)
            #my.cat(sum(the.metrics$use))
            #print(summary(the.metrics[the.metrics$use==T]$GHGA))
            #print(the.metrics[use==T]$GHGA)
            #print(the.metrics[1:40])
          }
          setkey(the.metrics,VehicleStatus,VehicleType,FuelType,County)
          the.macc <- the.metrics[the.metrics$use,list(VehicleStatus,VehicleType,FuelType,County,perm.i)][the.macc]
          the.macc[,NewOrder:=tradeoffs.permute.order(perm.i),by=c('VehicleStatus','VehicleType','FuelType','County')]

          setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,NewOrder)
          the.macc[,Penetration:=resolve.pens(PenetrationLimit),by=c('VehicleStatus','VehicleType','FuelType','County')]
          the.macc[,AbatementPotential:=Penetration*RawGHGA]
          #break
          #as.data.frame(the.macc[,list(AltFuel,VehicleStatus,VehicleType,FuelType,County,MAC,Penetration,AbatementPotential)])
        }
        
        setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel)
        if(i==1){
          mac.results <- array(NA,c(nrow(the.macc),n,4))
        }
        mac.results[,i,1] <- rank(the.macc$MAC,ties.method='random')
        mac.results[,i,2] <- the.macc$MAC
        mac.results[,i,3] <- the.macc$AbatementPotential
        mac.results[,i,4] <- the.macc$MIAC
        if(abs(weighted.mean(the.macc$MAC, the.macc$AbatementPotential) - 220) < 5)break
        
########################################################################################################
# Seems like we are missing some closing brackets - they do not match their comments.
########################################################################################################
        
      } # end for each i

      #write.csv(avg.results,file=pp(scenario.dir,'avg-mac-table.csv'))

      results$the.macc[[level.i]] <- the.macc
      results$mac.results[[level.i]] <- mac.results
    } # end if base / not base
    save(results,file=pp(scenario.dir,'results.Rdata'))
    
  } # end for each level combination
  
} # end for each experiment / scenario

# seg <- the.macc[FuelType=='Gas' & VehicleType=='LDV' & County=='Humboldt' & VehicleStatus=='New']
# AbatementPotential <- seg$AbatementPotential
# PenetrationLimit <- seg$PenetrationLimit
# Penetration <- seg$Penetration
# MAC <- seg$MAC
# MIAC <- seg$MIAC
# RawGHGA <- seg$RawGHGA

##########################
# Plots
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

if(F){
  load(file=pp(scenario.dir,'results.Rdata'))

  level.combs <- results$level.combs
  for(level.i in 1:nrow(level.combs)){
    the.macc <- results$the.macc[[level.i]]
    mac.results <- results$mac.results[[level.i]]

    ##########################
    # Summarize the results and output
    ##########################

    ###
    # MAC
    ###
    # Find average cost/rank
    inds <- !is.na(mac.results[1,,2])
    setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel)
    avg.results <- the.macc[,list(VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel,EnergyDemand,GHG)]
    avg.results[,':='(MAC=apply(mac.results[,inds,2],1,mean,na.rm=T),MAC.sd=apply(mac.results[,inds,2],1,sd),MIAC=apply(mac.results[,inds,4],1,mean,na.rm=T),MIAC.sd=apply(mac.results[,inds,4],1,sd),AbatementPotential=apply(mac.results[,inds,3],1,mean))]
    avg.results <- avg.results[!is.nan(MAC) & !MAC==Inf]
    avg.results[,AbatementPotential:=AbatementPotential/1e3]
    avg.results[,rank:=rank(MAC,ties.method='random')]

    # Report the total cost of the solution and the conventional cost by comparison
    setkey(avg.results,MAC)
    avg.results[,CumulativeAbatement:=cumsum(AbatementPotential)]
    avg.results[,TotalCost:=MAC * AbatementPotential / 1e3] # AbatementPotential is in K-tons, so this puts cost into $M
    my.cat(pp('Net Cost of Solution: ',sum(avg.results[CumulativeAbatement <= ghg.goal/1e3]$TotalCost)))
    mark.rv[,TotalCost:=EnergyDemand*ConventionalFuelCost * 1e3 / 1e6] # puts cost into $M

    mark.veh <- copy(mark)
    conv.veh.cost <- inputs[['EconomicAssumptions']][Name=="GasVehAve",Value]
    setkey(mark.veh,FuelType,VehicleType)
    setkey(tot.vmt,FuelType,VehicleType)
    mark.veh <- tot.vmt[mark.veh]
    mark.veh[,MJ.per.mile:=EnergyDemand / SegmentVMT * 1000]
    mark.veh[,cost.per.MJ := amort.veh.cost( conv.veh.cost,VMTPerLifetime,SegmentVMT,MJ.per.mile)]
    mark.veh[,tot.cost:=cost.per.MJ * EnergyDemand*1000]
    bau.cost <- sum(mark.rv$TotalCost) + sum(mark.veh$tot.cost)/1e6

    my.cat(pp('Cost of BAU: ',bau.cost))
    my.cat(pp('Cost as % of BAU: ',sum(avg.results[CumulativeAbatement <= ghg.goal/1e3]$TotalCost)*100/bau.cost))
    mark.veh[,Gal:=EnergyDemand*1e3/c('Gas'=115.63,'Diesel'=134)[FuelType]]
    my.cat(pp('Cost of solution on $/GGE basis: ',bau.cost*1e6/sum(mark.veh$Gal)))

    avg.results[,Penetration:=AbatementPotential*1e3/GHG]
    avg.results[,EnergyPenetrated:=EnergyDemand*Penetration]
    avg.results[,MJToGalFactor:=c('Gas'=115.63,'Diesel'=134)[FuelType]]
    avg.results[,GalPenetrated:=EnergyPenetrated*1e3/MJToGalFactor]
    my.cat(pp('Cost of solution on $/GGE basis: ',1e6*sum(avg.results[CumulativeAbatement <= ghg.goal/1e3]$TotalCost)/sum(avg.results[CumulativeAbatement <= ghg.goal/1e3]$GalPenetrated)))
    my.cat(pp('Cost of solution on $/GGE basis spread over all BAU gallons: ',1e6*sum(avg.results[CumulativeAbatement <= ghg.goal/1e3]$TotalCost)/sum(mark.veh$Gal)))

    WtAvgMAC <- avg.results[CumulativeAbatement<ghg.goal/1e3,list(WtAvg=weighted.mean(MAC,AbatementPotential))]$WtAvg

    # Consolidate matching MACs (biofuels)
    #setkey(avg.results,rank,County)
    #avg.results[,MAC:=round(MAC,3)]
    #setkey(avg.results,MAC)
    #dups <- avg.results[duplicated(avg.results)]
    #all.dups <- avg.results[avg.results$MAC %in% dups$MAC]
    #setkey(all.dups,MAC,County)
    #consol <- all.dups[,list(AbatementPotential=sum(AbatementPotential),AltFuelType=AltFuelType[1],AltFuel=AltFuel[1],VehicleStatus='ALL',FuelType='ALL',VehicleType='ALL',MAC=MAC[1]),by=c('MAC','County')]
    #avg.results <- rbindlist(list(avg.results[!avg.results$MAC %in% dups$MAC],consol),use.names=T,fill=T)[AbatementPotential>0]

    # Plot avg results as a bar plot with error bars for +/-1SD
    setkey(avg.results,MAC)
    #avg.results[,key:=factor(pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel,' - ',County,' - ',VehicleStatus,FuelType,VehicleType))]
    avg.results[,key:=factor(pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel))]
    avg.results[,x.pos:=0.5*(cumsum(AbatementPotential)+cumsum(c(0, head(AbatementPotential,-1))))]

    to.use <- avg.results[AbatementPotential>0]
    setkey(to.use,MAC)
    to.use[,first.over.target:=CumulativeAbatement >= ghg.goal/1e3]
    to.use[,first.over.target:=(first.over.target==T & c(F,head(first.over.target,-1))==F)]
    to.use <- to.use[CumulativeAbatement <= ghg.goal/1e3 | first.over.target==T]
    alt.order <- to.use[,list(key=key[1]),by='key']
    to.use[,key:=factor(key,alt.order$key)]

    #to.use <- avg.results[AbatementPotential>0]
    max.y <- max(to.use$MAC + to.use$MAC.sd)
    ggplot(to.use,aes(x=x.pos,label=key,width=AbatementPotential,y=MAC,fill=key))+
      geom_bar(stat='identity',position='identity',colour='black',linetype='blank')+
      #geom_point()+
      #geom_errorbar(aes(ymin=MAC-MAC.sd,ymax=MAC+MAC.sd,width=0))+
      theme_bw()+
      geom_hline(yintercept=WtAvgMAC,colour=my.colors['grey'],linetype='longdash')+
      geom_vline(xintercept=ghg.goal/1e3,color=my.colors['red'],linetype='longdash')+
      scale_fill_manual(values=as.character(alt.colors$color.hex[match(sort(u(to.use$key)),alt.colors$AltKey)]))+
      labs(x='GHG Abatement (1000 tons CO2eq)',y='Marginal Abatement Cost ($/ton CO2eq)',fill='County',title='Average MACC (500 Trials)')

    # Now summarize the average MACC in terms of the number of vehicles
    to.use <- avg.results[AbatementPotential>0]
    setkey(to.use,MAC)
    to.use[,first.over.target:=CumulativeAbatement >= ghg.goal/1e3]
    to.use[,first.over.target:=(first.over.target==T & c(F,head(first.over.target,-1))==F)]
    to.use <- to.use[CumulativeAbatement <= ghg.goal/1e3 | first.over.target==T]
    alt.order <- to.use[,list(key=key[1]),by='key']
    to.use[,key:=factor(key,alt.order$key)]
    setkey(mark,County, FuelType, VehicleType,VehicleStatus)
    setkey(to.use,County, FuelType, VehicleType,VehicleStatus)
    to.use <- mark[,list(County, FuelType, VehicleType,VehicleStatus,NumVehicles)][to.use]
    to.use[,NumVehiclesPenetrated:=NumVehicles*Penetration]
    setkey(to.use,MAC)
    to.use[,x.pos:=0.5*(cumsum(NumVehiclesPenetrated)+cumsum(c(0, head(NumVehiclesPenetrated,-1))))]
    ggplot(to.use,aes(x=County,y=NumVehiclesPenetrated,fill=key,colour=FuelType))+
      geom_bar(stat='identity')+
      theme_bw()+
      scale_colour_manual(values=as.character(alt.type.colors$color.hex[alt.type.colors$color%in%c('blue','green')]))+
      scale_fill_manual(values=as.character(alt.colors$color.hex[match(sort(u(to.use$key)),alt.colors$AltKey)]))+
      facet_grid(VehicleType~VehicleStatus)+
      theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'))+
      labs(x='Region',y='Number of Vehicles')
    ggplot(mark,aes(x=County,y=NumVehiclesPenetrated,fill=FuelType))+
      geom_bar(stat='identity')+
      theme_bw()+
      scale_fill_manual(values=as.character(alt.type.colors$color.hex[alt.type.colors$color%in%c('blue','green')]))+
      facet_grid(VehicleType~VehicleStatus)+
      theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'))+
      labs(x='Region',y='Number of Vehicles')
    ggplot(to.use,aes(x=x.pos,label=key,width=NumVehiclesPenetrated,y=MAC,fill=key))+
      geom_bar(stat='identity',position='identity',colour='black',linetype='blank')+
      theme_bw()+
      scale_fill_manual(values=as.character(alt.colors$color.hex[match(sort(u(to.use$key)),alt.colors$AltKey)]))+
      labs(x='Number of Vehicles (1000 Vehicles)',y='Marginal Abatement Cost ($/ton CO2eq)',fill='County',title='Average MACC (500 Trials)')

    ### 
    # Make a prettier correlation plot
    ###
    fuel.ts <- h(inputs[['FuelTimeSeries']],nrow(inputs[['FuelTimeSeries']])-2)
    fuel.ts[,':='(Year=NULL,Quarter=NULL,CornEthanolDryMill=NULL,CornEthanolWetMill=NULL)]

    ###
    # Instead of abatement curve, just show $/ton as straight bar plot
    ###
    setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel)
    avg.results <- the.macc[,list(VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel,GHG,EnergyDemand)]
    avg.results[,':='(rank.avg=apply(mac.results[,,1],1,median),rank.sd=apply(mac.results[,,1],1,sd),MIAC=apply(mac.results[,,4],1,median),MAC=apply(mac.results[,,2],1,median),MAC.sd=apply(mac.results[,,2],1,sd),AbatementPotential=apply(mac.results[,,3],1,median))]
    avg.results <- avg.results[!is.nan(MAC) & !MAC==Inf]
    avg.results[,AbatementPotential:=AbatementPotential/1e3]
    avg.results[,rank:=rank(MAC,ties.method='random')]
    setkey(avg.results,rank,County)
    avg.results[,MAC:=round(MAC,3)]

    avg.results[,AltKey:=pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel)]
    setkey(avg.results,AltKey)
    avg.by.fuel <- avg.results[AltFuel!='CornEthanolDryMill',list(MAC=weighted.mean(MAC,GHG),MIAC=weighted.mean(MIAC,EnergyDemand),AltFuelType=AltFuelType[1]),by='AltKey']
    avg.by.fuel[AltFuelType%in%c('BEV','PHEV'),AltFuelType:='PEV']
    setkey(avg.by.fuel,MAC)
    avg.by.fuel[,AltKey:=factor(AltKey,AltKey[order(MAC)])]
    avg.by.fuel[,AltFuelType:=factor(AltFuelType,u(avg.by.fuel$AltFuelType))]
    ggplot(avg.by.fuel,aes(x=AltKey,y=MAC,fill=AltFuelType))+geom_bar(stat='identity')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      scale_fill_manual(values=as.character(alt.type.colors$color.hex[match(u(avg.by.fuel$AltFuelType),alt.type.colors$AltKey)]))+
      labs(x='',y='Cost of Avoided GHG Emissions ($/ton CO2eq)')
    setkey(avg.by.fuel,MIAC)
    avg.by.fuel[,AltKey:=factor(AltKey,AltKey[order(MIAC)])]
    avg.by.fuel[,AltFuelType:=factor(AltFuelType,u(avg.by.fuel$AltFuelType))]
    ggplot(avg.by.fuel,aes(x=AltKey,y=MIAC,fill=AltFuelType))+geom_bar(stat='identity')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      scale_fill_manual(values=as.character(alt.type.colors$color.hex[match(u(avg.by.fuel$AltFuelType),alt.type.colors$AltKey)]))+
      labs(x='',y='Cost of Reduced Fuel Carbon Intensity($1000/(g/MJ)')

    ###
    # Plot Market Segments
    ###
    mark$County <- factor(mark$County,c('Del Norte','Humboldt','Mendocino','Trinity','Siskiyou','I5'))
    ggplot(mark,aes(x=County,y=EnergyDemand/1e6,fill=FuelType))+
      geom_bar(stat='identity')+
      theme_bw()+
      scale_fill_manual(values=as.character(alt.type.colors$color.hex[alt.type.colors$color%in%c('blue','green')]))+
      facet_grid(VehicleType~VehicleStatus)+
      theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'))+
      labs(x='Region',y='Annual Fuel Energy Demand (PJ)')

    ###
    # Quantiles of MACCs
    ###
    setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel)
    ghg.results.limited <- sapply(1:ncol(mac.results[,,3]),function(i){ 
                                  ord <- order(mac.results[,i,2])
                                  cutoff.i <- which(cumsum(mac.results[ord,i,3])>ghg.goal)[1]
                                  if(!is.na(cutoff.i)){ if(cutoff.i < length(ord)) mac.results[ord,i,3][(cutoff.i+1):length(ord)] <- 0 }
                                  mac.results[,i,3]
    })

    mac.weighted.avg <- apply(mac.results[,,2]*ghg.results.limited,2,sum,na.rm=T)/apply(ghg.results.limited,2,sum,na.rm=T)
    # make a colour histogram to illustrate the nonile concept
    #macs <- data.frame(WeightedAvgMAC=mac.weighted.avg,prob=ecdf(mac.weighted.avg)(mac.weighted.avg))
    probs <- seq(0,1,length.out=10)
    quant.names <- c('1st','2nd','3rd',pp(4:9,'th'))
    #macs$Nonile <- quant.names[match(probs[findInterval(macs$prob,probs,rightmost.closed=T)],probs)]
    #ggplot(macs,aes(x=WeightedAvgMAC,fill=Nonile))+geom_histogram() + theme_bw() + labs(x='Weighted Avg MAC ($/ton CO2e)') + scale_fill_manual(values=pp(my.colors,""))

    mac.quants <- data.table()
    #probs <- seq(0,1,length.out=25)
    quants <- quantile(mac.weighted.avg,probs)
    #for(quant.i in 1:length(quants)){
    for(quant.i in 1:(length(quants)-1)){
      #curve.i <- which.min(abs(quants[quant.i] - mac.weighted.avg))
      curve.i <- which(mac.weighted.avg > quants[quant.i] & mac.weighted.avg < quants[quant.i+1])
      temp <- copy(the.macc[,list(VehicleStatus,VehicleType,FuelType,County,AltFuelType,AltFuel)])
      #temp[,':='(Quantile=probs[quant.i],MIAC=round(mac.results[,curve.i,2],3),AbatementPotential=mac.results[,curve.i,3]/1e3)]
      temp[,':='(WtAvgMAC=mean(mac.weighted.avg[curve.i]),Nonile=quant.names[quant.i],MAC=round(apply(mac.results[,curve.i,2],1,mean),3),AbatementPotential=apply(mac.results[,curve.i,3],1,mean)/1e3)]
      temp <- temp[!is.nan(MAC) & !MAC==Inf]
      setkey(temp,MAC)
      temp[,rank:=rank(MAC,ties.method='random')]
      mac.quants <- rbindlist(list(mac.quants,temp),use.names=T,fill=T)
    }

    setkey(mac.quants,Nonile,rank,County)

    #mac.quants[,key:=factor(pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel,' - ',County,' - ',VehicleStatus,FuelType,VehicleType))]
    mac.quants[,key:=pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel)]
    mac.quants[,key:=factor(key)]
    #mac.quants[,key:=factor(mac.quants$key,mac.quants$key)] # reorders the levels to match their rank
    mac.quants[,CumulativeAbatement:=cumsum(AbatementPotential),by='Nonile']
    mac.quants[,x.pos:=0.5*(cumsum(AbatementPotential)+cumsum(c(0, head(AbatementPotential,-1)))),by='Nonile']

    setkey(mac.quants,key)
    the.labs <- mac.quants[,list(x.pos=weighted.mean(x.pos,AbatementPotential),MAC=weighted.mean(MAC,AbatementPotential)),by=c('Nonile','key')]
    the.labs[,':='(AbatementPotential=NA,County=NA)]
    setkey(mac.quants,Nonile,rank,County)

    weighted.avg.by.nonile <- mac.quants[,list(WtAvgMAC=WtAvgMAC[1]),by='Nonile']

    to.use <- mac.quants
    factor.name <- 'Nonile' 
    streval(pp('setkey(to.use,',factor.name,',MAC)'))
    to.use[,first.over.target:=CumulativeAbatement >= ghg.goal/1e3]
    streval(pp('to.use[,first.over.target:=(first.over.target==T & c(F,head(first.over.target,-1))==F),by="',factor.name,'"]'))
    to.use <- to.use[CumulativeAbatement <= ghg.goal/1e3 | first.over.target==T]
    alt.order <- to.use[,list(key=key[1]),by='key']
    to.use[,key:=factor(key,alt.order$key)]
    setkey(to.use,MAC)
    max.y <- max(to.use$MAC)
    ggplot(to.use,aes(x=x.pos,label=key,width=AbatementPotential,y=MAC,fill=key))+
      geom_bar(stat='identity',position='identity',colour='black',linetype='blank')+
      theme_bw()+
      #geom_text(width=1,y=-0.01,angle=60,hjust=1,size=4,data=the.labs)+
      #scale_y_continuous(limit=c(-750,max.y))+
      scale_x_continuous(limit=c(-10,max(to.use$x.pos)))+
      scale_fill_manual(values=as.character(alt.colors$color.hex[match(sort(u(to.use$key)),alt.colors$AltKey)]))+
      facet_wrap(~Nonile)+
      geom_vline(xintercept=ghg.goal/1e3,color=my.colors['red'],linetype='longdash')+
      geom_hline(aes(yintercept=WtAvgMAC),data=weighted.avg.by.nonile,colour=my.colors['grey'],linetype='longdash')+
      labs(x='GHG Abatement (kt CO2eq)',y='Marginal Abatement Cost ($/t CO2eq)',fill='AltFuel',title='MACC of Each Nonile')

  ##########################
  # Plot average pricing
  ##########################
  fuel.price.rv <- copy(fuel.price)
  fuel.price.rv[,PriceRV:=PriceAvg]
  elec.price <- fuel.price.rv[AltFuel=='BEV']$PriceRV
  gas.price <- fuel.price.rv[AltFuel=='Gas']$PriceRV
  diesel.price <- fuel.price.rv[AltFuel=='Diesel']$PriceRV
  # Blend Flex and Ethanol
  setkey(fuel.price.rv,FuelType,AltFuel)
  flex.prices <- copy(fuel.price.rv[FuelType=='Ethanol'])
  flex.prices[,':='(FuelType='Flex',PriceRV=PriceRV*get.param('FlexBlend')+gas.price*(1-get.param('FlexBlend')))]
  fuel.price.rv[FuelType=='Ethanol',':='(PriceRV=PriceRV*get.param('EthanolBlend')+gas.price*(1-get.param('EthanolBlend')))]
  # Blend PHEV
  phev.price <- data.table(AltFuel='PHEV',FuelType='PHEV',Replaces='None',PriceRV=elec.price * get.param('PHEVBlend') + gas.price * (1 - get.param('PHEVBlend')))
  # Blend Biodiesel
  fuel.price.rv[FuelType=='Biodiesel',':='(PriceRV=PriceRV*get.param('BiodieselBlend')+diesel.price*(1-get.param('BiodieselBlend')))]
  # Rename Elec to BEV
  fuel.price.rv[AltFuel=='Electricity',':='(FuelType='BEV',AltFuel='BEV')]
  fuel.price.rv <- rbindlist(list(fuel.price.rv,flex.prices,phev.price),use.names=T,fill=T)

  setkey(fuel.price.rv,Replaces)
  fuel.price.by.county <- fuel.markup[fuel.price.rv,allow.cart=T]
  fuel.price.by.county[is.na(Markup),Markup:=0]
  fuel.price.by.county[,FuelCost:=PriceRV+Markup]

  # Now EER weight the cost using the LDV value for H2
  setkey(carb,AltFuel,IntensityEER) # need this to get H2 in correct order
  setkey(carb,AltFuel)
  setkey(fuel.price.by.county,AltFuel)
  fuel.price.by.county <- unique(carb)[,list(AltFuel,EER)][fuel.price.by.county]
  fuel.price.by.county[,FuelCost:=FuelCost/EER]

  # Join the conventional prices to market segments for later use in making the fuel cost incremental
  mark.rv <- copy(mark)
  setkey(mark.rv,County,FuelType)
  setkey(fuel.price.by.county,County,FuelType)
  mark.rv <- fuel.price.by.county[mark.rv,list(County,FuelType,VehicleType,VehicleStatus,NumVehicles,SegmentVMT,EnergyDemand,GHG,travel.demand,FuelCost)]
  mark.rv[,':='(ConventionalFuelCost=FuelCost,FuelCost=NULL)]

  # Now drop the conventionals
  fuel.price.by.county <- fuel.price.by.county[FuelType!="Gas" & FuelType!="Diesel"]
  fuel.price.by.county[,':='(AltFuelType=FuelType,FuelType=NULL)]

  ###
  # Sample and amortized the station cost into 2014$ / MJ
  ###
  station.costs[,DistCostRV:=DistCostAvg]
  station.costs[,StationCost:=DistCostRV * amort.dist.ratio]
  setkey(carb,AltFuelType,IntensityEER) # need this to get H2 in correct order
  setkey(carb,AltFuelType)
  setkey(station.costs,AltFuelType)
  station.costs <- unique(carb)[,list(AltFuelType,EER)][station.costs]
  station.costs[,StationCost:=StationCost/EER]

  ###
  # Sample and amortize the vehicle costs into 2014$ / MJ
  ###
  veh.costs[,':='(VehicleCost=VehicleCostAvg * cost.per.MJ.EER.ratio)]
  
  ###
  # Sum the costs together within the context of each market segment
  # note that conventional fuel cost will be reletive to the segment and subtracted out after the join
  ###
  setkey(station.costs,AltFuelType) 
  setkey(veh.costs,AltFuelType) 
  all.costs <- station.costs[,list(AltFuelType,StationCost)][veh.costs[,list(AltFuelType,County,FuelType,VehicleType,VehicleCost)]]
  setkey(all.costs,AltFuelType,County)
  setkey(fuel.price.by.county,AltFuelType,County)
  all.costs <- all.costs[fuel.price.by.county[,list(AltFuelType,County,AltFuel,FuelCost)],allow.cart=T]
  all.costs[is.na(StationCost),StationCost:=0]
  all.costs[is.na(VehicleCost),VehicleCost:=0]
  all.costs[,SubtotalCost:=StationCost+VehicleCost+FuelCost]

  all.costs.with.FuelType <- all.costs[!is.na(FuelType)]
  all.costs.without.FuelType <- all.costs[is.na(FuelType)]
  setkey(all.costs.with.FuelType,FuelType,VehicleType,County)
  setkey(mark.rv,FuelType,VehicleType,County)

  all.costs.with.FuelType <- mark.rv[,list(FuelType,VehicleType,County,VehicleStatus,ConventionalFuelCost,EnergyDemand,GHG)][all.costs.with.FuelType[,list(AltFuelType,FuelType,VehicleType,County,AltFuel,StationCost,VehicleCost,FuelCost,SubtotalCost)],allow.cart=T]
  setkey(all.costs.without.FuelType,County)
  setkey(mark.rv,County)
  all.costs.without.FuelType <- mark.rv[,list(FuelType,VehicleType,County,VehicleStatus,ConventionalFuelCost,EnergyDemand,GHG)][all.costs.without.FuelType[,list(AltFuelType,FuelType,VehicleType,County,AltFuel,StationCost,VehicleCost,FuelCost,SubtotalCost)],allow.cart=T]

  all.costs <- rbindlist(list(all.costs.with.FuelType,all.costs.without.FuelType),use.names=T,fill=T)[,list(AltFuelType,AltFuel,FuelType,VehicleType,County,VehicleStatus,ConventionalFuelCost,StationCost,VehicleCost,FuelCost,SubtotalCost,EnergyDemand,GHG)]
  all.costs[,':='(TotalCost=SubtotalCost - ConventionalFuelCost,FuelCost=FuelCost - ConventionalFuelCost)]

  # At this point, NA's mean we have incomplete data in the inputs, drop now
  all.costs <- na.omit(all.costs)
  all.costs[,AltKey:=pp(ifelse(AltFuelType=='Flex','Flex',''),AltFuel)]
  all.costs.sub <- all.costs[((FuelType=='Gas' & AltFuelType!='Biodiesel') | (FuelType=='Diesel' & AltFuelType=='Biodiesel')) & (! (AltFuel=='H2' & VehicleType=='HDV')) & (County=='Humboldt' | is.na(County))]
  setkey(all.costs.sub,AltKey)
  key.for.ordering <- data.table(all.costs.sub[,list(tot=mean(TotalCost)),by='AltKey'],key='tot')
  all.costs.sub[,AltKey:=factor(AltKey, key.for.ordering$AltKey)]

  # Plot up the amortized costs 
  setkey(all.costs.sub,AltKey,VehicleType)
  all.costs.m <- melt(all.costs.sub,id.vars=c('AltKey','VehicleType','FuelType'),measure.vars=c('VehicleCost','StationCost','FuelCost'))
  all.costs.m$value <- all.costs.m$value * c('Gas'=115.63,'Diesel'=134)[all.costs.m$FuelType]
  setkey(all.costs.m,AltKey,VehicleType,variable)
  all.costs.m <- all.costs.m[,list(cost=mean(value,na.rm=T)),by=c('AltKey','variable')]
  #ggplot(subset(all.costs.m,VehicleType=='LDV'),aes(x=County,y=cost,fill=variable))+geom_bar(stat='identity',position='stack')+facet_wrap(FuelType~VehicleType)
  #ggplot(subset(all.costs.m,VehicleType=='LDV' | is.na(VehicleType)),aes(x=AltKey,y=cost,fill=variable))+geom_bar(stat='identity',position='stack')+geom_segment(aes(x=AltKey,xend=AltKey,y=0,yend=cost),arrow=arrow(length = unit(0.5, "cm")))+labs(y="Amortized Incremental Cost ($/MJ-EER)",fill='Cost Category')+theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'))

  #1/115.63*25 == 0.2162069 miles/MJ Gas
  #1/134*29 == 0.2164179 miles/MJ Diesel
  ggplot(subset(all.costs.m,!AltKey%in%c('CornEthanolWetMill','FlexCornEthanolWetMill')),aes(x=AltKey,y=cost,fill=variable))+
    geom_bar(stat='identity',position='stack')+
    theme_bw()+
    scale_fill_manual(values=pp(my.colors,""))+
    labs(y="Amortized Incremental Cost ($/GGE)",fill='Cost Category')+
    theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'))
  }

}


