the.macc[,OriginalOrder:=1:length(MAC),by=c('VehicleStatus','VehicleType','FuelType','County')] 

tradeoffs.permute <- function(AltFuel,MAC,PenetrationLimit,RawGHGA,Penetration,EnergyDemand,cost.ghg.weight,return.order=F){
  final.ordering <- 1:length(MAC)
  for(contender.i in 1:length(MAC)){
    if(contender.i <= 1){
      excluded.inds <- c()
    }else{
      excluded.inds <- 1:(contender.i-1)
    }
    possible.orderings <- sapply(contender.i:length(MAC),function(x){ 
      remaining.inds <- contender.i:length(MAC)
      c(final.ordering[excluded.inds],final.ordering[x],final.ordering[remaining.inds[-which(x==remaining.inds)]])
    })
    #delta.ghg.per.mj <- -(sum(RawGHGA*Penetration,na.rm=T)-apply(possible.orderings,2,function(ord){ sum(resolve.pens(PenetrationLimit[ord])*RawGHGA[ord],na.rm=T) }))/apply(possible.orderings,2,function(ord){ sum(resolve.pens(PenetrationLimit[ord])*EnergyDemand) })
    delta.ghg <- sum(RawGHGA*Penetration,na.rm=T)-apply(possible.orderings,2,function(ord){ sum(resolve.pens(PenetrationLimit[ord])*RawGHGA[ord],na.rm=T) })
    delta.ghg[delta.ghg>=0] <- Inf
    delta.cost <- (apply(possible.orderings,2,function(ord){ sum(MAC[ord]*resolve.pens(PenetrationLimit[ord])*RawGHGA[ord],na.rm=T) }) - sum(MAC*RawGHGA*Penetration,na.rm=T))/(-delta.ghg)^2
    delta.cost <- (apply(possible.orderings,2,function(ord){ sum(MAC[ord]*resolve.pens(PenetrationLimit[ord])*RawGHGA[ord],na.rm=T) }) - sum(MAC*RawGHGA*Penetration,na.rm=T))/(-delta.ghg)^2
    #delta.cost[delta.cost<0] <- 1/delta.cost[delta.cost<0]
    delta.cost[is.nan(delta.cost) | delta.ghg==Inf] <- Inf
    final.ordering <- possible.orderings[,which.min(delta.cost)]
    #if(!all(is.na(delta.ghg)) & !all(is.na(delta.cost)) & length(delta.ghg)>0){
      #delta.ghg.norm <- delta.ghg-min(delta.ghg)
      #if(any(delta.ghg.norm<Inf,na.rm=T)){
       #if(any(delta.ghg.norm[delta.ghg.norm<Inf]!=0,na.rm=T))delta.ghg.norm <- delta.ghg.norm/(max(delta.ghg[delta.ghg<Inf])-min(delta.ghg))
      #}
      #delta.cost.norm <- delta.cost-min(delta.cost)
      #if(any(delta.cost.norm<Inf,na.rm=T)){
        #if(any(delta.cost.norm[delta.cost.norm<Inf]!=0,na.rm=T))delta.cost.norm <- delta.cost.norm/(max(delta.cost[delta.cost<Inf])-min(delta.cost))
      #}
      #delta.weighted <- delta.ghg.norm*cost.ghg.weight + (1-cost.ghg.weight)*delta.cost.norm
      #final.ordering <- possible.orderings[,which.min(delta.weighted)]
    #}
  }
  if(return.order){
    order(final.ordering)
  }else{
    list(DeltaCost=delta.cost,DeltaGHG=delta.ghg)
  }
}

      pause <- sum(the.macc$AbatementPotential) < ghg.goal
      cost.ghg.weight <- 0
      the.macc.orig <- copy(the.macc)
      for(cost.ghg.weight in c(0,0.25,0.5,0.75,0.8,0.85,0.9,0.95)){
        #my.cat(pp('Cost.v.GHG.Weight: ',cost.ghg.weight))
        the.macc <- copy(the.macc.orig)
        use.segment <- the.macc[,list(Use=T),by=c('VehicleStatus','VehicleType','FuelType','County')]
        setkey(use.segment,VehicleStatus,VehicleType,FuelType,County)
        while(sum(the.macc$AbatementPotential) < ghg.goal & any(use.segment$Use)){
          #my.cat(round(sum(the.macc$AbatementPotential)/1e3))
          setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,OriginalOrder)

          the.macc <- use.segment[the.macc]
          if('i.Use' %in% names(the.macc))the.macc[,i.Use:=NULL]
          the.metrics[,order:=1:length(MAC),by=key]
          attempt <- the.metrics[,list(perm.i=perm.i[1],GHGA=GHGA[1],MAC=MAC[1],used=order[1]),by=key]
          setkey(attempt,key)
          setkey(the.metrics,key)
          the.metrics <- attempt[,list(key,used)][the.metrics]
          attempt <- the.metrics[,list(perm.i=perm.i[1],GHGA=GHGA[1],MAC=MAC[1],used=order[1]),by=key]
          setkey(the.metrics,MAC,key)

          winner.i <- which(the.metrics$used+1 == the.metrics$order)[1]

          bad.metrics <- the.metrics[DeltaGHG==Inf | DeltaCost==Inf]
          use.segment[VehicleStatus%in%bad.metrics$VehicleStatus & VehicleType%in%bad.metrics$VehicleType & FuelType%in%bad.metrics$FuelType & County%in%bad.metrics$County,Use:=F]
          the.metrics <- the.metrics[DeltaGHG<Inf & DeltaCost<Inf]
          if(nrow(the.metrics)>1){
            winner <- the.metrics[which.min(DeltaCost)]

            #the.metrics[,delta.cost.norm:=DeltaCost-min(DeltaCost)]
            #if(any(the.metrics$delta.cost.norm!=0))the.metrics[,delta.cost.norm:=delta.cost.norm/(max(DeltaCost)-min(DeltaCost))]
            #the.metrics[,delta.ghg.norm:=DeltaGHG-min(DeltaGHG)]
            #if(any(the.metrics$delta.ghg.norm!=0))the.metrics[,delta.ghg.norm:=delta.ghg.norm/(max(DeltaGHG)-min(DeltaGHG))]
            #the.metrics[,delta.weighted:=delta.ghg.norm*cost.ghg.weight + (1-cost.ghg.weight)*delta.cost.norm]
            #winner <- the.metrics[which.min(delta.weighted)]
          }else{
            winner <- the.metrics
          }

          winner.i <- which(the.macc$VehicleStatus==winner$VehicleStatus & the.macc$VehicleType==winner$VehicleType & the.macc$FuelType==winner$FuelType & the.macc$County==winner$County)
          the.macc[winner.i,OriginalOrder:=tradeoffs.permute(AltFuel,MAC,PenetrationLimit,RawGHGA,Penetration,EnergyDemand,cost.ghg.weight,return.order=T)]
          use.segment[VehicleStatus==winner$VehicleStatus & VehicleType==winner$VehicleType & FuelType==winner$FuelType & County==winner$County,Use:=F]
          setkey(the.macc,VehicleStatus,VehicleType,FuelType,County,OriginalOrder)
          the.macc[,Penetration:=resolve.pens(PenetrationLimit),by=c('VehicleStatus','VehicleType','FuelType','County')]
          the.macc[,AbatementPotential:=Penetration*RawGHGA]
        }
        if(sum(the.macc$AbatementPotential) > ghg.goal)break
        #cost.ghg.weight <- cost.ghg.weight + 0.2

      #if(pause)my.cat(pp('Final weight: ',cost.ghg.weight))
      #my.cat(pp('FINAL: ',round(sum(the.macc$AbatementPotential)/1e3)))
      #my.cat('')
      #as.data.frame(the.macc[,list(AltFuel,VehicleStatus,VehicleType,FuelType,County,MAC,Penetration,AbatementPotential,OriginalOrder)])
