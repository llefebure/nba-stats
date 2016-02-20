shotChart <- function(){
  params <- list(SeasonType="Regular+Season", TeamID=0, PlayerID=201939, GameID="", Outcome="", Location="", Month=0, SeasonSegment="", DateFrom="", DateTo="", OpponentTeamID=0, VsConference="", VsDivision="", Position="", RookieYear="", GameSegment="", Period=0, LastNGames=10, ContextMeasure="FGA", Season="2015-16")
  d <- getGenericData("shotchartdetail", params)
  plot(as.numeric(d[[1]]$LOC_X), as.numeric(d[[1]]$LOC_Y))  
}
