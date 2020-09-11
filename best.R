best <- function(state, outcome) {
        data <- read.csv("rprog3/outcome-of-care-measures.csv")[, c(7, 2,11, 17, 23)]
        posout <- c("heart attack", "heart failure", "pneumonia")
        posstates <- as.character(levels(as.factor(data$State)))
        besthospital <- character()
        if (!match(state, posstates, nomatch = FALSE)) {
                return(warning("Invalid State"))
        }
        else if (!match(outcome, posout, nomatch = FALSE)) {
                return(warning("Invalid Outcome"))
        }
        else {
                if (outcome == "heart attack") {
                        data1 <- data[, 1:3]
                        data1[[3]] <- as.numeric(data1[[3]])
                        good <- complete.cases(data1)
                        good.data <- data1[good,]
                        statesplit <- split(good.data, good.data$State)
                        alphasplit <- as.data.frame(statesplit[state])
                        betasplit <- alphasplit[order(alphasplit[[2]]),]
                        charliesplit <- betasplit[[3]]
                        minrate <- min(charliesplit)
                        ind <- match(minrate, charliesplit)
                        besthospital <- betasplit[[2]][ind]
                }
                else if (outcome == "heart failure") {
                        data1 <- data[, c(1,2,4)]
                        data1[[3]] <- as.numeric(data1[[3]])
                        good <- complete.cases(data1)
                        good.data <- data1[good,]
                        statesplit <- split(good.data, good.data$State)
                        alphasplit <- as.data.frame(statesplit[state])
                        betasplit <- alphasplit[order(alphasplit[[2]]),]
                        charliesplit <- betasplit[[3]]
                        minrate <- min(charliesplit)
                        ind <- match(minrate, charliesplit)
                        besthospital <- betasplit[[2]][ind]
                }
                else if (outcome == "pneumonia") {
                        data1 <- data[, c(1,2,5)]
                        data1[[3]] <- as.numeric(data1[[3]])
                        good <- complete.cases(data1)
                        good.data <- data1[good,]
                        statesplit <- split(good.data, good.data$State)
                        alphasplit <- as.data.frame(statesplit[state])
                        betasplit <- alphasplit[order(alphasplit[[2]]),]
                        charliesplit <- betasplit[[3]]
                        minrate <- min(charliesplit)
                        ind <- match(minrate, charliesplit)
                        besthospital <- betasplit[[2]][ind]
                }
                
        }
        return(besthospital)
}
