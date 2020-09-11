rankhospital <- function(state, outcome, rank) {
        data0 <- read.csv("rprog3/outcome-of-care-measures.csv")[,c(7, 2, 11, 17, 23)]
        pos.out <- c("heart attack", "heart failure", "pneumonia")
        pos.states <- as.character(levels(as.factor(data0$State)))
        req.hospital <- character()
        if (!match(state, pos.states, nomatch=FALSE)) {
                stop("invalid state")
        }
        else if (!match(outcome, pos.out, nomatch=FALSE)) {
                stop("invalid outcome")
        }
        else {
                if (outcome == "heart attack") {
                        data1 <- data0[, 1:3]
                        data1[,3] <- as.numeric(data1[[3]])
                        good <- complete.cases(data1)
                        cdata <- data1[good,]
                        state.split <- split(cdata, cdata$State)
                        state.data <- as.data.frame(state.split[state])
                        alphabetical.order.data <- state.data[order(state.data[[2]]),]
                        ranked.data <- alphabetical.order.data[order(alphabetical.order.data[[3]]),]
                }
                else if (outcome == "heart failure") {
                        data1 <- data0[, c(1,2,4)]
                        data1[,3] <- as.numeric(data1[[3]])
                        good <- complete.cases(data1)
                        cdata <- data1[good,]
                        state.split <- split(cdata, cdata$State)
                        state.data <- as.data.frame(state.split[state])
                        alphabetical.order.data <- state.data[order(state.data[[2]]),]
                        ranked.data <- alphabetical.order.data[order(alphabetical.order.data[[3]]),]      
                }
                else if (outcome == "pneumonia") {
                        data1 <- data0[, c(1,2,5)]
                        data1[,3] <- as.numeric(data1[[3]])
                        good <- complete.cases(data1)
                        cdata <- data1[good,]
                        state.split <- split(cdata, cdata$State)
                        state.data <- as.data.frame(state.split[state])
                        alphabetical.order.data <- state.data[order(state.data[[2]]),]
                        ranked.data <- alphabetical.order.data[order(alphabetical.order.data[[3]]),]
                }
        }
        if (rank == "best") {
                req.hospital <- head(ranked.data[[2]], n=1)
        }
        else if (rank == "worst") {
                req.hospital <- tail(ranked.data[[2]], n=1)
        }
        else {
                if (rank > nrow(ranked.data)) {
                        return(NA)
                }
                else {
                        req.hospital <- ranked.data[[2]][rank]
                }
        }
        return(req.hospital)
}
