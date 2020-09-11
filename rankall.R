rankall <- function(outcome, num="best") {
        pos.out <- c("heart attack", "heart failure", "pneumonia")
        if (!match(outcome, pos.out, nomatch=FALSE)) {
                stop("invalid outcome")
        }
        else {
                data0 <- read.csv("rprog3/outcome-of-care-measures.csv")[,c(7, 2, 11, 17, 23)]
                rank.data.m <-matrix(nrow=0, ncol=2) 
                
                if (outcome == "heart attack") {
                        data1 <- data0[, c(1,2,3)]
                }
                else if (outcome == "heart failure") {
                        data1 <- data0[, c(1,2,4)]
                }
                else if (outcome == "pneumonia") {
                        data1 <- data0[, c(1,2,5)]
                }
                
                data1[,3] <- as.numeric(data1[[3]])
                good <- complete.cases(data1)
                cdata <- data1[good,]
                state.split <- split(cdata, cdata$State)
                state.names <- names(state.split)
                for (i in seq_along(state.names)) {
                        state.data <- as.data.frame(state.split[state.names[[i]]])
                        state.data <- state.data[order(state.data[[2]]),]
                        state.data <- state.data[order(state.data[[3]]),]
                        rank <- (if (num == "best") {1}
                                 else if (is.numeric(num)) {num}
                                 else if (num == "worst") {nrow(state.data)}
                        )
                        rank.data.m <- rbind(rank.data.m, c(state.data[[2]][rank],state.names[i]))
                }
        }
        rank.data.d <- as.data.frame(rank.data.m)
        names(rank.data.d) <- c("hospital", "state")
        return(rank.data.d)
}