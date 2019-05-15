library(shiny)

shinyServer(function(input, output){
        URL <- "http://estadisticas.mecd.gob.es/EducaJaxiPx/files/_px/es/csv_sc/Universitaria/Alumnado/Avance/2017-2018/1GradoCiclo/CapituloIII/Publicas/l0/AV16_mat_III6_pub.px?nocab=1"
        filecsv <- read.csv2(URL, encoding = "unicode", skip = 5,
                             stringsAsFactors = FALSE,
                             na.strings = "..", header = TRUE)
        
        ## Cleaning data
        
        ## Getting studies
        
        columnames <- read.csv2(URL, encoding = "unicode", skip = 5,
                                stringsAsFactors = FALSE, header = FALSE,
                                nrows = 1)
        studies <- columnames[, !is.na(columnames)]
        studies <- studies[, 2:length(studies)]
        studies <- gsub("[0-9]", "", studies)
        studies <- gsub("-", "", studies)
        studies <- gsub("^[[:space:]]+[[:space:]]", "", studies)
        
        ## Getting studies-sex column names
        
        studiessex <- cbind(paste(studies, " - Women"),
                            paste(studies, " - Men"))
        
        
        colconstruct <- function (df) {
                k <- 1
                cnames <- c()
                for (i in 1:nrow(df)) {
                        for (j in 1:ncol(df)) {
                                cnames[k] <- df[i, j]
                                k <- k + 1
                        }
                }
                cnames
        }
        
        studycols <- colconstruct(studiessex)
        
        ## Getting column names:
        
        cols <- c("University", studycols)
        
        ## Getting data
        
        data <- read.csv2(URL, encoding = "unicode", skip = 10,
                          stringsAsFactors = FALSE,
                          na.strings = "..", header = FALSE,
                          nrows = 57-10)
        datacnames <- read.csv2(URL, encoding = "unicode", skip = 6,
                                stringsAsFactors = FALSE, header = FALSE,
                                nrows = 1)
        
        colnames(data) <- datacnames
        
        ## Removing useless data:
        
        df0 <- data[, -(2:4)]
        rmcols <- c(grep("Ambos", colnames(df0)), ncol(df0))
        df1 <- df0[, -rmcols]
        
        ## Renaming columns:
        
        colnames(df1) <- cols
        
        ## Informática is repeated, so it's necessary to remove repeated data:
        
        df2 <- df1[, -grep("Informática", colnames(df1))[3:4]]
        
        ## "Tidyversing": Multiple variables in one column: studies and sex
        
        df3 <- tidyr::gather(df2[!is.na(df2[, 2]), ], study_sex, count, -University)
        df4 <- tidyr::separate(data = df3,
                               col = study_sex,
                               into = c("Degree", "Sex"),
                               sep = " - ")
        my_df <- data.frame(University = as.factor(df4$University),
                            Degree =as.factor(df4$Degree),
                            Sex = as.factor(df4$Sex),
                            count = as.numeric(df4$count))
        
        ## Men vs women per degree
        
        mwdegree <- dplyr::summarise(dplyr::group_by(my_df, Degree, Sex),
                                     Total = sum(count, na.rm = TRUE))
        
        ## Adding colors
        
        mwdegree$col <- colors(distinct = TRUE)[1:nrow(mwdegree)]
        
        ## Server commands
        
        output$plot1 <- renderPlot({
                showwomen <- input$checkwomen
                showmen <- input$checkmen
                mfrow <- c(1, showwomen + showmen)
                min <- input$students[1]
                max <- input$students[2]
                data1 <- subset(mwdegree, mwdegree$Sex == "Men" &
                                        mwdegree$Total >= min(min, 84001) &
                                        mwdegree$Total <= max)[, -2]
                data2 <- subset(mwdegree, mwdegree$Sex == "Women" &
                                        mwdegree$Total >= min(min, 103001) &
                                        mwdegree$Total <= max)[, -2]
                if (mfrow[2] == 0) {
                        mfrow[2] <- 1
                }
                par(mfrow = mfrow)
                if (showmen){
                        with(data1[order(-data1$Total), ],
                             pie(Total, labels = Degree,
                                 radius = 1.0, main = "Men", col = col))
                }
                if (showwomen) {
                        with(data2[order(-data2$Total), ],
                             pie(Total, labels = Degree,
                                 radius = 1.0, main = "Women", col = col))
                }
        })
})