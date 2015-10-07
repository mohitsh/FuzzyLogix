require(gdata)
age <- read.xls("Consolidated Case Study Data.xlsx", header = FALSE, sheet =1)
age_sc <- read.xls("Consolidated Case Study Data.xlsx", header = FALSE, sheet =2)
age_st <- read.xls("Consolidated Case Study Data.xlsx", header = FALSE, sheet =3)
literacy <- read.xls("Consolidated Case Study Data.xlsx",header = FALSE, sheet = 4)
literacy_sc <- read.xls("Consolidated Case Study Data.xlsx",header = FALSE, sheet = 5)
literacy_st <- read.xls("Consolidated Case Study Data.xlsx",header = FALSE, sheet = 6)
poverty <- read.xls("Consolidated Case Study Data.xlsx", header = FALSE, sheet = 7)

#do it
mod_poverty <- read.xls("poverty.xlsx", header = FALSE, sheet = 2)

## myfunction <- function(state_name1, state_name2,cast,gender){
##        statements
##        return(object)
## }

dudefunction <- function(state_name1,state_name2,cast,gender,state_type){
# JK literate person vs their poverty
if (cast == "total"){JK_lit <- subset(literacy, V4 == state_name1 & V5 ==  "Illiterate")}
else if(cast == "sc"){JK_lit <- subset(literacy_sc, V4 == state_name1 & V5 ==  "Illiterate")}
else if(cast == "st"){JK_lit <- subset(literacy_st, V4 == state_name1 & V5 ==  "Illiterate")}

JK_Pov <- subset(poverty, V1 == state_name2)

if (gender == "total") {JK_lit_Person <- JK_lit[1,c("V7","V10","V13")]}
else if (gender == "male") {JK_lit_Person <- JK_lit[1,c("V8","V11","V14")]}
else if (gender == "female") {JK_lit_Person <- JK_lit[1,c("V9","V12","V15")]}

# getting total population of a state
if(cast == "total"){ JK_population <- subset(age, V4 == state_name1) }
else if(cast == "sc"){ JK_population <- subset(age_sc, V4 == state_name1) }
else if(cast == "st"){ JK_population <- subset(age_st, V4 == state_name1) }

if (gender == "total") {JK_polulation1 <- JK_population[1,c("V7","V10","V13")]}
else if(gender == "male"){JK_polulation1 <- JK_population[1,c("V8","V11","V14")]}
else if(gender == "female"){JK_polulation1 <- JK_population[1,c("V9","V12","V15")]}

# changing data frame from factor to numric form
indx <- sapply(JK_lit_Person, is.factor)
JK_lit_Person[indx] <- lapply(JK_lit_Person[indx], function(x) as.numeric(as.character(x)))

indx <- sapply(JK_polulation1, is.factor)
JK_polulation1[indx] <- lapply(JK_polulation1[indx], function(x) as.numeric(as.character(x)))

# calculating percentage
JK_total_literate_percentage <- JK_lit_Person/JK_polulation1

if (state_type == "total"){return (JK_total_literate_percentage[,1])}
else if (state_type == "rural"){return (JK_total_literate_percentage[,2])}
else if (state_type == "urban"){return (JK_total_literate_percentage[,3])}

}
## starts
name1 <- c("State - JAMMU & KASHMIR (01)",
           "State - HIMACHAL PRADESH (02)",
           "State - PUNJAB (03)",
           "State - CHANDIGARH (04)",
           "State - UTTARAKHAND (05)",
           "State - HARYANA (06)",
           "State - NCT OF DELHI (07)",
           "State - RAJASTHAN (08)",
           "State - UTTAR PRADESH (09)",
           "State - BIHAR (10)",
           "State - SIKKIM (11)",
           "State - ARUNACHAL PRADESH (12)",
           "State - NAGALAND (13)",
           "State - MANIPUR (14)",
           "State - MIZORAM (15)",
           "State - TRIPURA (16)",
           "State - MEGHALAYA (17)",
           "State - ASSAM (18)",
           "State - WEST BENGAL (19)",
           "State - JHARKHAND (20)",
           "State - ODISHA (21)",
           "State - CHHATTISGARH (22)",
           "State - MADHYA PRADESH (23)",
           "State - GUJARAT (24)",
           "State - DAMAN & DIU (25)",
           "State - DADRA & NAGAR HAVELI (26)",
           "State - MAHARASHTRA (27)",
           "State - ANDHRA PRADESH (28)",
           "State - KARNATAKA (29)",
           "State - GOA (30)",
           "State - LAKSHADWEEP (31)",
           "State - KERALA (32)",
           "State - TAMIL NADU (33)",
           
           "State - PUDUCHERRY (34)",
           "State - ANDAMAN & NICOBAR ISLANDS (35)"
)

name2 <- c(
        "Jammu & Kashmir",
        "Himachal Pradesh",
        "Punjab",
        "Chandigarh",
        "Uttarakhand",
        "Haryana",
        "Delhi",
        "Rajasthan",
        "Uttar Pradesh",
        "Bihar",
        "Sikkim",
        "Arunachal Pradesh",
        "Nagaland",
        "Manipur",
        "Mizoram",
        "Tripura",
        "Meghalaya",
        "Assam",
        "West Bengal",
        "Jharkhand",
        "Odisha",
        "Chhatisgarh",
        "Madhya Pradesh",
        "Gujarat",
        "Daman & Diu",
        "Dadra & Nagar Haveli",
        "Maharashtra",
        "Andhra Pradesh",
        "Karnataka",
        "Goa",
        "Lakshadweep",
        "Kerala",
        "Tamil Nadu",
        "Puducherry",
        "Andaman & Nicobar Islands"
)




## ends
coolfunction <- function(cast,gender,state_type,state_type_poverty){
        illiteracy <- numeric()
        for (i in 1:33 ) {
                
                n1 <- name1[i]
                n2 <- name2[i]
                a <- dudefunction(n1,n2,cast,gender,state_type)
                illiteracy <- c(illiteracy,a)
        }
        #return (illiteracy)
        if (state_type_poverty == "total"){state_poverty <- mod_poverty[1:33,"V2"]}  
        else if (state_type_poverty == "rural"){state_poverty <- mod_poverty[1:33,"V4"]}
        else if (state_type_poverty == "urban"){state_poverty <- mod_poverty[1:33,"V6"]}
        
        #indx <- sapply(dude_pov, is.factor)
        #dude_pov[indx] <- lapply(dude_pov[indx], function(x) as.numeric(as.character(x)))
        
        #dude_pov_num <- as.numeric(dude_pov[1,])
        #return (dude_pov)
        #dabba <- class(dude_pov[1])
        #return (dabba)
        #illiteracy <- illiteracy[illiteracy>x]
        #state_poverty <- state_poverty[state_poverty>y]
        fit <- lm(illiteracy~state_poverty)
        summ1 <- summary(fit)
        res <- signif(residuals(fit))
        pre <- predict(fit)
        
        plot(state_poverty,illiteracy)
        segments(state_poverty, illiteracy, state_poverty, pre, col="red")
        abline(fit)
        title("poverty vs illiteracy")
        #library(calibrate)
        #textxy(state_poverty, illiteracy, res, cx=0.7)
        return (summ1)
        
}

## format for input
## coolfunction("sc","male","rural","rural")
## console will print all the summary and you can see the plot












