
# Function to recode parties
recode_parties <- function(code) {
    case_when(
        code == "1" ~ "Social Democratic Party",
        code == "2" ~ "Social Liberal Party",
        code == "3" ~ "Conservative Party",
        code == "4" ~ "Centre Democrats",
        code == "5" ~ "Justice Party",
        code == "6" ~ "Socialist People’s Party",
        code == "7" ~ "Communist Party",
        code == "8" ~ "Danish People’s Party",
        code == "9" ~ "Common Course",
        code == "10" ~ "Christian Democrats",
        code == "11" ~ "Liberal Party",
        code == "12" ~ "Red-Green Alliance",
        code == "13" ~ "Left Socialists",
        code == "14" ~ "Progress Party / Liberty 2000",
        code == "15" ~ "Greenland/Faroe Islands (Unaff.)",
        code == "16" ~ "Independents / Minor Parties",
        code == "17" ~ "Liberal Alliance",
        code == "21" ~ "The Alternative",
        TRUE ~ NA_character_
    )
}

# Function to recode major topics
recode_major_topics <- function(code) {
    case_when(
        code == 1 ~ "Domestic Macroeconomic Issues",
        code == 2 ~ "Civil Rights, Minority Issues, and Civil Liberties",
        code == 3 ~ "Health",
        code == 4 ~ "Agriculture",
        code == 5 ~ "Labor and Employment",
        code == 6 ~ "Education",
        code == 7 ~ "Environment",
        code == 8 ~ "Energy",
        code == 9 ~ "Immigration and Refugee Issues",
        code == 10 ~ "Transportation",
        code == 12 ~ "Law, Crime, and Family Issues",
        code == 13 ~ "Social Welfare",
        code == 14 ~ "Community Development and Housing Issues",
        code == 15 ~ "Banking, Finance, and Domestic Commerce",
        code == 16 ~ "Defense",
        code == 17 ~ "Space, Science, Technology, and Communications",
        code == 18 ~ "Foreign Trade",
        code == 19 ~ "International Affairs and Foreign Aid",
        code == 20 ~ "Government Operations",
        code == 21 ~ "Public Lands, Water Management, and Territorial Issues",
        code == 23 ~ "Cultural Policy Issues",
        TRUE ~ NA_character_
    )
}

# Function to recode ministries
recode_ministries <- function(ministry_code) {
    case_when(
        ministry_code == "1"  ~ "Prime Minister",
        ministry_code == "2"  ~ "Foreign Affairs",
        ministry_code == "3"  ~ "Finance",
        ministry_code == "4"  ~ "Employment",
        ministry_code == "5"  ~ "Social Affairs",
        ministry_code == "6"  ~ "Justice",
        ministry_code == "7"  ~ "Interior",
        ministry_code == "8"  ~ "Defence",
        ministry_code == "9"  ~ "Environment",
        ministry_code == "10" ~ "Housing & Urban Affairs",
        ministry_code == "11" ~ "Ecclesiastical Affairs",
        ministry_code == "12" ~ "Business & Industry",
        ministry_code == "13" ~ "Economic Affairs",
        ministry_code == "14" ~ "Health",
        ministry_code == "15" ~ "Education",
        ministry_code == "16" ~ "Food & Agriculture",
        ministry_code == "17" ~ "Fisheries",
        ministry_code == "18" ~ "Transport",
        ministry_code == "19" ~ "Energy & Climate",
        ministry_code == "20" ~ "Communication & Tourism",
        ministry_code == "21" ~ "Culture",
        ministry_code == "22" ~ "Nordic Affairs",
        ministry_code == "23" ~ "Taxation",
        ministry_code == "24" ~ "Greenland Affairs",
        ministry_code == "25" ~ "Minister without Portfolio",
        ministry_code == "26" ~ "Budgetary Affairs",
        ministry_code == "27" ~ "European Affairs",
        ministry_code == "28" ~ "Research & Science",
        ministry_code == "29" ~ "Development & Foreign Aid",
        ministry_code == "30" ~ "Economic Coordination",
        ministry_code == "31" ~ "Business Coordination",
        ministry_code == "32" ~ "Immigration & Integration",
        ministry_code == "33" ~ "Gender Equality",
        ministry_code == "35" ~ "Family Affairs",
        ministry_code == "36" ~ "Civil Service Affairs",
        ministry_code == "37" ~ "Climate, Energy & Buildings",
        ministry_code == "38" ~ "Welfare",
        ministry_code == "39" ~ "Trade & Investment",
        ministry_code == "40" ~ "Elderly Affairs",
        ministry_code == "41" ~ "Public Innovation",
        ministry_code == "42" ~ "Children & Social Affairs",
        TRUE ~ NA_character_
    )
}


# Function to link ministries to major topics
link_ministries <- function(major_topic_code, list_current_ministries) {
    
    x <- case_when(
        major_topic_code == 1 ~ list(c("Finance", "Economic Affairs", "Budgetary Affairs", "Economic Coordination")),
        major_topic_code == 2 ~ list(c("Justice", "Gender Equality")),
        major_topic_code == 3 ~ list(c("Health")),
        major_topic_code == 4 ~ list(c("Food & Agriculture", "Fisheries")),
        major_topic_code == 5 ~ list(c("Employment")),
        major_topic_code == 6 ~ list(c("Education")),
        major_topic_code == 7 ~ list(c("Environment")),
        major_topic_code == 8 ~ list(c("Energy & Climate", "Climate, Energy & Buildings")),
        major_topic_code == 9 ~ list(c("Immigration & Integration")),
        major_topic_code == 10 ~ list(c("Transport")),
        major_topic_code == 12 ~ list(c("Justice", "Family Affairs")),
        major_topic_code == 13 ~ list(c("Social Affairs", "Welfare", "Elderly Affairs", "Children & Social Affairs")),
        major_topic_code == 14 ~ list(c("Housing & Urban Affairs")),
        major_topic_code == 15 ~ list(c("Business & Industry", "Business Coordination", "Trade & Investment")),
        major_topic_code == 16 ~ list(c("Defence")),
        major_topic_code == 17 ~ list(c("Communication & Tourism", "Research & Science")),
        major_topic_code == 18 ~ list(c("Trade & Investment", "Foreign Affairs")),
        major_topic_code == 19 ~ list(c("Foreign Affairs", "Nordic Affairs", "European Affairs", "Development & Foreign Aid")),
        major_topic_code == 20 ~ list(c("Prime Minister", "Interior", "Ecclesiastical Affairs", "Civil Service Affairs", "Public Innovation", "Minister without Portfolio")),
        major_topic_code == 21 ~ list(c("Greenland Affairs")),
        major_topic_code == 23 ~ list(c("Culture")),
        TRUE ~ list(NA_character_)
    )[[1]]

   intersect(x, list_current_ministries)
}


# Function to link ministries to WhoGov positions
link_ministries_whogov <- function(port_label) {
  case_when(
    port_label %in% c(
      "Min. Of Foreign Affairs", 
      "Min. For Foreign Affairs"
    ) ~ "Foreign Affairs",
    
    port_label %in% c(
      "Min. Of Finance", 
      "Min. For Finance"
    ) ~ "Finance",
    
    port_label %in% c(
      "Min. Of Employment", 
      "Min. Of Employment & Equality", 
      "Min. For Employment"
    ) ~ "Employment",
    
    port_label %in% c(
      "Min. Of Social Affairs", 
      "Min. Of Social Affairs & Gender Equality", 
      "Min. Of Social Affairs & Integration"
    ) ~ "Social Affairs",
    
    port_label %in% c(
      "Min. Of Justice", 
      "Min. For Justice"
    ) ~ "Justice",
    
    port_label %in% c(
      "Min. Of Interior Affairs", 
      "Min. Of Interior Affairs & Health", 
      "Min. Of Interior Affairs & Social Welfare", 
      "Min. For Social Affairs & The Interior"
    ) ~ "Interior",
    
    port_label == "Min. Of Defense" ~ "Defence",
    
    port_label %in% c(
      "Min. Of Environment", 
      "Min. Of Environment & Nordic Affairs", 
      "Min. For Environment & Food"
    ) ~ "Environment",
    
    port_label %in% c(
      "Min. Of Towns, Housing & Rural Affairs", 
      "Min. For Immigration, Integration & Housing"
    ) ~ "Housing & Urban Affairs",
    
    port_label %in% c(
      "Min. Of Education & Ecclesiastical Affairs", 
      "Min. Of Refugees, Immigration, Integration & Ecclesiastical Affairs", 
      "Min. Of Ecclesiastical Affairs", 
      "Min. For Culture & Ecclesiastical Affairs"
    ) ~ "Ecclesiastical Affairs",
    
    port_label %in% c(
      "Min. Of Economic Affairs, Business & Trade", 
      "Min. Of Economic & Business Affairs", 
      "Min. Of Business Affairs & Growth", 
      "Min. For Business Affairs & Growth"
    ) ~ "Business & Industry",
    
    port_label == "Min. Of Economic Affairs" ~ "Economic Affairs",
    
    port_label %in% c(
      "Min. Of Health & Prevention", 
      "Min. Of Health", 
      "Min. For Health"
    ) ~ "Health",
    
    port_label %in% c(
      "Min. Of Education", 
      "Min. Of Children & Education", 
      "Min. For Children, Education & Gender Equality"
    ) ~ "Education",
    
    port_label %in% c(
      "Min. Of Food, Agriculture & Fisheries", 
      "Min. Of Agriculture, Food & Fisheries"
    ) ~ "Food & Agriculture",
    
    port_label %in% c(
      "Min. Of Transport & Energy", 
      "Min. Of Transport", 
      "Min. For Transport & Building"
    ) ~ "Transport",
    
    port_label %in% c(
      "Min. Of Climate & Energy", 
      "Min. Of Climate, Energy & Construction", 
      "Min. For Energy, Utilities & Climate"
    ) ~ "Energy & Climate",
    
    port_label == "Min. Of Cultural Affairs" ~ "Culture",
    
    port_label %in% c(
      "Min. Of Education & Nordic Cooperation", 
      "Min. Of Nordic Cooperation", 
      "Min. For Nordic Cooperation"
    ) ~ "Nordic Affairs",
    
    port_label %in% c(
      "Min. Of Taxation", 
      "Min. For Taxation"
    ) ~ "Taxation",
    
    port_label == "Min. Of European Cooperation" ~ "European Affairs",
    
    port_label %in% c(
      "Min. Of Science, Technology & Innovation", 
      "Min. Of Research, Innovation & Continuing Education", 
      "Min. For Higher Education & Science"
    ) ~ "Research & Science",
    
    port_label == "Min. For Development Cooperation" ~ "Development & Foreign Aid",
    
    port_label %in% c(
      "Min. Of Refugees, Immigration & Integration"
    ) ~ "Immigration & Integration",
    
    port_label %in% c(
      "Min. Of Gender Equality", 
      "Min. Of Equality"
    ) ~ "Gender Equality",
    
    port_label %in% c(
      "Min. Of Family & Consumer Affairs", 
      "Min. Of Children & Equality"
    ) ~ "Family Affairs",
    
    port_label == "Min. Of Social Welfare & Gender Equality" ~ "Welfare",
    port_label == "Min. Of Trade & Investment" ~ "Trade & Investment",
    
    TRUE ~ NA_character_
  )
}
