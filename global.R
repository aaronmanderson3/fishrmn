library(lubridate)
library(janitor)

#### Function to get lakes in given counties ####

#### Function to get survey headers for given lakes ####
get_survey_headers <- function(lake_id) {

	map_dfr(lake_id, ~ {
		paste0("https://maps2.dnr.state.mn.us/cgi-bin/lakefinder/detail.cgi?type=lake_survey&id=", .) %>%
			read_json(simplifyVector = T) %>%
			pluck("result") %>%
			pluck("surveys") %>%
			pluck("headerInfo") %>%
			map(set_names, c("lake", "NA", "lake_id", "survey_date", "survey_id", "survey_type")) %>%
			bind_rows()
		#select(surveyID, surveySubType, surveyDate) %>%
		#unite("name", surveyDate, surveySubType, sep = " - ")
	}) %>%
		mutate(lake = paste0(lake," (",lake_id,")")) %>%
		select(-`NA`, -lake_id) %>%
		split(.$lake) %>%
		modify(unite, col = "survey", sep = " - ", survey_date, survey_type) %>%
		modify(select, survey, survey_id) %>%
		modify(deframe)
}


#### Function to get survey data for given lakes and surveys ####
lake_id <- c("86011400", "10005900")
survey_id <- c("15831914852624001", "10081413240649003", "15111622126525000", "5171415333822000")

get_survey <- function(lake_id, survey_id) {

	map_chr(lake_id, ~ paste0("https://maps2.dnr.state.mn.us/cgi-bin/lakefinder/detail.cgi?type=lake_survey&id=", .)) %>%
		map(read_json, simplifyVector = T) %>%
		map("result") %>%
		set_names(map(., "lakeName")) %>%
		map("surveys") %>%
		map(filter, surveyID %in% survey_id) %>%
		map_dfr(as_tibble, .id = "lake") %>%
		nest(lengths = lengths) %>%
		select(lake, surveyDate, surveySubType, fishCatchSummaries, lengths, narrative) %>%
		arrange(lake, desc(surveyDate)) %>%
		mutate_all(type.convert)

}

survey_data <- get_survey(lake_id, survey_id)
.fish_species <- fish_species_codes$Panfish
get_survey_cpue <- function(survey_data, .fish_species = NULL) {

	if(is.null(.fish_species))
		.fish_species = flatten(fish_species)

	survey_data %>%
		select(lake:fishCatchSummaries) %>%
		unnest(cols = fishCatchSummaries) %>%
		filter(species %in% .fish_species) %>%
		select(!contains("Weight"), -gearCount, -totalCatch) %>%
		extract(quartileCount,
						into = c("quartile_min", "quartile_max"),
						regex = "(.+)-(.+)",
						convert = T) %>%
		filter(!is.na(quartile_min)) %>%
		mutate(species = recode(species, !!!fish_species_names)) %>%
		clean_names

}

ggplot(get_survey_cpue(survey_data, .fish_species = flatten(fish_species_codes)),
			 aes(x = lake,
			 		y = cpue,
			 		ymin = quartile_min,
			 		ymax = quartile_max,
			 		color = lake,
			 		fill = lake,
			 		shape = gear,
			 		group = interaction(lake, survey_date, gear))) +
	geom_point(position = "dodge") +
	geom_tile(aes(y = (quartile_min + quartile_max) / 2,
								width = 0.5,
								height = quartile_max - quartile_min),
						position = "dodge",
						color = NA,
						alpha = 0.25) +
	coord_flip() +
	facet_wrap(. ~ species, scales = "free")
