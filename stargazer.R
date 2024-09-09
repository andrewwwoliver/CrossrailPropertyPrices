library(stargazer)


# Export the multilevel model to LaTeX
wordreg(list(multilevel_model, multilevel_slm, multilevel_sample_model),
        file = "multilevel_model.docx", 
        single.row = FALSE,
        custom.coef.names = property_custom_coef_names,
        custom.model.names = c("Multilevel", "Spatial Lag", "Multilevel Sample"),
        )

custom_coef_names <- c(
"(Intercept)", 
"Close to Crossrail", 
"Post Treatment", 
"Log Population Density", 
"Avg Bedrooms", 
"Average Qualification Level 4", 
"Employment Pct 60", 
"Old/New (Y)", 
"Log Subcentre Distance", 
#"Flat",
#"Semi-Detached",
#"Terraced",
"Noise Class >= 55", 
"Log Crime Count", 
"Access to Parks", 
"Close to Crossrail * Post Treatment", 
"Rho"  
)
# detached
wordreg(list(detached_lmer_model, detached_slm),
        file = "detached_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Detached", "Spatial Lag"),
        )

# flat
wordreg(list(flat_lmer_model, flat_slm, flat_sample_model),
        file = "flat_model2.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Flat", "Spatial Lag", "Flat Sample"),
        )

# terraced
wordreg(list(terraced_lmer_model, terraced_slm, terraced_sample_model),
        file = "terraced_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Terraced", "Spatial Lag", "Terraced Sample"),
        )

# semi-detached
wordreg(list(semi_detached_lmer_model, semi_detached_slm, semi_detached_sample_model),
        file = "semi_detached_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Semi-Detached", "Spatial Lag", "Semi-Detached Sample"),
        )

# western detached
wordreg(list(western_detached_lmer_model, western_detached_slm),
        file = "western_detached_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Western Detached", "Spatial Lag"),
        )

# eastern detached
wordreg(list(eastern_detached_lmer_model, eastern_detached_slm),
        file = "eastern_detached_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Eastern Detached", "Spatial Lag"),
        )



# western flat
wordreg(list(western_flat_lmer_model, western_flat_slm, western_flat_sample_model),
        file = "western_flat_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Western Flat", "Spatial Lag", "Western Flat Sample"),
        )

# eastern flat
wordreg(list(eastern_flat_lmer_model, eastern_flat_slm, eastern_flat_sample_model),
        file = "eastern_flat_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Eastern Flat", "Spatial Lag", "Eastern Flat Sample"),
        )

# central flat
wordreg(list(central_flat_lmer_model, central_flat_slm),
        file = "central_flat_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Central Flat", "Spatial Lag"),
        )

# western terraced
wordreg(list(western_terraced_lmer_model, western_terraced_slm, western_terraced_sample_model),
        file = "western_terraced_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Western Terraced", "Spatial Lag", "Western Terraced Sample"),
        )

# eastern terraced
wordreg(list(eastern_terraced_lmer_model, eastern_terraced_slm, eastern_terraced_sample_model),
        file = "eastern_terraced_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Eastern Terraced", "Spatial Lag", "Eastern Terraced Sample"),
        )

# central terraced
wordreg(list(central_terraced_lmer_model, central_terraced_slm),
        file = "central_terraced_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Central Terraced", "Spatial Lag"),
        )

# western semi-detached
wordreg(list(western_semi_detached_lmer_model, western_semi_detached_slm, western_semi_detached_sample_model),
        file = "western_semi_detached_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Western Semi-Detached", "Spatial Lag", "Western Semi-Detached Sample"),
        )

# eastern semi-detached
wordreg(list(eastern_semi_detached_lmer_model, eastern_semi_detached_slm, eastern_semi_detached_sample_model),
        file = "eastern_semi_detached_model.docx", 
        single.row = FALSE,
        custom.coef.names = custom_coef_names,
        custom.model.names = c("Eastern Semi-Detached", "Spatial Lag", "Eastern Semi-Detached Sample"),
        )

# central semi-detached
wordreg(list(central_semi_detached_lmer_model),
        file = "central_semi_detached_model.docx", 
        single.row = FALSE,
        custom.coef.names = norho_custom_coef_names,
        custom.model.names = c("Central Semi-Detached"),
        )

#accessibility
wordreg(list(western_flat_lmer_model, western_flat_travel_model, western_flat_notravel_model, western_flat_slm_no_transport),
             file = "western_flat_accessibility.docx",
             single.row = FALSE,
             #custom.coef.names = custom_coef_names,
             custom.model.names = c("Employment Accessibility", "CBD Distance", "Neither", "Spatial"),)

# central terraced accessibility
wordreg(list(central_terraced_lmer_model, central_terraced_travel_model, central_terraced_notravel_model, central_terraced_slm_no_transport),
             file = "central_terraced_accessibility.docx",
             single.row = FALSE,
             #custom.coef.names = custom_coef_names,
             custom.model.names = c("Employability Accessibility", "CBD Distance", "Neither", "Spatial"),)

