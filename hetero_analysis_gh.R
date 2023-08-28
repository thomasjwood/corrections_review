library(tidyverse)
library(magrittr)
library(estimatr)
library(emmeans)
library(broom)
library(effectsize)
library(ggtext)
library(ggbeeswarm)

t1 <- "https://github.com/thomasjwood/corrections_review/raw/main/t1.rds" %>% 
  url %>% 
  readRDS

t2 <-  "https://github.com/thomasjwood/corrections_review/raw/main/t2.rds" %>% 
  url %>% 
  readRDS

com <- t1 %>% 
  select(
    caseid, wave, country, cond, issue, out_true, birthyr, gender, ideo, party_support, educ
  ) %>% 
  rename(
    outcome = out_true,
    party = party_support
  ) %>% 
  mutate(
    birthyr = birthyr %>% 
      as.character,
    ideo = ideo %>% 
      as.character,
    study = "full fact"
  ) %>% 
  bind_rows(
    t2 %>% 
      select(
        caseid, wave, geo_country_code, condition, issue, outcome, birthyr, gender, ideology, grp_partyid, grp_education 
      ) %>% 
      rename(
        country = geo_country_code,
        cond = condition,
        party = grp_partyid,
        ideo = ideology,
        educ = grp_education
      ) %>% 
      mutate(
        ideo = ideo %>% as.character,
        study = "keen zion"
      )
  ) %>%
  filter(
    gender %>% 
      str_detect("Male|Female")
  ) %>% 
  mutate(
    gender = gender %>% 
      factor(
        c("Female", "Male")
      ),
    educ = educ %>% 
      case_match(
        c("Primary",
          "No formal education", 
          "NVQ1, NVQ2", "GNVQ / GSVQ / GCSE/ SCE standard.",
          "Secondary school (age under 15 years old)", 
          "High school completed",
          "NVQ3/ SCE Higher Grade/ Advanced GNVQ/ GCE A/AS or similar.",
          "Some high school", 
          "Some primary school",
          "Complete Secondary",
          "Complete Primay",
          "Incomplete Primary",
          "Incomplete Secondary",
          "Secondary",
          "HSD or less") ~ "HSD or less",
        c("Artisans certificate, technical/secretarial qualification obtained",
          "Complete Tertiary education (non-University)",
          "Incomplete University", 
          "Incomplete Tertiary education (non-University)",
          "Higher non-university",
          "Some college") ~ "Some college",
        c("University degree completed",
          "Technikon diploma/degree completed", 
          "Post graduate degree completed",
          "NVQ5 or post-graduate diploma.", 
          "NVQ4 / HNC / HND / Bachelor's degree or similar.",
          "Complete University",
          "Complete Postgraduate / Master / Doctor's Degree",
          "Incomplete Postgraduate / Master / Doctor's Degree",
          "University",
          "BA or more") ~ "BA or more"
      ) %>% 
      factor(
        c("HSD or less",
          "Some college",
          "BA or more")
      ),
    ideo = ideo %>% 
      case_match(
        c("Left1", "1",  "2", "3") ~ "Left",
        c("4", "5", "6", "7") ~ "Moderate",
        c("8", "9", "10", "Right10") ~ "Right",
        c("DK", "Don't know", "Prefer not to say") ~ "Prefer not to say"
      ) %>% 
      factor(
        c("Left", "Moderate", "Right", "Prefer not to say")
      ),
    agegrp = study %>% 
      equals("full fact") %>% 
      ifelse(
        2020 %>%
          subtract(as.numeric(birthyr)),
        2021 %>%
          subtract(as.numeric(birthyr))
      ) %>% 
      cut(
        c(18, 30, 45, Inf),
        labels = c("18-29", "30-44", "45+")
      ),
    out_num  = outcome  %>%
      case_match(
        "False" ~ 1,
        "Not at all accurate" ~ 1,
        "Not very accurate" ~ 2, 
        "Probably false" ~ 2,
        "Not sure" ~ NA,
        "Probably true" ~ 3,
        "Somewhat accurate" ~3,
        "True" ~ 4, 
        "Very accurate" ~ 4
      ),
    cond = cond %>% 
      case_match(
        "cond_corr" ~ "Misinformation and correction",
        "cond_items" ~ "Control - items only",
        "cond_misinfo" ~ "Misinformation",
        "Control - only outcome" ~ "Control - items only",
        "Misinformation" ~ "Misinformation",
        "Misinformation & Fact-check" ~ "Misinformation and correction") %>% 
      factor(
        c("Control - items only",
          "Misinformation",
          "Misinformation and correction")
      ),
    country2 = country %>% 
      case_match(
        c("United Kingdom","France", "Germany",  "United States") ~ "WEIRD country",
        c("South Africa", "Argentina", "nigeria", "Brazil",
          "India", "Indonesia", "Mexico", "Nigeria", 
          "Peru") ~ "Non WEIRD country"
      ) %>% 
      factor
  )


m1 <- com %>% 
  filter(wave == "wave 1") %>% 
  select(
    caseid, country, cond, issue, 
    gender, ideo, agegrp, educ,
    out_num, country2
  ) %>% 
  pivot_longer(
    names_to = "grp", 
    values_to = "vals", 
    cols = c(gender, ideo, agegrp, educ, country2), 
    values_drop_na = T
  ) %>% 
  mutate(
    vals = vals %>% 
      factor(
        com %>% 
          select(gender, ideo, agegrp, educ, country2) %>% 
          map(levels) %>% 
          unlist %>% 
          as.character
      )
  ) %>% 
  group_by(country, issue, grp) %>% 
  nest

m1$mods <- m1$data %>% 
  map2(
    m1$grp,
    \(i, j)
    
    if(
      j %>% equals("country2")
    ){
      lm_robust(
        out_num ~ cond, 
        data = i 
      )
      
    } else {
      lm_robust(
        out_num ~ cond * vals, 
        data = i 
      )  
    }
  )

m1$emm <- m1 %>% 
  ungroup %>% 
  select(mods, data, grp) %>% 
  pmap(
    \(mods, data, grp)
    
    if(
      grp %>% equals("country2")
    ){
      mods %>% 
        emmeans(
          consec ~ cond,
          data = data, 
          adjust = "none"
        )
    } else {
      
      mods %>% 
        emmeans(
          consec ~ cond | vals,
          data = data, 
          adjust = "none"
        )  
    }
  )

m1$contrasts <- m1$emm %>%
  map(
    \(i)
    i %>% 
      pluck("contrasts") %>% 
      tidy, 
    .progress = T
  )

r1 <- m1 %>% 
  ungroup %>% 
  select(
    country:grp, contrasts
  ) %>% 
  pmap(
    \(country, issue, grp, contrasts)
    contrasts %>% 
      mutate(
        country, issue, grp
      )
  ) %>% 
  list_rbind %>% 
  mutate(
    contrast = contrast %>% 
      case_match(
        "Misinformation - (Control - items only)" ~ "Misinformation effect",
        "Misinformation and correction - Misinformation" ~ "Correction effect"
      ),
    grp = grp %>% 
      case_match(
        "gender" ~ "Gender",
        "ideo" ~ "Ideology",
        "agegrp" ~ "Age",
        "educ" ~ "Education",
        "country2" ~ "National setting"
      ),
    vals = vals %>% 
      is.na %>%
      not %>%
      ifelse(
        vals, 
        country %>% 
          case_match(
            c("United Kingdom","France", "Germany",  "United States") ~ "WEIRD country",
            .default = "Non WEIRD country"
          )
      ) %>% 
      as.character %>% 
      factor(
        com %>%
          select(gender, ideo, agegrp, educ, country2) %>%
          map(levels) %>%
          unlist %>%
          as.character
      )
    )


# ordering facets

rof <- r1 %>% 
  group_by(
    grp
  ) %>% 
  nest %>% 
  mutate(
    aov = data %>% 
      map_dbl(
        \(i)
        
        # i <- r1 %>%
        #   group_by(
        #     grp
        #   ) %>%
        #   nest %>%
        #   pluck("data", 4)
        
        aov(
          estimate ~ vals, data = i
        ) %>% 
          eta_squared %>% 
          pluck("Eta2")
      )
  ) %>% 
  arrange(desc(aov)) %>% 
  ungroup %>% 
  mutate(
    grp2 = grp %>%
      str_c(
        "<br>(",
        c("<em>&eta;<sup>2</sup></em> = ") %>% 
          rep(times = 5),
        aov %>% 
          round(4) %>% 
          str_sub(2),
        ")"
      ) %>%
      fct_inorder
  )

r1 %<>% 
  left_join(rof %>% 
              select(grp, grp2))

# summaries

library(metafor)

r2 <- r1 %>% 
  group_by(
    grp2, vals, contrast
  ) %>% 
  nest %>% 
  mutate(
    mod = data %>% 
      map(
        \(i)
        
        rma(
          yi = estimate,
          sei = std.error,
          data = i
          )
        )
    )

r3 <- r2 %>% 
  ungroup %>% 
  filter(
    contrast %>% str_detect("Correction")
  ) %>% 
  select(-data, -contrast) %>%  
  pmap(
    \(vals, grp2, mod)
    mod %>% 
      tidy(conf.int = T) %>% 
      mutate(
        vals, grp2
        )
    ) %>% 
  list_rbind

r1_1 <- r1 %>% 
  mutate(
    vals = vals %>% 
      as.character
  ) %>% 
  bind_rows(
    tibble(
      vals = " ",
      grp2 = r1$grp2 %>% 
        levels %>% 
        extract2(1),
      contrast = r1$contrast %>% 
        extract2(2),
      estimate = 0
    )
  ) %>% 
  mutate(
    vals = vals %>% 
      factor(
        c(" ",
          r1$vals %>%
            levels
          )
        ),
    grp2 = grp2 %>% 
      factor(
        r1$grp2 %>% levels
      )
  )

r1_1 %>% 
  filter(
    contrast %>% str_detect("Correction")
  ) %>% 
  ggplot(
    aes(
      vals, estimate
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_beeswarm(
    alpha = .5,
    cex = 2,
    method = "center"
  ) +
  geom_linerange(
    aes(
      ymin = conf.low - .007,
      ymax = conf.high + .007,
      x = vals
    ),
    size = 1.1,
    color = "white",
    r3 
    ) +
  geom_linerange(
    aes(
      ymin = conf.low,
      ymax = conf.high,
      x = vals
    ),
    size = .5,
    color = "black",
    data = r3
  ) +
  geom_point(
    aes(
      vals, estimate, 
    ),
    shape = 21,
    size = 6,
    fill = "white",
    data = r3
  ) +
  geom_text(
    aes(
      vals, estimate, label = estimate %>% round(2) %>% as.character %>% str_replace("0.",".")
    ),
    size = 2,
    data = r3
  ) +
  labs(
    x = "",
    y = "Correction effect\n(Difference on 4pt scale)"
  ) +
  geom_segment(
    aes(x, y, xend = xend, yend = yend),
    linewidth = .5,
    arrow = arrow(
      length = unit(0.15,"cm"), 
      type = "closed"),
    data = tribble(
      ~x,     ~y, ~xend, ~yend,
       1.4,  .05,  1.4,  .4,
       1.4, -.05,  1.4, -.4
      ) %>% 
      mutate(
        grp2 = "Ideology<br>(<em>&eta;<sup>2</sup></em> = .0084)" %>%
          factor(
            r1_1$grp2 %>%
              levels
          )
      )
  ) +
  geom_richtext(
    aes(
      x, y, label = label
    ),
    fill = NA,
    label.color = NA,
    data = tribble(
      ~x, ~y, ~label,
      1.1, .3, "Corrections<br><em><b>degrade</b></em> accuracy.",
      1.1, -.3, "Corrections<br><em><b>improve</b></em> accuracy."
      
    ) %>% 
      mutate(
        grp2 = "Ideology<br>(<em>&eta;<sup>2</sup></em> = .0084)" %>%
          factor(
            r1_1$grp2 %>%
              levels
          )
      ),
    size = 2.5,
    lineheight = 0,
    angle = 90
  ) +
  facet_grid(
    . ~ grp2,
    scales = "free_x", space = "free_x"
  ) +
  scale_x_discrete(
    breaks = r1$vals %>% 
      levels,
    labels = r1$vals %>% 
      levels %>% 
      str_wrap(width = 8)
  ) +
  scale_y_continuous(
    expand = expansion(add = c(.05, .15))
  ) 