convert_edges_to_power_flow <- function(edges) {
  # Employee -> Employer, weight: 2
  works_for <- edges %>% filter(subtype == "WorksFor") %>% mutate(weight = 2)
  
  # Person1 <-> Person2, weight: 1
  family <- edges %>% filter(subtype == "FamilyRelationship") %>% mutate(weight = 2)
  family_rev <- family %>%
    mutate(temp = from, from = to, to = temp) %>%
    select(from, to, supertype, subtype, start_date, end_date, weight)
  
  # Shareholder <- Company, weight: 2
  shareholder <- edges %>% filter(subtype == "Shareholdership") %>%
    mutate(
      temp = from,
      from = to,
      to = temp,
      weight = 3,
      # Rename to prevent confusion due to reversed arrows
      subtype = "HasShareholder",
    ) %>%
    select(from, to, supertype, subtype, start_date, end_date, weight)
  
  # BeneficialOwner <- Company, weight: 3
  owner <- edges %>% filter(subtype == "BeneficialOwnership") %>%
    mutate(
      temp = from,
      from = to,
      to = temp,
      weight = 5,
      # Rename to prevent confusion due to reversed arrows
      subtype = "OwnedBy",
    ) %>%
    select(from, to, supertype, subtype, start_date, end_date, weight)
  
  works_for %>%
    rbind(family) %>%
    rbind(family_rev) %>%
    rbind(shareholder) %>%
    rbind(owner)
}