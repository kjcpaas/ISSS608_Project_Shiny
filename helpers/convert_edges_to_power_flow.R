convert_edges_to_power_flow <- function(edges) {
  if(nrow(edges) == 0) {
    return(edges)
  }
  
  # Employee -> Employer, weight: 3
  works_for <- edges %>% filter(subtype == "WorksFor") %>% mutate(weight = 3)
  
  # Person1 -> Person2, weight: 2
  family <- edges %>% filter(subtype == "FamilyRelationship") %>% mutate(weight = 2)
  
  # Shareholder <- Company, weight: 3
  shareholder <- edges %>% filter(subtype == "Shareholdership") %>%
    mutate(
      temp = from,
      from = to,
      to = temp,
      weight = 3,
      # Rename to prevent confusion due to reversed arrows
      subtype = "HasShareholder",
    ) %>%
    select(from, to, supertype, subtype, start_date, end_date, weight, included)
  
  # BeneficialOwner <- Company, weight: 5
  owner <- edges %>% filter(subtype == "BeneficialOwnership") %>%
    mutate(
      temp = from,
      from = to,
      to = temp,
      weight = 5,
      # Rename to prevent confusion due to reversed arrows
      subtype = "OwnedBy",
    ) %>%
    select(from, to, supertype, subtype, start_date, end_date, weight, included)
  
  works_for %>%
    rbind(family) %>%
    rbind(shareholder) %>%
    rbind(owner)
}