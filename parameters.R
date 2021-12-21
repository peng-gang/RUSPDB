# parameters

aac_group <- c("12-23", "24-48", "49-168")
sex_group <- c("Male", "Female")
bw_group <- c("1000-2499", "2500-3000", "3001-3500", "3501-4000", "4001-5000")
ga_group <- c("28-36", "37-38", "39-40", "41",  "42")
race_group <- c("Asian", "Black", "Hispanic", "White")
tpn_group <- c("NoTPN", "TPN")
compare_group <- c("No Comparison", "Sex", "Birth Weight", "Gestational Age", "Race/Ethnicity", "TPN")

analytes_all <- c(
  "Glycine", "Alanine", "Proline", "Valine", "5-Oxoproline", "Leucine/Isoleucine", "Ornithine", "Methionine", "Arginine", "Citrulline", 
  "Phenylalanine", "Tyrosine", "Succinylacetone", "C0", "C2", "C3", "C4", "C5:1", "C5", "C6", 
  "C8:1", "C8", "C10:1", "C10", "C12:1", "C12", "C14:2", "C14:1", "C14", "C14OH", 
  "C16:1", "C16", "C16OH", "C18:2", "C18:1", "C18", "C18:1OH", "C18OH", "C5OH", "C3DC", 
  "C5DC"
)
  
analytes_colname <- c(
  "GLY", "ALA", "PRO", "VAL", "OXP", "XLE", "ORN", "MET", "ARG", "CIT",
  "PHE", "TYR", "SA", "C0", "C02", "C03", "C04", "C051", "C05", "C06",
  "C081", "C08", "C101", "C10", "C121", "C12", "C142", "C141", "C14", "C14OH",
  "C161", "C16", "C16OH", "C182", "C181", "C18", "C181OH", "C18OH", "C05OH", "C03DC", 
  "C05DC"
)
