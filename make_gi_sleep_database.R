library(jsonlite)

### Jaccard similarity

### define groups based on sleep features:

behA <- c("Omi", "Vps13", "Vps35", "Synj", "Tango14", "Dj1aDj1b", "iPLA2VIA", "Rab39", "GBA")
behB <- c("Gdh", "Lrrk","CG5608", "vham89", "Punch", "Rme8", "eIF4G")
behC <- c("Park","CHCHD2", "Pink1", "Auxillin", "nutcracker", "CG32000", "Coq2", "Loqs")

### trees based on genetic interaction:
GI1 <- c("Park","Punch", "Pink1","Auxillin", "Vps13", "CHCHD2","Rab39", "Omi","nutcracker","Vps35","Gdh")
GI2 <- c("CG32000","Tango14","Lrrk","Dj1aDj1b", "iPLA2VIA","GBA","Synj")
GI3 <- c("Rme8","Coq2", "Loqs","CG5608", "vham89","eIF4G")
groups <- list(
  sleep = list(
    behA=behA,
    behB=behB,
    behC=behC        
  ),
  gi = list(
    GI1=GI1,
    GI2=GI2,
    GI3=GI3        
  )
)
jsonlite::write_json(x = groups, path = "database.json")
