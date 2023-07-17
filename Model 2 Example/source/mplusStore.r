# move Mplus outputs and draws into folder
mplus_out = list.files(path = "mplus/", pattern=".out", full.names = F)
file.copy(from = paste0("mplus/", mplus_out),
          to = paste0(path_mplus, mplus_out),
          overwrite = T)
file.remove(list.files(path = "mplus/", pattern=".out", full.names = T))

mplus_out = list.files(path = "mplus/", pattern=".dat", full.names = F)
file.copy(from = paste0("mplus/", mplus_out),
          to = paste0(path_mplus, mplus_out),
          overwrite = T)
file.remove(list.files(path = "mplus/", pattern=".dat", full.names = T))
