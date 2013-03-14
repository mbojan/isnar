data(Wnet)
coleman(as.directed(Wnet, "mutual"), "gender")

data(EFnet)
coleman( as.directed(EFnet, "mutual"), "type")
