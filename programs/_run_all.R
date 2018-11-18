#------------------------------------------------------------------------------#
#    TITLE : 
#     DATE : 20181118
#   AUTHOR : B. Saul
#  PURPOSE : 
#------------------------------------------------------------------------------#

###
files <- list.files(path = "programs",
                    full.names = TRUE,
                    recursive = FALSE,
                    include.dirs = FALSE)

create_data_Rfiles <- files[grepl("programs/0.*data\\.R$", files)]

for(i in seq_along(create_data_Rfiles)){
  source(file = create_data_Rfiles[i], echo = TRUE)
}

rm(list = ls())
devtools::build()