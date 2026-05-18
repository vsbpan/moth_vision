# Load package
vmisc::load_all2("mothr")

# See explanation at https://github.com/vsbpan/moth_vision/tree/main/doc/image_organization

# I store everything in my `D:/` disk. You might want to change that.  
options(
  "database_path" = "C:/"
)

# Step 1
## Initiate expected file tree
init_image_dir()
## Move images to pending folder
collect_images()

# Step 2
## Visually inspect the photos and make corrections.
shell(sprintf("Open %s", get_pending_path()))

# Step 3
## Merge the validated photos to the database
# Change the `historic` argument to `TRUE` if the specimens are historic specimens
new_files <- merge_to_database(historic = FALSE)

# You are probably done here if everything ran smoothly. Now please back up the entire moth_photos folder.


#### optional ####


# Step 4 (optional)
## Find the paths to the new images that you want to register
path_to_new_files <- list.files(get_database_path(), 
                                full.names = TRUE, 
                                recursive = TRUE, 
                                ignore.case = TRUE, 
                                pattern = ".jpg")

## Register images that are not found in the database
register_image_id(path_to_new_files)

## If you registered new images, then you want to push the changes to the main branch on GitHub. 


# Step 5 (optional)
# If you want to remove images you accidentally merge to the database, please flag them by supplying a vector of file names.
# The function doesn't check if the file exists, so make sure that you entered in the file name correctly. 
flag_image_exclude("FILE NAME OF THE EXCLUDED IMAGE. e.g., img_moth_99999991.jpg")









