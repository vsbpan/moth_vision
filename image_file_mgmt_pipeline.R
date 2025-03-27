# Load package
vmisc::load_all2("mothr")

# See explanation at https://github.com/vsbpan/moth_vision/tree/main/doc/image_organization

# I store everything in my `D:/` disk. You might want to change that.  
rp <- "D:/"

# Step 1
## Initiate expected file tree
init_image_dir(root_path = rp)
## Move images to pending folder
collect_images(root_path = rp)

# Step 2
## Visually inspect the photos and make corrections.
shell(sprintf("Open %s", get_pending_path(root_path = rp)))

# Step 3
## Merge the validated photos to the database
new_files <- merge_to_database(root_path = rp)

# Step 4 (optional)
## Find the paths to the new images that you want to register
path_to_new_files <- list.files(get_database_path(root_path = rp), 
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









