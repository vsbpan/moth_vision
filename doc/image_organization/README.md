# Image Files Organization

## Getting started

Here, I document some information on how the moth image collection is organized. To do anything slightly complicated with the images, we want to use the custom R package *mothr*. In the demo code below, I assume that the package is loaded.

```{r}
vmisc::load_all2("mothr") # Load the custom library
```

There are several IDs associated with each image and they might have slightly different names under different contexts. Here is some quick disambiguation, with more details explained in the workflow section below:

1.  **file_name**:
    1.  raw: These file names usually look something like `IMG_0003.JPG` , with the four digits differing between images. The name comes directly from the Cannon camera itself. An annoying feature of this naming scheme is that the file names are not unique, so that images taken on different days can share the same file name. In addition, the files are kept unique by the position in different directories, so moving them to another directory changes the identity of the file.
    2.  processed: The file names are usually something like `img_moth_00013450.jpg`. The eight zero padded digits following `img_moth_` is the order at which the image is renamed. Each file name is unique and there can be gaps between numbers as images are deleted. Note that the eight-digit number is different from the image_id.
2.  **image_id**:
    1.  The image id usually look something like `1100000` . It is an integer number that counts up from the 6th digit. The last five digits are reserved for unique instances associated with each image. When an image is registered, a unique image_id is assigned to the image based on its file name (the path is ignored). This is the id which we use to identify specific images when dealing with COCO annotation formats (`COCO_Json`), `raw_inference`, and `parsed_inference` objects. With some luck, the COCO annotator might even honor these identifiers. There can be multiple image_ids (images) associated with each specimen.
3.  **instance_id**:
    1.  The instance ID usually looks something like `1100004`, where the last five digits are unique identifiers for different instances and the first n digits up to the 6th digit is the image_id. There can be multiple instance_id associated with each unique image. This is the id which we use to identify specific images when dealing with COCO annotation formats (`COCO_Json`), `raw_inference`, and `parsed_inference` objects. With some luck, the COCO annotator might even honor these identifiers.
4.  **tag_id**:
    1.  The tag id is a unique specimen identifier that usually looks like `2024DCR3203` . This is the identifier which is printed on one of the photographed tags in each image. It is also the same identifier which relate each specimen to the specimen metadata spreadsheet. Not all specimens have a tag id. As I understand it, only the photographed ones do.
    2.  *pytesseract* is optimized to only detect the characters `0-9` and `DCR` and the R pipeline can only parse YEAR-DCR-# or YEAR-DCR-C-#. Anything else, the pipeline would fail, so **please stick to the same naming scheme**. Currently, the `tag_id` in the online spreadsheet is formatted as DCR-YEAR-# with no zero padding. A naive tag_id matching would fail without more processing.
    3.  There probably needs to be one version that is guessed by *pytesseract* and another version that is manually verified or corrected.

## Workflows

### Adding new images to the database

#### 0. Preamble

In this section, I go over how one would add the new images taken directly from the Cannon camera to the database of cleaned images used for analysis.

#### 1. Image collection

After a photography session (or whenever you are ready), find the new photos (and subdirectories) in `C:/Users/LoPresti Lab/Pictures/new_cannon_photos/` . Move the folders that contain the new photos to a directory called `C:/moth_photos/input_photos`. If such a directory does not exist, you can initiate the file tree with the code:

```{r}
init_image_dir(root_path = "C:/")
```

The root path can be changed from `C:/` to something else with the `root_path` argument.

Run the code below to collect all the photos in a new directory called `C:/moth_photos/pending_merge`. The photos are copied, so if something goes wrong, you still have the `moth_photos/input_photos` to restore your progress.

```{r}
collect_images(root_path = "C:/")
```

#### 2. Image validation

Next, you want to check the images in the `moth_photos/pending_merge/` folder for any obvious errors and remove those images. Generally, we want to:

1.  Remove duplicate images. Keep the best one for each specimen.
2.  Remove photos without moths.
3.  Remove corrupted images.
4.  Fix the orientation of the images such that the moth is oriented up right (forewings above hindwings). If the moth is not oriented upright in the photo, prioritize keeping the text of the tags oriented correctly, otherwise the text recognition would fail. It is also good to keep the color checker upright with the ruler at the bottom.

#### 3. Merging pending images

Once the images in the `moth_photos/pending_merge/` folder passed the visual validation, we want to merge them to the image database which we went through great lengths to keep clean. Run the code below to do so. When the merging is complete, **a garbage collector would try to wipe the files in the** `moth_photos/input_images/` **and** `moth_photos/pending_merge/`. Every 2000 images will be put into a new subdirectory `moth_photos/database/batch_**`.

```{r}
merge_to_database(root_path = "C:/")
```

There is currently no checks implemented to check if you are merging duplicate images due to computation limitations, so be careful not to merge a set of images multiple times! Contact Vincent for code to check if an image has already been entered in the database or run `image_in_database("MY_IMG_FILE_PATH",root_path = "C:/", quiet = FALSE)`.

#### 4. Image registration (optional)

When new images are added to the database, we need to register the image file names so that a unique image_id is assigned to each new file name. The reason is that annotation files need to have integer numbers as the identifier and we want to keep track of which image got annotated how. **Unless you are dealing with annotation files, there is no need to run this code.** If you do, you have to remember to push your changes to the repository so that subsequent registrations on other machines do not conflict with the new registrations you made.

```{r}
# Find the paths to the new images that you want to register
path_to_new_files <- list.files("MY_DIRECTORY", 
                                full.names = TRUE, 
                                recursive = TRUE, 
                                ignore.case = TRUE, 
                                pattern = ".jpg")

# Register images that are not found in the database
# Only the image file name is registered. Therefore, different images with the same file name but different file paths are treated as identical. 
# You'd want to make sure that the new files have unique names.
register_image_id(path_to_new_files)

# If all images have been registered properly, you should see a check mark. 
check_image_registration(path_to_new_files)
```

### Removing images from the database

Removing images from the database should be fairly straightforward. You can simply delete the image from the database, but note that when backing up the database, if the deleted file already exists in the folder you backup into, the deleted images will remain. This is probably a good thing anyway.

A better way to remove images from analysis / computer vision pipeline is to flag the image as exclude. The code below appends a csv file that stores the file name of all the excluded files.

```{r}
flag_image_exclude("FILE NAME OF THE EXCLUDED IMAGE.")
```

### Working with the image database

```{r}

fetch_image_database()
```

## Misc

```{r}

```
