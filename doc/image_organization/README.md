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
    1.  The tag id is a unique specimen identifier that usually look like `2024DCR3203` . This is the identifier which is printed on one of the photographed tags in each image. It is also the same identifier which relate each specimen to the specimen metadata spreadsheet. Not all specimens have a tag id. As I understand it, only the photographed ones do.
    2.  There probably needs to be one version that is guessed by *pytesseract* and another version that is manually verified or corrected.

## Workflow

### Adding new images to the database

#### Image registration

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

### Working with the image database

```{r}

fetch_image_database()
```

## Misc

```{r}

```
