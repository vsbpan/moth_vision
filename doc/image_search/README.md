# Image search

## Getting started

Here, I document some handy functions that can be used to search the image database.

First, load the custom library

```{r}
vmisc::load_all2("mothr")
```

Depending on where your image database is stored, you might want to change the root folder:

```{r}
options(
  "database_path" = "C:" # This is the default
)
```

The `find_path()` function takes many different kinds of indentifying information formatted in different ways. You can use the `launch_photo()` function to quickly open the image once you have the path.

```{r}
# Use `tag_id` to find path
find_path(tag_id = "WPC110") # or "WPC-110"
# Expects "C:/moth_photos/database/batch_09/img_moth_00017417.jpg"

# Use `file_name` to find path
find_path(file_name =  "img_moth_00001023.jpg")

# Use `image_id` to find path
find_path(image_id  =  "123300000") # or "img123300000"

# Use `mini_moth` to find path
find_path(mini_moth  =  "mini_moth_00001023.jpg") 

# Optionally, launch the file via shell script
find_path(tag_id = "2023DCR5586") %>% # or "DCR-2023-5586"
  launch_photo()

```
