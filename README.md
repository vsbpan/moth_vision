# moth vision

## Table of Contents

1.  [Overview](#Overview)
2.  [License & usage](#License)
3.  [Installation](#Installation)
4.  [Description](#Description)
    -   [Code libraries](#CodeLibraries)
    -   [File structure](#FileStructure)

## Overview <a name="Overview"></a>

This repo hosts code for misc computer vision stuff for Eric's moth collections

## License & usage <a name="License"></a>

The code in this repo is licensed under GNU GPLv3. The data belong to Eric LoPresti (University of South Carolina). Contact Eric LoPresti for any use of data in this repo. 

## Installation <a name="Installation"></a>

Run `Package_installation.R` to install repository R dependencies. For the dependency *vmisc*, see [vmisc](https://github.com/vsbpan/vmisc) GitHub page on installation instructions. All the poster processing / analysis and non-deeplearning related tasks can be done with with the R package.

To use the COCO-annotator, simply run, the following code after launching Docker desktop:

``` bash        
cd .\coco-annotator 
start "firefox.exe" http://localhost:5000/ 
Call docker-compose up
```

For computer vision tasks that uses deeplearning or more expensive image manipulations that involve the predictions of Mask-R-CNN, the python dependencies need to be installed.

``` bash        
# Create conda environment to install stuff into
conda create -n "mothz" python=3.8.2
conda activate mothz

# Begin with detectron2 installations
# Check CUDA version
nvcc --version
# Find the right pytorch versions for the CUDA version installed https://pytorch.org/
# conda install pytorch==2.5.1 torchvision==0.20.1 torchaudio==2.5.1 pytorch-cuda=12.1 -c pytorch -c nvidia
python -m pip install -e detectron2 to install Detectron2

# Install Jupyter notbook
pip install jupyterlab

# Install other packages
pip install opencv-contrib-python numpy matplotlib tensorflow pytesseract

# I also installed some mothra dependencies but that is not required.
# see `detectron2/requirements.txt` for detectron2 requirements
# see `detectron2/SORT_requirements.txt` for SORT requirements (beetle tracking stuff)
```

To do anything with python using the jupyter notebooks, run

``` bash        
cd .
Call conda activate mothz
Call jupyter lab
```

To do anything with pytasseract,

``` python
# Follow instructions to install tesseract
# https://github.com/tesseract-ocr/tessdoc/blob/main/Installation.md
# Find out where `tesseract.exe` is installed. 
# `read_img_tags()` and `read_text()` has an argument `tesseract_loc` where you can specify the location of the `tesseract.exe` file.
# Or do this for windows:
tesseract_loc = 'C:/Program Files/Tesseract-OCR/tesseract.exe'
pytesseract.pytesseract.tesseract_cmd = tesseract_loc

```

Some file manipulations may require the *ExifTool* software via the *exifr* package, but the package would ask you to install the program if it cannot be found. 

## Description <a name="Description"></a>

### Code libraries <a name="CodeLibraries"></a>

There are two custom libraries used in this project, one written in R and the other in python.

#### Python library *detectron2*

This is a fork from the *detectron2* package released by facebook. I've tried to avoid editing the package source code directly so that the installation would be less of a pain. The new code additions are added in `detectron2/detectron2/custom/` and imported into the jupyter notebook as needed. If those functions are not imported, then the package should behave more or less the same as the version (v0.6) released.

#### R library *mothr*

The custom code written for this project are bundled as a pseudo simulated package *mothr*. It can be imported using `vmisc::load_all2("mothr")`, or `pkgload::load_all("mothr")` to enter developer mode. The latter mode allows newly recompiled C++ code to be included in the package, but copies the .dll file for each time it is initiated, which can cause memory overflow, especially in multi-session parallel computing.

##### Key objects

-   Image representations
    -   `cimg`: RGB image tensor array from *imager* that represents an image in R. Can be used with functions from *imager*, *imagerExtra*, and *mothr*.
    -   `pixset`: Binary image tensor array from *imager* that represents binary masks in R. Can be used with functions from *imager*, *imagerExtra*, and *mothr*. Generic methods implemented in *mothr* include `plot()`, `area()`, `centroid()`, `as.polygon()`, `IOU()`.
    -   `imlist`: A list of `cimg` or `pixset` objects.
-   Annotation geometries
    -   `bbox`: Bounding box encoded as a 2 X 2 matrix of lower left and top right corner coordinates. Has generic methods such as `print()`, `area()`, `centroid()`, `as.polygon()`, `as.pixset()`, `IOU()`, `plot()`.
    -   `polygon`: Polygon encoded as a n X 2 matrix of margin coordinates. This is a more efficient representation of binary masks (`pixset` objects). Has generic methods such as `print()`, `area()`, `centroid()`, `as.bbox()`, `as.pixset()`, `IOU()`, `plot()`.
    -   `keypoint`: keypoints encoded as n_keypoints X 3 matrix of keypoint coordinates and score or flag. Has generic methods such as `print()`, `plot()`.
-   Detection instances
    -   `instance`: One instance of object detection composed of a list of annotation geometries, instance_id, image_id, score, and thing_class. Has generic methods such as `print()`, `plot()`, `as.bbox()`, `as.pixset()`,`find_things()`, `find_labels()`.
    -   `inlist`: A list of `instance` objects associated with an image. Has generic methods such as `print()`, `[]`, `c()`, `plot()`, `as.bbox()`, `as.pixset()`,`find_things()`, `find_labels()`.
-   Annotation files
    -   `raw_inference`: Raw inference file composed of the two .csv files created by *detectron2*. Can be converted to `parsed_inference` or `COCO_Json` objects.
    -   `parsed_inference`: A cleaned inference file formatted more like a COCO annotation file, but uses the nice tibble nested list function to store the `inlist` objects as entries of image metadata, which is a tibble. Supports various COCO evaluators, `bbox_evaluator()`, `mask_evaluator()`, and `keypoint_evaluator()`. This should be the primary format with which to interact with *detectron2* predictions.
    -   `COCO_Json`: A list of data.frames and lists that represents a COCO annotation file. This is the file format that COCO annotator would take. Has functions for `import_COCO()`, `export_COCO()`, `split_COCO()`, `merge_COCO()`, `sample_COCO()`, `subset_COCO()`, `wipe_annotations_COCO()`, `update_manual_COCO()`, `set_new_path_COCO()`, `as.parsed_inference()`,`print()`, and `summary()`.

### File structure <a name="FileStructure"></a>

Currently, Git is set to ignore the following directories (mainly due to file size limitations)
``` bash
/coco-annotator/datasets
/graphs
/invisible
/detectron2/custom_training
```

-   `mothr/` The root directory of the *mothr* package. `.R` files in this directory are not loaded in the package.
    -   `mothr/R/` Contains the R source code
    -   `mothr/src/` Contains the C++ source code
    -   `mothr/assets/` Contains data used for the functioning for the *mothr* package
        - `color_index_formulas.csv` Different RGB transformation formulas modified from *pilman*
        - `exclude_flags.csv` A single column of the file name of photos which should be excluded from analysis due to some error one way or another.  
        - `historic_specimens.csv` A single column of the file name of photos which are flagged as being from the historic specimens collection. Used for deciding which tag_id read output from the OCR module with different settings should be kept.  
        - `image_database.csv` A two-column table of the photo file_name to image_id mapping. 
        - `inference_error.csv` A single column of the file name of photos which should be excluded from analysis due to some problem with the computer vision inference. 
        - `real_tag_id.csv` A two-column table of the photo file_name to tag_id mapping that has been manually entered. Overrides whatever the OCR system guesses the tag_id is in the photo. 
    -   `DESCRIPTION` Package description file
    -   `NAMESPACE` Namespace file
    -   `.Rbuildignore` What to ignore in building the *mothr* package
    -   `Package_installation.R` a handy script to help you install *mothr* R dependencies. 
-   `archive/` Old and obsolete scripts used to development
-   `cleaned_data/` Stores cleaned data
-   `coco-annotator/` The folder for all *coco-annotator* related program files
-   `COCO_annotations/` The folder for storing *coco-annotator* generated annotation files 
    -   `coco_kp_test.json` Obsolete test keypoint annotations
    -   `coco_mask_test.json` Obsolete test mask annotations
    -   `mothz_sample1_fullset_keypoints.json` Sample 1 keypoint annotations
    -   `mothz_sample1_fullset_mask.json` Sample 1 mask annotations
-   `detectron2/` The folder for all *detectron2* related program files
-   `doc/` Some package and repo documentation
-   `inference/` Stores inference output from the *detectron2* pipeline
    -   `all_batches/` Stores inference output for the photo database. 
    -   `full_mothz_sample1_*` Store inference output for sample 1 (including train, test, and val datasets).
-   `misc/` Misc scripts that are currently in use for something but not organized. These scripts are not part of any pipeline. 
-   `raw_data/` Stores raw data
-   `scripts/`
    -   `cleaning/` Some scripts for cleaning
        -   `mini_moth.R` Script for generating cached mini_moth images for faster processing
        -   `flagging.R` Script for flagging photos
    -   `pre_inference_processing/` Some scripts for pre-inference tasks
        -   `coco_split.R` Script for splitting the coco annotation file into training, testing, and validation datasets
        -   `training_data_sample.R` Script for sampling training data from the image database
    -   `post_inference_processing/` Some scripts for post-inference tasks
        -   `COCO_evaluation.R` Script for benchmarking the *detectron2* model
        -   `merge_inferences.R` Script for merging and parsing the raw inferences for all batches
    -   `training_and_inference/` Some scripts for training the *detectron2* model and performing inference
-   `README.md` The thing that you are reading right now

### General workflow
#### Inference on new photos
1. Run `scripts/training_and_inference/model_v1_kp_train.ipynb` and `scripts/training_and_inference/model_v1_mask_train.ipynb` on new photos in the image database `./moth_photos/database/`. The new raw inference files should be written in `inference/all_batches/`. 
2. Run the following, 

``` R
parsed_mask <- import_raw_inference(path_meta = "inference/batch_*_image_meta_mask.csv", 
                          path_inference = "inference/batch_*_inference_mask.csv") %>% 
                    as.parsed_inference()
parsed_kp <- import_raw_inference(path_meta = "inference/batch_*_image_meta_keypoint.csv", 
                           path_inference = "inference/batch_*_inference_keypoint.csv") %>% 
                    as.parsed_inference()

my_object <- merge_parsed_inference(parsed_mask, parsed_kp)
```

The `my_object` is a `parsed_inference` object that can then be used for further analyses. 

3. For analysis of images in R, it is faster to work with the cached mini moth files, which are generated from the original image cropped at the bounding box. Run `scripts/cleaning/mini_moth.R` to make those files.  


### Ice box specific python script initiation

Tad's Ice box has a slightly different launching procedure for python related stuff. I document the steps I took that got it to work. 

First, enter the right directory then active the Conda environment:  
``` bash
cd ~/mothz 
conda activate mothz
```

We want to expose the path of the correct CUDA installation: 
``` bash
export CUDA_HOME=/usr/local/cuda
```

Now we can launch the jupyter notebook: 
``` bash 
jupyter lab
```








