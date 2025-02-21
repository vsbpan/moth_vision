# moth vision

## Table of Contents

1.  [Overview](#Overview)
2.  [Installation](#Installation)
3.  [Description](#Description)
    -   [Code libraries](#CodeLibraries)
    -   [File structure](#FileStructure)

## Overview <a name="Overview"></a>

This repo hosts code for misc computer vision stuff for Eric's moth collections

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
# see `detectron2/requirements.txt`
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

The custom code written for this project are bundled as a pseudo simulated package *mothr*. It can be imported using `vmisc::load_all2("mothr")`, or `pkgload::load_all("mothr")` to enter developer mode. The latter mode allows newly recompiled C++ code to be included in the package, but copies the .dll file for each time it is initiated, which can cause memory overflow, especially in multi-session parallel computing

##### Structure

-   `mothr/` The root directory of the *mothr* package. `.R` files in this directory are not loaded in the package.
    -   `mothr/R/` Contains the R source code
    -   `mothr/src/` Contains the C++ source code
    -   `DESCRIPTION` Package description file
    -   `NAMESPACE` Namespace file

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
    -   `instance`: One instance of object detection composed of a list of annotation geometries, instance_id, image_id, score, and thing_class. Has generic methods such as `print()`, `plot()`.
    -   `inlist`: A list of `instance` objects associated with an image. Has generic methods such as `print()`, `[]`, `c()`, `plot()`.
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
