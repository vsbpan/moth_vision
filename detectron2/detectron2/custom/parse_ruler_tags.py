import pytesseract
import cv2
import numpy as np
from detectron2.data import MetadataCatalog, DatasetCatalog

def detect_mask_angle(mask):
    coords = np.column_stack(np.where(mask.numpy()))
    angle = cv2.minAreaRect(coords)[-1]
    return 90 - angle

def detect_text_rotation(im_cropped): 
    erosion_size = 5
    element = cv2.getStructuringElement(cv2.MORPH_RECT, (2 * erosion_size + 1, 2 * erosion_size + 1), (erosion_size, erosion_size) )
    im_cropped = cv2.erode(im_cropped, element)
    coords = np.column_stack(np.where(im_cropped > 0))
    angle = cv2.minAreaRect(coords)[-1]
    # the `cv2.minAreaRect` function returns values in the
    # range [-90, 0); as the rectangle rotates clockwise the
    # returned angle trends to 0 -- in this special case we
    # need to add 90 degrees to the angle
    # print(f"Original angle detected: {angle}")
    if angle < -45:
    	angle = -(90 + angle)
    # otherwise, just take the inverse of the angle to make
    # it positive
    else:
    	angle = -angle
    return angle

def correct_text_rotation(im_cropped, angle):
    (h, w) = im_cropped.shape[:2]
    res = cv2.warpAffine(im_cropped, cv2.getRotationMatrix2D((w // 2, h // 2), angle, 1.0), (w, h), flags=cv2.INTER_CUBIC, borderMode=cv2.BORDER_REPLICATE)
    return res

def select_tags(instances, meta_data):
    things_classes = MetadataCatalog.get(meta_data).get("thing_classes")
    things_predicted = [things_classes[i] for i in instances.pred_classes.tolist()]
    index = [i == 'tag' for i in things_predicted]
    return instances[index]

def get_tags_cropped(instances, image, meta_data): 
    s = select_tags(instances, meta_data)
    bbox = s.pred_boxes.tensor.tolist()
    masks = s.pred_masks
    
    im_cropped = list()
    for i in range(len(bbox)): 
        # Find bbox for instance i
        row_max = round(bbox[i][3])
        row_min = round(bbox[i][1])
        col_max = round(bbox[i][2])
        col_min = round(bbox[i][0])
        
        # Some preprocessing
        crp_imgi = image[row_min:row_max,col_min:col_max,:]
        crp_imgi = cv2.cvtColor(crp_imgi, cv2.COLOR_BGR2GRAY)
        
        # Mask angle
        angle = detect_mask_angle(masks[i])
        h, w = crp_imgi.shape
        crp_imgi = crp_imgi[20:(h - 20),20:(w - 20)]
        
        # Anything more than 25 degrees is probably an error
        if(angle > 0 and angle < 25):
           crp_imgi = correct_text_rotation(crp_imgi, angle)
        
        im_cropped.append(crp_imgi)
    return im_cropped
    
def read_text(im_cropped, tesseract_loc = 'C:/Program Files/Tesseract-OCR/tesseract.exe'): 
    pytesseract.pytesseract.tesseract_cmd = tesseract_loc
    # Correct for rotation using image mask
    #angle = detect_text_rotation(im_cropped)
    #if(angle > 0 and angle < 15):
    #    h, w = im_cropped.shape
    #    im_cropped = correct_text_rotation(im_cropped[20:(h - 20),20:(w - 20)], angle)
    
    generic_config = r'-c tessedit_char_whitelist=ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz'
    tag_id_config = r'-c tessedit_char_whitelist=DCR0123456789'

    thr, threshed = cv2.threshold(im_cropped,0,255,cv2.THRESH_OTSU)
    result = 255 - threshed
    result = cv2.GaussianBlur(result, (5,5), 0)
    
    # Read any generic text
    text = pytesseract.image_to_string(result, config = generic_config, lang = "eng")
    
    # same as above but optimized to read DCR tags
    tag_id_text = pytesseract.image_to_string(result, config = tag_id_config, lang = "eng")
    
    return text, tag_id_text

def read_img_tags(instances, image, meta_data, tesseract_loc = 'C:/Program Files/Tesseract-OCR/tesseract.exe'):
    text_generic = []
    text_tag_id = []
    try: 
      res = get_tags_cropped(instances, image, meta_data)
      for i in range(len(res)):
          try: 
              # Some denoising then sharpening
            generic_text, tag_id_text = read_text(res[i], tesseract_loc)
            text_generic.append(generic_text)
            text_tag_id.append(tag_id_text)
          except: 
            text_generic.append("text_read_failed")
            text_tag_id.append("text_read_failed")
    except: 
      text_generic.append("text_read_failed")
      text_tag_id.append("text_read_failed")
    return text_generic, text_tag_id
  
def select_ruler(instances, meta_data):
    things_classes = MetadataCatalog.get(meta_data).get("thing_classes")
    things_predicted = [things_classes[i] for i in instances.pred_classes.tolist()]
    index = [i == 'ruler' for i in things_predicted]
    return instances[index]

def get_binarized_ruler(instances, image, meta_data): 
    bbox = select_ruler(instances, meta_data).pred_boxes.tensor.tolist()[0]
    row_max = round(bbox[3])
    row_min = round(bbox[1])
    col_max = round(bbox[2])
    col_min = round(bbox[0])
    image = cv2.pyrMeanShiftFiltering(image[row_min:row_max,col_min:col_max,:],20,30) # adds quite a bit of time
    image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    thr,image = cv2.threshold(image,0,255,cv2.THRESH_OTSU)
    return image > thr

def fourier_trick_ruler(ruler):
    # adapted from mothra
    signal_thresholded = np.sum(ruler, axis=0) > 0
    fourier = np.fft.rfft(signal_thresholded)
    mod = np.abs(fourier)
    mod[0:10] = 0  # we discard the first several coeffs
    freq = np.fft.rfftfreq(len(signal_thresholded))
    f_space = freq[np.argmax(mod)]
    T_space = 1 / f_space
    return T_space

def ruler_tick(instances, image, meta_data):
    try: 
        ruler = get_binarized_ruler(instances, image, meta_data)
    except: 
        res = "get_ruler_failed"
        return res
    try: 
        res = fourier_trick_ruler(ruler)
    except: 
        res = "failed" 
    return res
