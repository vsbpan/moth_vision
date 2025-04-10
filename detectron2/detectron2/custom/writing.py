import csv
from datetime import datetime
import cv2
from detectron2.custom.misc import *
from detectron2.custom.parse_ruler_tags import *

# Function for writing csv
def write_csv(path, df):
    keys = df[0].keys()
    with open(path, "w", newline = "") as output_file: 
        dict_writer = csv.DictWriter(output_file, keys)
        dict_writer.writeheader()
        dict_writer.writerows(df)

def mask2polygon(mask, epsilon_wt = 0.01): 
  cont, _ = cv2.findContours(mask.astype('uint8'),cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE) 
  # format contour list as COCO
  polygon = []
  for contour in cont:
      epsilon = epsilon_wt * cv2.arcLength(contour, True)  # You can adjust epsilon based on your requirements
      approx = cv2.approxPolyDP(contour, epsilon, True)
      contour = np.flip(approx, axis=1)
      segmentation = contour.ravel().tolist()
      polygon.append(segmentation)
                    
  return polygon

def gather_instance_pred3(fi, instances, meta): 
    dataset = []

    if(len(instances) == 0): 
        data = {}
        data["file_name"] = fi
        data["thing_class"] = "NA"
        data["score"] = "NA"
        data["bbox"] = "NA"
        dataset.append(data)
    
    pred_classes = instances.pred_classes.tolist()
    classes_label = MetadataCatalog.get(meta).get("thing_classes")
    pred_class_score = instances.scores.tolist()
    pred_bbox = instances.pred_boxes.tensor.tolist()
    
    for i in range(len(instances)):
        data = {}
        data["file_name"] = fi
        data["thing_class"] = classes_label[pred_classes[i]]
        data["score"] = pred_class_score[i]
        data["bbox"] = pred_bbox[i]
        dataset.append(data)
        
    return dataset

def gather_image_meta2(fi, instances, inference_info): 
    data = {}
    data["file_name"] = fi
    data["image_size"] = list(instances.image_size)
    data["inference_info"] = inference_info
    return data

def gather_instance_pred2(fi, instances, meta): 
    dataset = []

    if(len(instances) == 0): 
        data = {}
        data["file_name"] = fi
        data["thing_class"] = "NA"
        data["score"] = "NA"
        data["keypoints"] = "NA"
        data["bbox"] = "NA"
        data["polygon"] = "NA"
        dataset.append(data)
    
    pred_classes = instances.pred_classes.tolist()
    classes_label = MetadataCatalog.get(meta).get("thing_classes")
    pred_class_score = instances.scores.tolist()
    pred_keypoints = instances.pred_keypoints.tolist()
    pred_bbox = instances.pred_boxes.tensor.tolist()
    pred_masks = instances.pred_masks.numpy()
    
    for i in range(len(instances)):
        data = {}
        data["file_name"] = fi
        data["thing_class"] = classes_label[pred_classes[i]]
        data["score"] = pred_class_score[i]
        data["keypoints"] = np.array(pred_keypoints[i]).ravel().tolist()
        data["bbox"] = pred_bbox[i]
        data["polygon"] = mask2polygon(pred_masks[i], 0.0001)
        dataset.append(data)
        
    return dataset

def gather_image_meta1(fi, instances, inference_info, im, meta): 
    data = {}
    data["file_name"] = fi
    data["image_size"] = list(instances.image_size)
    data["tick_size"] = ruler_tick(instances, im,  meta)
    text_generic, text_tag_id_m, text_tag_id_h  = read_img_tags(instances, im,  meta)
    data["tag_text"] = text_generic
    data["tag_id_guess_m"] = text_tag_id_m
    data["tag_id_guess_h"] = text_tag_id_h
    data["inference_info"] = inference_info
    return data

def gather_instance_pred1(fi, instances, meta): 
    dataset = []
    pred_classes = instances.pred_classes.tolist()
    classes_label = MetadataCatalog.get(meta).get("thing_classes")
    pred_class_score = instances.scores.tolist()
    pred_bbox = instances.pred_boxes.tensor.tolist()
    pred_masks = instances.pred_masks.numpy()
    
    for i in range(len(instances)):
        data = {}
        data["file_name"] = fi
        data["thing_class"] = classes_label[pred_classes[i]] 
        data["score"] = pred_class_score[i]
        data["bbox"] = pred_bbox[i]
        data["polygon"] = mask2polygon(pred_masks[i], 0.001)
        dataset.append(data)
        
    return dataset

# For performing model inference on all images in a subdirectory 
def img_inference(predictor, root_path, inference_info, meta, max_detection, mode):
    meta_data_out = []
    instance_data_out = []
    f = glob.glob(os.path.join(root_path,"*.JPG"))
    tot = len(f)
    for i in range(tot):
        print(f"{i+1} of {tot}", end = "\r")
        fi = f[i]
        im = cv2.imread(fi)
        inst = predictor(im)
        # [1, 2, 1, 1, 4] max detection: ['body', 'hindwing', 'color_checker', 'ruler', 'tag']
        # [2] Max detection [forewing]
        inst = filter_instance_by_classes(inst, meta, max_detection) 
        if mode == "mask": 
            meta_data = gather_image_meta1(fi, inst, inference_info, im, meta)
            instance_data = gather_instance_pred1(fi, inst, meta)
        elif mode == "keypoint": 
            meta_data = gather_image_meta2(fi, inst, inference_info)
            instance_data = gather_instance_pred2(fi, inst, meta)
        elif mode == "bbox":
            meta_data = gather_image_meta2(fi, inst, inference_info)
            instance_data = gather_instance_pred3(fi, inst, meta)
        else: 
            raise Exception("Invalid value provided for 'mode'. Must be 'mask', 'keypoint', or 'bbox'." ) 
        meta_data_out.append(meta_data)
        instance_data_out = instance_data_out + instance_data
    
    return meta_data_out, instance_data_out


# Wrapper for writing model inference on all images in a subdirectory of the root directory 
def write_img_inference(predictor, write_path, name, read_path, model_ver, meta, max_detection, mode):
    now = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    inference_info = model_ver + "__" + now
    df_meta, df_instance = img_inference(predictor, read_path, inference_info, meta, max_detection, mode)
    write_csv(os.path.join(write_path, name + "_inference_" + mode + ".csv"), df_instance)
    write_csv(os.path.join(write_path, name + "_image_meta_" + mode + ".csv"), df_meta)
