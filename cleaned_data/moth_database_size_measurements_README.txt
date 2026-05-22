id: image_id
file_name: file name in the moth database
path: image path on the LoPresti lab computer.
height: image pixel height
width: image pixel width
num_annotations: number of detected objects in the image
version: computer vision model number
inf_time: when was the computer vision pipeline run? moth/day/year hour:minute:second AM/PM
tag_id: tag_id that has been validated and matched to the google spreadsheet entires
tick_size: ruler tick size in pixels / mm
wing_length: wing length in mm (*: body, forewing, hindwing). If there are two body parts (i.e., left, right), then the maximum of the two is returned.
*_area: area of body part in mm2 (*: body, forewing, hindwing). If there are two body parts (i.e., left, right), then the maximum of the two is returned.
date: the date (of collection) column in the metadata spreadsheet. year-month-day 
MONA: MONA number
location: trap location or where the WPC specimen was collected
month: month of collection
year: year of collection
doy: day of year of collection
historic: TRUE or FALSE, the specimen is historic (as opposed to modern)
Genus_Species: binomial taxon name
Superfamily: superfamily taxon
Family: family taxon
Genus: genus taxon


