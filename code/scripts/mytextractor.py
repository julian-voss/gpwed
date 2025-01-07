import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import random
import sys
from tqdm import tqdm
import time
import re

from statistics import mean, median
from PIL import Image
from textractor import Textractor
from textractor.entities.word import Word
from textractor.entities.bbox import BoundingBox
from textractor.visualizers.entitylist import EntityList
from textractor.data.constants import TextractFeatures, Direction, DirectionalFinderType


# Define the function to list all files in a directory including its subdirectories
def list_all_files(directory):
    all_files = []  # List to store all file paths

    # Walk through all directories and files in the directory
    for root, dirs, files in os.walk(directory):
        for file in files:
            # Concatenate the directory and file name to form the full path
            file_path = os.path.join(root, file)
            # Add the file path to the list
            all_files.append(file_path)

    return all_files


def unit_vector(vector):
    """ Returns the unit vector of the vector.  """
    return vector / np.linalg.norm(vector)

def angle_between(v1, v2):
    """ Returns the angle in radians between vectors 'v1' and 'v2'::

            >>> angle_between((1, 0, 0), (0, 1, 0))
            1.5707963267948966
            >>> angle_between((1, 0, 0), (1, 0, 0))
            0.0
            >>> angle_between((1, 0, 0), (-1, 0, 0))
            3.141592653589793
    """
    v1_u = unit_vector(v1)
    v2_u = unit_vector(v2)
    return np.arccos(np.clip(np.dot(v1_u, v2_u), -1.0, 1.0))



def calc_rotation_angle(grouped_cols, image, colidx):

    # Write function that calculates rotation angle for row

    grouped_col = grouped_cols[colidx-1]

    try:
        word_upper = grouped_col[0][0]
        word_lower = grouped_col[-1][0]
    except:
        return 0
    
    x1, y1 = word_upper.x*image.width, -word_upper.y*image.height
    
    x2, y2 = word_lower.x*image.width, -word_lower.y*image.height

    angle = angle_between([x1,y1], [x1,y2])

    if x2>x1:
        angle = angle*-1

    return angle

def calc_rotation_angle_mean(grouped_cols, image, statistic = "mean"):

    angles = []
    
    for i in range(0, len(grouped_cols)):

        angle = calc_rotation_angle(grouped_cols, image, i)

        angles.append(angle)
    if statistic == "mean":
        mean_angle = mean(angles)

        return mean_angle
        
    elif statistic == "median":
        median_angle = median(angles)

        return median_angle
        
    else:
        print("No valid statistic defined")






def visualize_rotation(grouped_cols, image, colidx):
    
    grouped_col = grouped_cols[colidx-1]
    
    word_upper = grouped_col[0][0]
    
    word_lower = grouped_col[-1][0]
    
    x1, y1 = word_upper.x*image.width, -word_upper.y*image.height
    
    x2, y2 = word_lower.x*image.width, -word_lower.y*image.height

    plt.plot(np.array([x1, x2]), np.array([y1,y2]), 'ro-')
    plt.plot(np.array([x1, x1]), np.array([y1,y2]), 'ro-')
    
    # Setting axis limits
    plt.xlim([0, image.width]) # Adjust as needed
    plt.ylim([-image.height, 0]) # Adjust as needed
    
    # Setting the aspect ratio
    plt.gca().set_aspect('equal')
    
    plt.show()




def correct_rotation(table, image, statistic = "mean"):
    
    grouped_cols = group_table_cols(image, table)

    rotation_angle = calc_rotation_angle_mean(grouped_cols, image, statistic)

    image_rotated = image.rotate(rotation_angle, expand = True)

    return(image_rotated)





def group_words_by_distance(table, col_id, distance_th, image_height):

    """
    This function takes a table as an input and returns the grouped
    """

    table_data = table.strip_headers()
    
    words = table_data.words
    
    column = [word for word in words if word.col_index == col_id]
    column = sorted(column, key = lambda x: x.y)
    column = EntityList(column)

    distances = np.array([worda.y*image_height - wordb.y*image_height for (worda, wordb) in zip(column, column[1:])])

    distances = np.append([0], distances)

    word_group = (distances<distance_th).cumsum()

    grouped_words = [[] for i in range(max(word_group)+1)]
    for i, word in zip(word_group, column):
        grouped_words[i].append(word) 

    grouped_words = [sorted(words, key = lambda x: x.x) for words in grouped_words] # Sort words in each row horizontally
        
    grouped_words = [EntityList(words) for words in grouped_words] # Turn each row into EntityList 

    return grouped_words




def group_table_cols(image, table, distance_th = -5):

    image_height = image.height

    ncols = table.column_count 

    grouped_cols = [group_words_by_distance(table, colidx, distance_th, image_height) for colidx in range(1, ncols+1)]

    return(grouped_cols)



def merge_cols_first(A, B, image_height, height_th):

    idxA, idxB = 0, 0
    
    result = []

    placeholder = EntityList([Word(entity_id="placeholder", bbox = BoundingBox(0, 0, 0, 0))])

    isemptyA = sum([len(cell) for cell in A]) == 0
    isemptyB = sum([len(cell) for cell in B]) == 0

    if isemptyA: # If A column is empty

        for row in B:

            placeholder_a = EntityList([Word(entity_id="placeholder", bbox = row[0].bbox)])    
            result.append([placeholder_a, row])

        return result
    
    if isemptyB: # If B column is empty

        for row in A:
        
            placeholder_b = EntityList([Word(entity_id="placeholder", bbox = row[0].bbox)])    
            result.append([row, placeholder_b])

        return result
    
    while idxA < len(A) or idxB < len(B):
    
        if idxA  < len(A) and idxB < len(B):
    
            ay = min([word.y for word in A[idxA]]) * image_height
            bres = B[idxB]
            
            by = min([word.y for word in B[idxB]]) * image_height
            ares = A[idxA]
    
            if abs(ay - by) < height_th:
                idxA += 1; idxB += 1
            elif ay < by:
                idxA += 1; bres = placeholder
            else: 
                idxB += 1; ares = placeholder
    
        elif idxA>= len(A) and idxB >= len(B):
    
            break
    
        elif idxA >= len(A):
    
            ares = placeholder
            
            bres = B[idxB]
    
            idxB += 1
    
        elif idxB >= len(B):
    
            bres = placeholder
            ares = A[idxA]
    
            idxA += 1

        result.append([ares, bres])

    return result



def merge_cols_other(A, B, image_height, height_th):

    # Count # of columns in left element
    ncols_a = len(A[0])
    
    # Define row ids
    idxA, idxB = 0, 0
    
    # Empty list to store result in
    result = []

    if len(B) < 2:

        for row in A:
        
            placeholder_bbox = row[-1][0].bbox
    
            placeholder = [EntityList([Word(entity_id="placeholder", bbox = placeholder_bbox)])]

            result.append(row + placeholder)

        return result
    
    # Get image height and define height tresh for cells in the same row
    
    # # Create empty placeholder for new rows in B not already present in A
    # placeholder_a = EntityList([Word(entity_id="placeholder", bbox = BoundingBox(0, 0, 0, 0))])
    # placeholder_a = [placeholder_a for i in range(0, ncols_a)]
    
    # # Create empty placeholder for new rows in A
    # placeholder_b = EntityList([Word(entity_id="placeholder", bbox = BoundingBox(0, 0, 0, 0))])
    
    while idxA < len(A) or idxB < len(B):
    # Loop for as long as not at the bottom of both columns
    
        # Create empty placeholder for new rows in B not already present in A
        placeholder_a = EntityList([Word(entity_id="placeholder", bbox = BoundingBox(0, 0, 0, 0))])
        placeholder_a = [placeholder_a for i in range(0, ncols_a)]
        
        # Create empty placeholder for new rows in A
        if idxA < len(A):
            bbox = A[idxA][-1][-1].bbox # Assign placeholder bounding box from last word in A
            placeholder_b = [EntityList([Word(entity_id="placeholder", bbox = bbox)])]
        else:
            placeholder_b = [EntityList([Word(entity_id="placeholder", bbox = BoundingBox(0, 0, 0, 0))])]
    
        if idxA  < len(A) and idxB < len(B):
    
            ay = min([word.y for word in A[idxA][-1]]) * image_height
            bres = EntityList([B[idxB]])
            
            by = min([word.y for word in B[idxB]]) * image_height
            ares = A[idxA]
    
            if abs(ay - by) < height_th:
                idxA += 1; idxB += 1
            elif ay < by:
                idxA += 1; bres = placeholder_b
            else: 
                idxB += 1; ares = placeholder_a
    
        elif idxA>= len(A) and idxB >= len(B):
    
            break
    
        elif idxA >= len(A):
    
            ares = placeholder_a

            bres = EntityList([B[idxB]])
    
            idxB += 1
    
        elif idxB >= len(B):
    
            bres = placeholder_b
            ares = A[idxA]
    
            idxA += 1
    
        result.append(ares + bres)
    
    return result



def scrape_table(table, image, height_th, distance_th = -5):

    if isinstance(image, str):
        image = Image.open(image)

    height = image.height

    # Group columns
    grouped_cols = group_table_cols(image, table, distance_th = distance_th)

    # Merge columns 
    ncols = len(grouped_cols)
    
    colidx = 1
    
    result = grouped_cols[0]
    
    while colidx < ncols:

        if colidx == 1 or error:

            try:
                result = merge_cols_first(result, grouped_cols[colidx], height, height_th = height_th)
            except:
                error = True
                result = grouped_cols[colidx]
                continue
            else:
                error = False
            
        else:
            
            result = merge_cols_other(result, grouped_cols[colidx], height, height_th = height_th)
        
    
        colidx += 1


    return result



def table_to_df(scraped_table):#
    merged_texts = []
    
    for row in scraped_table:
    
        row_text = []
        
        for cell in row:
    
            cell_text = ' '.join(word.text for word in cell)
            
            row_text.append(cell_text)
    
        merged_texts.append(row_text)

    df = pd.DataFrame(merged_texts)

    return(df)



def batch_process_files(input_files, output_folder, height_th = 10, distance_th = -5, checkrotation = True, rotationstat = "mean", binarize = False, binthresh = 100,
                       crop_image = False, crop_factors = (0, 0, 0, 0)):
    
    extractor = Textractor(profile_name="default")

    error_files = []
    
    for idx in tqdm(range(0, len(input_files))):

        file = input_files[idx]
    
        try:
    
            # Read image file and analyze document
            image = Image.open(file)

            if binarize:

                # Grayscale
                image = image.convert('L')
                # Threshold
                image = image.point( lambda p: 255 if p > binthresh else 0 )
                # To mono
                image = image.convert('1')

            if crop_image:

                # Get the dimensions of the image
                width, height = image.size

                #crop_factor=crop_factor/2
                
                # Calculate the dimensions to crop
                left = width * crop_factors[0]
                upper = height * crop_factors[1]
                right = width * (1-crop_factors[2])
                lower = height * (1-crop_factors[3])
                
                # Define the crop box
                crop_box = (left, upper, right, lower)
                
                # Crop the image
                image = image.crop(crop_box)
            
            
            document = extractor.analyze_document(
                file_source=image,
                features=[TextractFeatures.TABLES],
                save_image=True)

            if len(document.tables) == 0:

                continue

            multitable = len(document.tables) > 1

            tableidx = 1

            for table in document.tables:
        
                if checkrotation:
                    
                    image_rotated = correct_rotation(table, image, rotationstat)
            
                    document = extractor.analyze_document(
                        file_source=image_rotated,
                        features=[TextractFeatures.TABLES],
                        save_image=True)
                
                    table = document.tables[tableidx]
            
                    scraped_table = scrape_table(table, image_rotated, height_th=height_th, distance_th = distance_th)
    
                    
    
                else:
                    
                    scraped_table = scrape_table(table, image, height_th=height_th, distance_th = distance_th)
            
                scraped_table_df = table_to_df(scraped_table)

                if multitable:
                    
                    path_out = output_folder + re.sub(r'\.png|\.jpg', '', os.path.basename(file)) + "_" + str(tableidx) + ".xlsx"

                    tableidx+=1

                else:

                    path_out = output_folder + re.sub(r'\.png|\.jpg', '.xlsx', os.path.basename(file))
            
                scraped_table_df.to_excel(path_out)
    
        except Exception as e: 
            
            print(e)    
            error_files.append(file)

    print("Finished!")

    return error_files
    
