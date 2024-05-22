#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 27 14:27:44 2024

@author: ruby

Overwriting .info files 

SAMPLE-011
SAMPLE-092 (which is version 2 of SAMPLE 9)
"""

import os


# Define the filenames
file1_name = "SAMPLE-001-T0.info"
file2_name = "SAMPLE-005-T0.info"

# Construct the full file paths
file1_path = "/Volumes/T7 Shield/T0_Registered/SAMPLE-001-T0/001-T0.info"
file2_path = "/Volumes/T7 Shield/T0_Registered/SAMPLE-005-T0/005-T0.info"

# Open the first file for reading
with open(file1_path, "r") as file1:
    #test = file1.readlines()[5][16:26]
    #print(test)
    lines_file1 = file1.readlines()  # Read all lines from the file

# Open the second file for reading and writing
with open(file2_path, "r+") as file2:
    lines_file2 = file2.readlines()  # Read all lines from the file

    # Copy specific areas from file1 to file2
    for i in range(5, 610):  # Loop through lines 5 to 625
        # Extract the specific portion from file1 and overwrite corresponding lines in file2
        lines_file2[i] = lines_file2[i][:16] + lines_file1[i][17:26] + lines_file2[i][26:]

    # Move cursor to the beginning of the file
    file2.seek(0)
    # Write the modified lines back to file2
    file2.writelines(lines_file2)
    # Truncate any remaining content if the new content is shorter than the original
    file2.truncate()