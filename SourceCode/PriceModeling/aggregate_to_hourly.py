import sys, getopt, time, datetime

from sets import Set
from os import listdir
from os.path import isfile,join
from collections import defaultdict
from dateutil.parser import parse

def hour_to_timestamp(hour):
    timestamp = datetime.datetime.fromtimestamp(hour * 3600)
    return timestamp

def convert_to_hourly_price(input_file):
    hourly_price = defaultdict(lambda:0.0, {})
    previous_record = defaultdict(lambda:-1, {})
    for line in reversed(open(input_file).readlines()):  # reading in the reverse order (the oldest one first
        price_time = line.rstrip().split()
        if len(price_time) < 2:
            continue
        new_price = float(price_time[0])
        current_second = int(time.mktime(parse(price_time[1]).timetuple()))
        current_hour = (current_second / 3600)
        if (previous_record['seconds'] >= 0):
            if (previous_record['hour'] == current_hour): # no hour difference simply add the price
                hourly_price[current_hour] += ((current_second - previous_record['seconds']) / 3600.0 * previous_record['price'])
            elif (previous_record['hour'] < current_hour):
                processed_second = previous_record['seconds']
                for h in range(previous_record['hour'], current_hour):
                    hourly_price[h] += (((h+1)*3600 - processed_second)/3600.0) * previous_record['price']
                    processed_second = (h+1)*3600
                hourly_price[current_hour] += ((current_second - processed_second)/3600.0 * previous_record['price'])
            else:
                raise Exception("invalid hour entered old:%d new:%d",(previous_record['hour'], current_hour))

        previous_record['seconds'] = current_second
        previous_record['hour'] = current_hour
        previous_record['price'] = new_price

    return hourly_price


def read_files_in_directory(in_dir, out_dir):
    for f in listdir(in_dir):
        if isfile(join(in_dir, f)):
            convert_to_hourly_price(join(in_dir, f), join(out_dir, f))

def get_all_filenames(directories):
    filenames = []
    for directory in directories:
        for f in listdir(directory):
            if isfile(join(directory, f)):
                filenames.append(f)
    return Set(filenames)

def merge_per_file(directories, filename):
    merged_hourly_record = {}
    for directory in directories:
        for f in listdir(directory):
            if isfile(join(directory, f)) and filename==f:
                merged_hourly_record = merge_two_dicts(convert_to_hourly_price(join(directory, f)), merged_hourly_record)
    return merged_hourly_record

def write_output_to_file(output_file_name, hourly_price):
    outcome = open(output_file_name, "w")
    for k in sorted(hourly_price):
        outcome.write(str(k) + " " + str(hourly_price[k]) +"\n")
    outcome.close()

def merge_two_dicts(x, y):
    """Given two dicts, merge them into a new dict as a shallow copy."""
    z = x.copy()
    z.update(y)
    return z

dir_prefix = "aws-ec2-spot-price/"
dir_prostfix = "_all_instances"
directories = [dir_prefix+"20160530"+dir_prostfix, dir_prefix+"20160801"+dir_prostfix,dir_prefix+"20161031"+dir_prostfix,dir_prefix+"20170204"+dir_prostfix,dir_prefix+"20170504"+dir_prostfix,dir_prefix+"20170921"+dir_prostfix,dir_prefix+"20171222"+dir_prostfix]

filenames = get_all_filenames(directories)

output_prefix = dir_prefix + "merged_output/"

for instance in filenames:
    output_record = merge_per_file(directories, instance)
    output_file_name = output_prefix + instance
    write_output_to_file(output_file_name, output_record)
