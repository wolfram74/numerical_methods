'''
templater.py yyyy_mm_dd num
problem template
  location
  description section
  steps
    expression
    explanation
'''
import sys
import os
import datetime

date_str = sys.argv[-2]
date_list = date_str.split('_')
print(date_list)
date_ints = [int(val) for val in date_list]
start_date = datetime.date(
  year=int(date_ints[0]),
  month=int(date_ints[1]),
  day=int(date_ints[2]))
one_week = datetime.timedelta(7)
weeks_to_gen = int(sys.argv[-1])

def week_gen(monday_date):
  folder = "week_%s" % monday_date
  os.mkdir(folder)
  view = open(folder+'/notes.txt', 'w')
  view.close()

for loop in range(weeks_to_gen):
  date = start_date+loop*one_week
  file_date = "%04d_%02d_%02d" % (date.year, date.month, date.day)
  print(file_date)
  week_gen(file_date)
