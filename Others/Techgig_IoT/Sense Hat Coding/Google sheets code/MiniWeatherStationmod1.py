#!/usr/bin/python

import json
import sys
import time
import datetime
import subprocess

# libraries
import sys #to interact with interpreter we use sys
import urllib2 #fetch url / perform URL actions(authentication
import json
import gspread #google spreadsheet package
from oauth2client.client import SignedJwtAssertionCredentials
from sense_hat import SenseHat

# Oauth JSON File
GDOCS_OAUTH_JSON       = 'MyProject2-ae6c4ba70f7b.json'

# Google Docs spreadsheet name.
GDOCS_SPREADSHEET_NAME = 'Readings1'

# How long to wait (in seconds) between measurements.
FREQUENCY_SECONDS= 30


def login_open_sheet(oauth_key_file, spreadsheet): #function to login to sheets
        """Connect to Google Docs spreadsheet and return the first worksheet."""
        try:
            json_key = json.load(open(oauth_key_file)) #Json File load
            credentials = SignedJwtAssertionCredentials(json_key['client_email'],json_key['private_key'],['https://spreadsheets.google.com/feeds']) #pass values of json file to credentials using SignedJWTAssertioncredentials
            gc = gspread.authorize(credentials) #login to google api using oauth2 credentials
            worksheet = gc.open(spreadsheet).sheet1 #open a spread sheet and parse it to worksheet
            return worksheet
        except Exception as ex:
            print 'Unable to login and get spreadsheet.  Check OAuth credentials, spreadsheet name, and make sure spreadsheet is shared to the client_email address in the OAuth .json file!'
            print 'Google sheet login failed with error:', ex
            sys.exit(1)


sense = SenseHat()
sense.clear()           #clears 8*8 LED matrix display
print 'Logging sensor measurements to {0} every {1} seconds.'.format(GDOCS_SPREADSHEET_NAME, FREQUENCY_SECONDS)
print 'Press Ctrl-C to quit.'
worksheet = None
while True:
        # Login if necessary.
        if worksheet is None:
           worksheet = login_open_sheet(GDOCS_OAUTH_JSON, GDOCS_SPREADSHEET_NAME) #function call login_open_sheet passing json file as well as spreadsheet name

        # Attempt to get sensor reading.
        cpu_temp = subprocess.check_output("vcgencmd measure_temp", shell=True)
        array = cpu_temp.split("=")
        array2 = array[1].split("'")
        cpu_temp = float(array2[0])
        cpu_temp = round(cpu_temp, 1)
        
        temp = sense.get_temperature()
        temp = round(temp, 1)
        
        humidity = sense.get_humidity()
        humidity = round(humidity, 1)
        
        pressure = sense.get_pressure()
        pressure = round(pressure, 1)
        
        # 8x8 RGB


        #sense.clear()
        #info = 'Temperature (C): ' + str(temp) + 'Humidity: ' + str(humidity) + 'Pressure: ' + str(pressure)
        #sense.show_message(info, text_colour=[255, 0, 0]) #red color
        
        # Print on python console
         # print "Temperature (C): ", temp
         # print "Humidity: ", humidity
         # print "Pressure: ", pressure, "\n"

        # Append the data in the spreadsheet, including a timestamp
        try:
           worksheet.append_row((datetime.datetime.now(), cpu_temp,temp,humidity,pressure))
        except:
        # Error appending data, most likely because credentials are stale(change in key of JSON file).
        # Null out the worksheet so a login is performed at the top of the loop.
           print 'Append error, logging in again'
           worksheet = None
           time.sleep(FREQUENCY_SECONDS)
           continue

        # Wait 30 seconds before continuing
        print 'Wrote a row to {0}'.format(GDOCS_SPREADSHEET_NAME)
        time.sleep(FREQUENCY_SECONDS)
