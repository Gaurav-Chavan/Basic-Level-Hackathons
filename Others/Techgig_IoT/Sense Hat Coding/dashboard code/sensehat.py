from sense_hat import SenseHat
import time
import requests
import subprocess

sense = SenseHat()

def Readings():
    cpu_temp = subprocess.check_output("vcgencmd measure_temp", shell=True)
    array = cpu_temp.split("=")
    array2 = array[1].split("'")
    cpu_temp= float(array2[0])
    print ("CPU_TEMPERATURE: %.2f" %cpu_temp)
    
    temp = round(sense.get_temperature(),2)
    temp_calibrated = round(temp - ((cpu_temp - temp)/5.466),2)
    a=str(temp_calibrated)
    print("Temperature: %s %%C" % a)
    # print temp_calibrated
    
    humidity = str(round(sense.get_humidity(),2))
    print("Humidity: %s %%rH" % humidity)
    pressure = str(round(sense.get_pressure(),2))
    print("Pressure: %s Millibars" % pressure)
    url='https://dweet.io/dweet/for/htp1991?' + 'temp='+ a + '&humidity=' + humidity +'&presure=' + pressure
    r=requests.post(url)
while True:
    
    Readings()
    time.sleep(10)
    print("readongs recorded")
