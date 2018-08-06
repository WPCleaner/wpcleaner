# !/bin/bash
#
# DO NOT MODIFY THIS SCRIPT, ANY MODIFICATION WILL BE REVERTED BY GETDOWN
#
# Script to run WPCleaner in graphical mode.
# 
# You can execute this script with optional parameters that will be passed to WPCleaner.
# 
# ===== CONNECTION =====
# For example, if you want to automatically login to English wikipedia, you can use the following syntax:
# WPCleaner.sh en <username> <password>
# 
# Or with a credentials.txt file containing the following 2 lines :
#   user=<username>
#   password=<password>
# Then you can use the following syntax:
# WPCleaner.sh -credentials credentials.txt en
#
# ===== NOTE =====
# If you want to pass extra arguments to the JVM, like increasing the memory available to Java,
# you can create an extra.txt file in the same folder with one parameter per line.
# For example, to allow 8G of RAM, the line will be: -Xmx=8192M

java -jar getdown.jar . client "$@"
