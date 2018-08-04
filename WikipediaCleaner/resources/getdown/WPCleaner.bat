@ECHO OFF
REM DO NOT MODIFY THIS SCRIPT, ANY MODIFICATION WILL BE REVERTED BY GETDOWN
REM
REM Script to run WPCleaner in graphical mode.
REM 
REM You can execute this script with optional parameters that will be passed to WPCleaner.
REM 
REM For example, if you want to automatically login to English wikipedia, you can use the following syntax:
REM WPCleaner.bat en <username> <password>
REM 
REM Or with a credentials.txt file containing the following 2 lines :
REM   user=<username>
REM   password=<password>
REM Then you can use the following syntax:
REM WPCleaner.bat -credentials credentials.txt en

java -jar getdown.jar . client %*
