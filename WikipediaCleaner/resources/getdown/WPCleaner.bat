@ECHO OFF
REM DO NOT MODIFY THIS SCRIPT, ANY MODIFICATION WILL BE REVERTED BY GETDOWN
REM
REM Script to run WPCleaner in graphical mode.
REM 
REM You can execute this script with optional parameters that will be passed to WPCleaner.
REM 
REM ===== CONNECTION =====
REM For example, if you want to automatically login to English wikipedia, you can use the following syntax:
REM WPCleaner.bat en <username> <password>
REM 
REM Or with a credentials.txt file containing the following 2 lines :
REM   user=<username>
REM   password=<password>
REM Then you can use the following syntax:
REM WPCleaner.bat -credentials credentials.txt en
REM
REM ===== NOTE =====
REM If you want to pass extra arguments to the JVM, like increasing the memory available to Java,
REM you can create an extra.txt file in the same folder with one parameter per line.
REM For example, to allow 8G of RAM, the line will be: -Xmx=8192M

IF EXIST "credentials.txt" (
  java -jar libs/getdown-launcher.jar . client -credentials credentials.txt %*
) ELSE (
  java -jar libs/getdown-launcher.jar . client %*
)
