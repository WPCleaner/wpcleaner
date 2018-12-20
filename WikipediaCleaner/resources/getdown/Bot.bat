@ECHO OFF
REM DO NOT MODIFY THIS SCRIPT, ANY MODIFICATION WILL BE REVERTED BY GETDOWN
REM
REM Script to run WPCleaner in bot mode.
REM 
REM You need to execute this script with parameters that will be passed to WPCleaner.
REM 
REM ===== CONNECTION =====
REM The first part of the parameters consists in connection information.
REM You can use the following syntax:
REM Bot.bat en <username> <password> ...
REM 
REM Or with a credentials.txt file containing the following 2 lines :
REM   user=<username>
REM   password=<password>
REM Then you can use the following syntax:
REM Bot.bat -credentials credentials.txt en ...
REM
REM ===== TASKS =====
REM The second part of the parameters consists in the tasks to ask to the bot.
REM For example, if you want to update disambiguation warnings, you can use the following syntax:
REM Bot.bat ... UpdateDabWarnings
REM Or if you want to execute a set of tasks described in a task file, you can use the following syntax:
REM Bot.bat ... DoTasks <task file>
REM
REM ===== NOTE =====
REM If you want to pass extra arguments to the JVM, like increasing the memory available to Java,
REM you can create an extra.txt file in the same folder with one parameter per line.
REM For example, to allow 8G of RAM, the line will be: -Xmx=8192M

java -jar libs/getdown-launcher.jar . bot %*
