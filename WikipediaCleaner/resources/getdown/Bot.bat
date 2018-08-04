@ECHO OFF
REM DO NOT MODIFY THIS SCRIPT, ANY MODIFICATION WILL BE REVERTED BY GETDOWN
REM
REM Script to run WPCleaner in bot mode.
REM 
REM You need to execute this script with parameters that will be passed to WPCleaner.
REM 
REM ===== AUTHENTICATION =====
REM The first part of the parameters consists in authentication information.
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

java -jar getdown.jar . bot %*
