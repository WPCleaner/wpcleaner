# !/bin/bash
#
# DO NOT MODIFY THIS SCRIPT, ANY MODIFICATION WILL BE REVERTED BY GETDOWN
#
# Script to run WPCleaner in bot mode.
# 
# You need to execute this script with parameters that will be passed to WPCleaner.
# 
# ===== CONNECTION =====
# The first part of the parameters consists in connection information.
# You can use the following syntax:
# Bot.sh en <username> <password> ...
# 
# Or with a credentials.txt file containing the following 2 lines :
#   user=<username>
#   password=<password>
# Then you can use the following syntax:
# Bot.sh -credentials credentials.txt en ...
#
# ===== TASKS =====
# The second part of the parameters consists in the tasks to ask to the bot.
# For example, if you want to update disambiguation warnings, you can use the following syntax:
# Bot.sh ... UpdateDabWarnings
# Or if you want to execute a set of tasks described in a task file, you can use the following syntax:
# Bot.sh ... DoTasks <task file>
#
# ===== NOTE =====
# If you want to pass extra arguments to the JVM, like increasing the memory available to Java,
# you can create an extra.txt file in the same folder with one parameter per line.
# For example, to allow 8G of RAM, the line will be: -Xmx=8192M

java -jar libs/getdown-launcher.jar . bot "$@"
