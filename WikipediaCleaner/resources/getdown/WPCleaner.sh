#!/usr/bin/env sh
# =============================================================================
# Filename:                WPCleaner.sh
# Description:             POSIX-compliant shell script to launch the WPCleaner
#                          Java application's graphical mode using the default
#                          JRE in *NIX-like environments
# Project:                 WPCleaner <https://w.wiki/8ta>
# Author:                  NicoV <https://w.wiki/8tb>
# SPDX-License-Identifier: Apache-2.0
# Last updated:            2019-09-25
# =============================================================================
# Copyright © 2012-2019 NicoV <https://w.wiki/8tb>.
#           © 2019 Peter J. Mello (RogueScholar) <https://w.wiki/8td>.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at:
#
#    https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.
# =============================================================================
# DO NOT MODIFY THIS SCRIPT, IT IS OVERWRITTEN ON EACH EXECUTION BY GETDOWN
#
# You can execute this script with optional parameters that will be passed to
# WPCleaner. For example, if you want to automatically login to English
# Wikipedia, you can use the following syntax:
#   WPCleaner.sh en <username> <password>
#
# Or with a credentials.txt file containing these 2 lines,
#   user=<username>
#   password=<password>
# you can use the following syntax to login automatically:
#   WPCleaner.sh -credentials credentials.txt en
#
# If you want to pass extra arguments to the JVM, like increasing the memory
# available to Java, create a file named 'extra.txt' in the same directory as
# this script, with read permissions for any user which might invoke it, with
# one parameter per line. For example, to allow 8G of RAM, the line would read:
#   -Xmx=8192M

JAVA_APP_DIR="$(cd "$(dirname "$0")"; pwd -P)"
JAVA_LIB_DIR="${JAVA_APP_DIR}/libs"
cd "$JAVA_APP_DIR" || ( echo "Unable to open install directory." >&2; exit 1; )

JAVA_PARAMS="-jar ${JAVA_LIB_DIR}/getdown-launcher.jar . client"

if [ -f credentials.txt ]; then
  JAVA_PARAMS="${JAVA_PARAMS} -credentials credentials.txt"
fi

case $# in
  0) java ${JAVA_PARAMS}
  ;;
  *) java ${JAVA_PARAMS} $@
  ;;
esac
