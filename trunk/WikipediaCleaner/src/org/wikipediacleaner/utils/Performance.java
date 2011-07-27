/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2011  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.utils;

import java.io.PrintStream;


/**
 * Utility class to measure performance.
 */
public class Performance {

  /**
   * Stream to write performance information to.
   */
  private final static PrintStream output = System.err;

  /**
   * Global flag for printing.
   */
  private static boolean print = true;

  /**
   * Print a message.
   * 
   * @param message Message to be printed.
   */
  public static void printTimedMessage(String message) {
    if (print) {
      output.println("" + currentTime() + ": " + message);
    }
  }

  /**
   * Print a message.
   * 
   * @param message Message to be printed. 
   * @return Time of the message.
   */
  public static long printStartTimedMessage(String message) {
    long time = currentTime();
    if (print) {
      output.println("" + time + ": " + message);
    }
    return time;
  }

  /**
   * Print a message.
   * 
   * @param start Start time.
   * @param message Message to be printed.
   * @return Time of the message.
   */
  public static long printStopTimedMessage(long start, String message) {
    long time = currentTime();
    if (print) {
      output.println("" + time + ": (" + (time - start) + ") " + message);
    }
    return time;
  }

  /**
   * @return Current time.
   */
  private static long currentTime() {
    return System.currentTimeMillis() / 1;
  }
}
