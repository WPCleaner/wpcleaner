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
   * Global flag for using high precision.
   */
  private static boolean highPrecision = false;

  /**
   * Method name.
   */
  private final String method;

  /**
   * Unit of time.
   */
  private final String unit;

  /**
   * Initial time.
   */
  private long initialTime;

  /**
   * Last time.
   */
  private long lastTime;

  /**
   * Threshold for printing duration.
   */
  private long threshold;

  /**
   * @param method Method name.
   */
  public Performance(String method) {
    this.method = method;
    this.unit = highPrecision ? "ns" : "ms";
    initialTime = currentTime();
    lastTime = initialTime;
    threshold = 0;
  }

  /**
   * @param threshold Threshold for printing duration.
   */
  public void setThreshold(long threshold) {
    this.threshold = threshold;
  }

  /**
   * Print a message.
   * 
   * @param message Message to be printed.
   */
  private void printMessage(String message) {
    if (print) {
      output.print(method);
      if (message != null) {
        output.print(": ");
        output.print(message);
      }
      output.println();
      output.flush();
    }
  }

  /**
   * Print a message.
   * 
   * @param time Current time.
   * @param message Message to be printed.
   */
  @SuppressWarnings("unused")
  private void printTimedMessage(long time, String message) {
    if (print) {
      output.print(time);
      output.print(" - ");
      output.print(method);
      if (message != null) {
        output.print(": ");
        output.print(message);
      }
      output.println();
      output.flush();
    }
  }

  /**
   * Print a start message.
   */
  public void printStart() {
    initialTime = currentTime();
    lastTime = initialTime;
    printMessage(null);
  }

  /**
   * Print a step message.
   * 
   * @param message Message to be printed.
   */
  public void printStep(String message) {
    long time = currentTime();
    printMessage("(" + (time - lastTime) + unit + ") " + message);
    lastTime = time;
  }

  /**
   * Print an end message.
   */
  public void printEnd() {
    long time = currentTime();
    if (time > initialTime + threshold) {
      printMessage("(" + (time - initialTime) + unit + ")");
    }
  }

  /**
   * @return Current time.
   */
  private static long currentTime() {
    if (highPrecision) {
      return System.nanoTime();
    }
    return System.currentTimeMillis();
  }
}
