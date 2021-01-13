/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Utility class to measure performance.
 */
public class Performance {

  /** Logger */
  private final static Logger log = LoggerFactory.getLogger("PERF");

  /** Global flag for printing */
  private final static boolean print = true;

  /** Global flag for using high precision */
  private final static boolean highPrecision = false;

  /** Method name */
  private String method;

  /** Unit of time */
  private String unit;

  /** Initial time */
  private long initialTime;

  /** Last time */
  private long lastTime;

  /** Threshold for printing duration */
  private long threshold;

  /** Map to measure parts of the execution */
  private Map<String, Long> parts;

  /** Initial time for a part of the execution */
  private long partInitialTime;

  /**
   * @param method Method name.
   */
  private Performance(String method) {
    this(method, 0);
  }

  /**
   * @param method Method name.
   * @param threshold Threshold for printing results.
   */
  private Performance(String method, long threshold) {
    initialize(method, threshold);
  }

  /**
   * Initialize a performance instance.
   * 
   * @param methodName Method name.
   * @param thresholdValue Threshold for printing results.
   */
  private void initialize(String methodName, long thresholdValue) {
    this.method = methodName;
    this.unit = highPrecision ? "ns" : "ms";
    this.threshold = thresholdValue;
    initialTime = currentTime();
    partInitialTime = initialTime;
    lastTime = initialTime;
  }

  /** Available instances to measure performance. */
  private final static List<Performance> availablePerformances = new LinkedList<>();

  /** Instances currently used to measure performance. */
  private final static List<Performance> usedPerformances = new LinkedList<>();

  /**
   * @param method Method name.
   * @return An instance for measuring performance.
   */
  public static Performance getInstance(String method) {
    return getInstance(method, 0);
  }

  /**
   * @param method Method name.
   * @param threshold Threshold for printing results.
   * @return An instance for measuring performance.
   */
  public static Performance getInstance(String method, long threshold) {
    Performance perf = null;

    // Look for already created instances
    synchronized (availablePerformances) {
      if (!availablePerformances.isEmpty()) {
        perf = availablePerformances.remove(0);
        perf.initialize(method, threshold);
      }
    }

    // Create a new instance if needed
    if (perf == null) {
      perf = new Performance(method, threshold);
    }

    // Register instance
    synchronized (usedPerformances) {
      usedPerformances.add(0, perf);
    }

    return perf;
  }

  /**
   * Release a performance instance from the pool of performance objects.
   */
  public void release() {
    synchronized (usedPerformances) {
      usedPerformances.remove(this);
    }
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
      StringBuilder text = new StringBuilder();
      text.append(method);
      if (message != null) {
        text.append(": ");
        text.append(message);
      }
      log.info(text.toString());
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
      StringBuilder text = new StringBuilder();
      text.append(time);
      text.append(" - ");
      text.append(method);
      if (message != null) {
        text.append(": ");
        text.append(message);
      }
      log.info(text.toString());
    }
  }

  /**
   * Print a start message.
   */
  public void printStart() {
    initialTime = currentTime();
    lastTime = initialTime;
    printMessage("Begin");
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
   * Start measuring time for a part of the execution.
   */
  public void startPart() {
    partInitialTime = currentTime();
  }

  /**
   * Stop measuring time for a part of the execution.
   * 
   * @param part Part name.
   */
  public void stopPart(String part) {
    long time = currentTime();
    if (time <= partInitialTime) {
      return;
    }
    if (parts == null) {
      parts = new HashMap<>();
    }
    Long previousTime = parts.get(part);
    if (previousTime != null) {
      parts.put(part, Long.valueOf(time - partInitialTime + previousTime.longValue()));
    } else {
      parts.put(part, Long.valueOf(time - partInitialTime));
    }
    partInitialTime = time;
  }

  /**
   * Print an end message.
   */
  public void printEnd() {
    long time = currentTime();
    if (time > initialTime + threshold) {
      printDetail("End (" + (time - initialTime) + unit + ")");
    }
  }

  /**
   * Always print an end message.
   */
  public void printEndAlways() {
    long time = currentTime();
    printDetail("End (" + (time - initialTime) + unit + ")");
  }

  /**
   * Print an end message.
   * 
   * @param message Message.
   */
  public void printEnd(String message) {
    long time = currentTime();
    if (time > initialTime + threshold) {
      printDetail(message + "(" + (time - initialTime) + unit + ")");
    }
  }

  /**
   * Print an end message.
   * 
   * @param message Message.
   * @param message2 Second part of the message.
   */
  public void printEnd(String message, String message2) {
    long time = currentTime();
    if (time > initialTime + threshold) {
      printDetail(message + "(" + (time - initialTime) + unit + "):" + message2);
    }
  }

  /**
   * Print detail.
   * 
   * @param message Messgae. 
   */
  private void printDetail(String message) {
    printMessage(message);
    if (parts != null) {
      for (Entry<String, Long> entry : parts.entrySet()) {
        printMessage("  " + entry.getKey() + "(" + entry.getValue() + unit + ")");
      }
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
