/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check;


/**
 * Bean for holding information about problems detected by Check Wiki.
 */
public class CheckWikiDetection {

  /**
   * Error number (algorithm).
   */
  private final int errorNumber;

  /**
   * Location of the error in the wiki text.
   */
  private final int location;

  /**
   * Wiki text where the error was found.
   */
  private final String detection;

  /**
   * @param errorNumber Error number (algorithm).
   * @param location Location of the error.
   * @param detection Text near the error.
   */
  CheckWikiDetection(int errorNumber, int location, String detection) {
    this.errorNumber = errorNumber;
    this.location = location;
    this.detection = detection;
  }

  /**
   * @return Error number (algorithm).
   */
  public int getErrorNumber() {
    return errorNumber;
  }

  /**
   * @return Location of the error.
   */
  public int getLocation() {
    return location;
  }

  /**
   * @return Text near the error.
   */
  public String getDetection() {
    return detection;
  }
}
