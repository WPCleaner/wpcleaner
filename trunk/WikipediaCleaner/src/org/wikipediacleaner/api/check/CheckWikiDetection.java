/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check;

import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;


/**
 * Bean for holding information about problems detected by Check Wiki.
 */
public class CheckWikiDetection {

  /**
   * Method to interpret the location.
   */
  private final char locationMethod;

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
   * @param locationMethod Method to interprete the location.
   * @param errorNumber Error number (algorithm).
   * @param location Location of the error.
   * @param detection Text near the error.
   */
  CheckWikiDetection(
      char locationMethod, int errorNumber,
      int location, String detection) {
    this.locationMethod = locationMethod;
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
  public int getLocation(PageAnalysis analysis) {
    if (location < 0) {
      return location;
    }
    if (locationMethod == '+') {
      int currentIndex = 0;
      int currentCount = 0;
      String contents = analysis.getContents();
      while ((currentIndex < contents.length()) &&
             (currentCount < location)) {
        boolean moved = false;
        if (!moved) {
          PageElementComment comment = analysis.isInComment(currentIndex);
          if ((comment != null) && (comment.getBeginIndex() == currentIndex)) {
            moved = true;
            currentIndex = comment.getEndIndex();
          }
        }
        // TODO: ignore other things
        if (!moved) {
          currentIndex++;
          currentCount++;
        }
      }
      return currentIndex;
    }
    return location;
  }

  /**
   * @return Text near the error.
   */
  public String getDetection() {
    return detection;
  }
}
