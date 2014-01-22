/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 89 of check wikipedia project.
 * Error 89: DEFAULTSORT with no space after the comma
 */
public class CheckErrorAlgorithm089 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm089() {
    super("DEFAULTSORT with no space after the comma");
  }
}
