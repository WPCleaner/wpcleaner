/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 62 of check wikipedia project.
 * Error 62: URL containing no http://
 */
public class CheckErrorAlgorithm062 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm062() {
    super("URL containing no http://");
  }
}
