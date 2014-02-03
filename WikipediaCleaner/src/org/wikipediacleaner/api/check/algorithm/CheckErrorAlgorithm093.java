/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 93 of check wikipedia project.
 * Error 93: External link with double http://
 */
public class CheckErrorAlgorithm093 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm093() {
    super("External link with double http://");
  }
}
