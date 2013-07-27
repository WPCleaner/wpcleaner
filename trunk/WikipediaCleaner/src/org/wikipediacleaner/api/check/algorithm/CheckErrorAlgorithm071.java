/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 71 of check wikipedia project.
 * Error 71: ISBN wrong position of X
 */
public class CheckErrorAlgorithm071 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm071() {
    super("ISBN wrong position of X");
  }
}
