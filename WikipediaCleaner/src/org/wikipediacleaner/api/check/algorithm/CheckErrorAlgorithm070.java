/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 70 of check wikipedia project.
 * Error 70: ISBN wrong length
 */
public class CheckErrorAlgorithm070 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm070() {
    super("ISBN wrong length");
  }
}
