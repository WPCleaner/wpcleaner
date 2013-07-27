/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 1 of check wikipedia project.
 * Error 1: No bold title
 */
public class CheckErrorAlgorithm001 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm001() {
    super("No bold title");
  }
}
