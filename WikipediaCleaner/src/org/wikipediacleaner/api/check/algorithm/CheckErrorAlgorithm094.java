/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 94 of check wikipedia project.
 * Error 94: Reference tags with no correct match
 */
public class CheckErrorAlgorithm094 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm094() {
    super("Reference tags with no correct match");
  }
}
