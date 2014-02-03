/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 96 of check wikipedia project.
 * Error 96: TOC after first headline
 */
public class CheckErrorAlgorithm096 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm096() {
    super("TOC after first headline");
  }
}
