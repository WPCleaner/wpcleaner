/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 4 of check wikipedia project.
 * Error 4: Article with weblink
 */
public class CheckErrorAlgorithm004 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm004() {
    super("Article with weblink");
  }
}
