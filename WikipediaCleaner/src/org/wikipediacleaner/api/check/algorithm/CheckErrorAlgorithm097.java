/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 97 of check wikipedia project.
 * Error 97: Material between TOC and first headline
 */
public class CheckErrorAlgorithm097 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm097() {
    super("Material between TOC and first headline");
  }
}
