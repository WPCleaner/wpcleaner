/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;


/**
 * Algorithm for analyzing error 95 of check wikipedia project.
 * Error 95: Editor's signature or link to user space
 */
public class CheckErrorAlgorithm095 extends CheckErrorAlgorithmUnavailable {

  public CheckErrorAlgorithm095() {
    super("Editor's signature or link to user space");
  }
}
