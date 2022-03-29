/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a574;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.wikipediacleaner.api.check.algorithm.a5xx.a57x.a573.CheckErrorAlgorithm573;


/**
 * Algorithm for analyzing error 573 of check wikipedia project.
 * <br>
 * Error 573: Unnecessary non-breaking space <code>\u00A0</code>.
 */
public class CheckErrorAlgorithm574 extends CheckErrorAlgorithm573 {

  public CheckErrorAlgorithm574() {
    super();
  }

  private static final List<String> NBSP = Stream.of("\u00A0").collect(Collectors.toList());

  /**
   * @return List of texts matching a non-breaking space
   */
  @Override
  protected List<String> getNonBreakingSpaceTexts() {
    return NBSP;
  }
}
