/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a02x.a020;

import java.util.Collections;
import java.util.List;

import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmHtmlNamedEntities;


/**
 * Algorithm for analyzing error 20 of check wikipedia project.
 * Error 20: Symbol for dead
 */
public class CheckErrorAlgorithm020 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private static final List<HtmlCharacters> htmlCharacters = Collections.singletonList(HtmlCharacters.SYMBOL_DAGGER);

  public CheckErrorAlgorithm020() {
    super("Symbol for dead");
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }
}
