/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.check.HtmlCharacters;


/**
 * Algorithm for analyzing error 20 of check wikipedia project.
 * Error 20: Symbol for dead
 */
public class CheckErrorAlgorithm020 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private final List<HtmlCharacters> htmlCharacters;

  public CheckErrorAlgorithm020() {
    super("Symbol for dead");
    htmlCharacters = new ArrayList<HtmlCharacters>();
    htmlCharacters.add(HtmlCharacters.SYMBOL_DAGGER);
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }
}
