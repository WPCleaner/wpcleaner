/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a05x.a050;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmHtmlNamedEntities;


/**
 * Algorithm for analyzing error 50 of check wikipedia project.
 * Error 50: en dash or em dash
 */
public class CheckErrorAlgorithm050 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private final List<HtmlCharacters> htmlCharacters;

  public CheckErrorAlgorithm050() {
    super("en dash or em dash");
    htmlCharacters = new ArrayList<>();
    htmlCharacters.add(HtmlCharacters.SYMBOL_EM_DASH);
    htmlCharacters.add(HtmlCharacters.SYMBOL_EN_DASH);
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }
}
