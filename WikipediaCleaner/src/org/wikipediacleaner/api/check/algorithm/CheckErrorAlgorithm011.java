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
 * Algorithm for analyzing error 11 of check wikipedia project.
 * Error 11: HTML named entities
 */
public class CheckErrorAlgorithm011 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private final List<HtmlCharacters> htmlCharacters;

  public CheckErrorAlgorithm011() {
    super("HTML named entities");
    htmlCharacters = new ArrayList<HtmlCharacters>();
    for (HtmlCharacters htmlCharacter : HtmlCharacters.values()) {
      if (!HtmlCharacters.SYMBOL_DAGGER.equals(htmlCharacter) &&
          !HtmlCharacters.SYMBOL_EM_DASH.equals(htmlCharacter) &&
          !HtmlCharacters.SYMBOL_EN_DASH.equals(htmlCharacter)) {
        htmlCharacters.add(htmlCharacter);
      }
    }
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }
}
