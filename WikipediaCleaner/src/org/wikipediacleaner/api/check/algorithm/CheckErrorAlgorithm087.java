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
import org.wikipediacleaner.api.data.analysis.PageAnalysis;


/**
 * Algorithm for analyzing error 87 of check wikipedia project.
 * Error 87: HTML named entities without semicolon
 */
public class CheckErrorAlgorithm087 extends CheckErrorAlgorithmHtmlNamedEntities {

  /**
   * List of HTML characters managed by this error.
   */
  private final List<HtmlCharacters> htmlCharacters;

  public CheckErrorAlgorithm087() {
    super("HTML named entities without semicolon");
    htmlCharacters = new ArrayList<HtmlCharacters>();
    for (HtmlCharacters htmlCharacter : HtmlCharacters.values()) {
      htmlCharacters.add(htmlCharacter);
    }
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  @Override
  protected List<HtmlCharacters> getHtmlCharacters() {
    return htmlCharacters;
  }

  /**
   * @return True if full HTML named entities should be searched.
   */
  @Override
  protected boolean useSemiColon() {
    return false;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    return analysis.getContents();
  }
}
