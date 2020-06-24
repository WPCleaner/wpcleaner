/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing errors based on HTML named entities.
 */
public abstract class CheckErrorAlgorithmHtmlNamedEntities extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Replace all"),
  };

  /**
   * @param name Name of the error.
   */
  public CheckErrorAlgorithmHtmlNamedEntities(String name) {
    super(name);
  }

  /**
   * @return List of HTML characters managed by this error.
   */
  protected abstract List<HtmlCharacters> getHtmlCharacters();

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Analyzing the text from the beginning
    boolean result = false;
    String contents = analysis.getContents();
    int ampersandIndex = contents.indexOf('&');
    int maxLength = contents.length();
    while ((ampersandIndex >= 0) && (ampersandIndex + 2 < maxLength)) {

      // Check if we should look for a match at this position
      boolean shouldMatch = true;
      if (shouldMatch &&
          ((analysis.isInComment(ampersandIndex) != null) ||
           (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, ampersandIndex) != null) ||
           (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, ampersandIndex) != null))) {
        shouldMatch = false;
      }
      if (shouldMatch) {
        PageElementExternalLink link = analysis.isInExternalLink(ampersandIndex);
        if (link != null) {
          int offset = link.getTextOffset();
          if ((offset < 0) || (ampersandIndex < link.getBeginIndex() + offset)) {
            shouldMatch = false;
          }
        }
      }

      if (shouldMatch) {
        for (HtmlCharacters htmlCharacter : getHtmlCharacters()) {
          String name = htmlCharacter.getName();
          if ((name != null) &&
              contents.startsWith(name, ampersandIndex + 1) &&
              htmlCharacter.shouldReplaceName()) {
            ErrorLevel errorLevel = ErrorLevel.ERROR;

            // Analyze semicolon after the name
            int colonIndex = ampersandIndex + name.length() + 1;
            boolean found = false;
            if (useSemiColon()) {
              if ((colonIndex < maxLength) && (contents.charAt(colonIndex) == ';')) {
                found = true;
              }
            } else {
              if ((colonIndex >= maxLength) ||
                  (contents.charAt(colonIndex) != ';')) {
                if (Character.isLetterOrDigit(contents.charAt(colonIndex))) {
                  errorLevel = ErrorLevel.WARNING;
                }
                found = true;
                colonIndex--;
              }
            }

            // Report error
            if (found) {
              if (errors == null) {
                return true;
              }
              result = true;

              // Analyze for possible semicolon afterwards
              int endIndex = colonIndex + 1;
              if (!useSemiColon()) {
                int tmpIndex = endIndex;
                while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
                  tmpIndex++;
                }
                if (contents.charAt(tmpIndex) == ';') {
                  endIndex = tmpIndex + 1;
                }
              }

              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, ampersandIndex, endIndex,
                  errorLevel);
              errorResult.addReplacement("" + htmlCharacter.getValue());
              if (endIndex > colonIndex + 1) {
                errorResult.addReplacement(
                    "" + htmlCharacter.getValue() +
                    contents.substring(colonIndex + 1, endIndex));
              }
              if (!useSemiColon()) {
                errorResult.addReplacement(
                    "&amp;" + contents.substring(ampersandIndex + 1, endIndex));
              }
              errors.add(errorResult);
            }
          }
        }
      }
      ampersandIndex = contents.indexOf('&', ampersandIndex + 1);
    }
    return result;
  }

  /**
   * @return True if full HTML named entities should be searched.
   */
  protected boolean useSemiColon() {
    return true;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
