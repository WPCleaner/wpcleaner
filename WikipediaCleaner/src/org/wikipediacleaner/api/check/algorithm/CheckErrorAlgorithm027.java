/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 27 of check wikipedia project.
 * Error 27: Unicode syntax
 */
public class CheckErrorAlgorithm027 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Replace all"),
  };

  public CheckErrorAlgorithm027() {
    super("Unicode syntax");
  }

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
    while ((ampersandIndex >= 0) && (ampersandIndex < maxLength)) {
      // TODO : Check if we should look for a match a this position
      int tmpIndex = ampersandIndex + 1;
      if ((tmpIndex < maxLength) && (contents.charAt(tmpIndex) == '#')) {
        tmpIndex++;
      }
      int radix = 10;
      if ((tmpIndex < maxLength) && (contents.charAt(tmpIndex) == 'x')) {
        radix = 16;
        tmpIndex++;
      }
      int startIndex = tmpIndex;
      while ((tmpIndex < maxLength) &&
             (Character.digit(contents.charAt(tmpIndex), radix) >= 0)) {
        tmpIndex++;
      }
      if ((tmpIndex > startIndex) &&
          (tmpIndex < maxLength) &&
          (contents.charAt(tmpIndex) == ';')) {
        int entityNumber = Integer.parseInt(contents.substring(startIndex, tmpIndex), radix);
        HtmlCharacters htmlCharacter = HtmlCharacters.getCharacterByEntityNumber(entityNumber);
        boolean shouldReplace = true;
        if (htmlCharacter != null) {
          shouldReplace = htmlCharacter.shouldReplaceNumeric();
          if (HtmlCharacters.SYMBOL_VERTICAL_BAR.equals(htmlCharacter) &&
              (analysis.isInTemplate(ampersandIndex) != null)) {
            shouldReplace = false;
          }
        }
        if (shouldReplace) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, ampersandIndex, tmpIndex + 1,
              htmlCharacter != null ? ErrorLevel.ERROR : ErrorLevel.WARNING);
          if (htmlCharacter != null) {
            errorResult.addReplacement("" + htmlCharacter.getValue(), true);
          } else {
            errorResult.addReplacement("" + (char) entityNumber, false); 
          }
          errors.add(errorResult);
        }
      }
      ampersandIndex = contents.indexOf('&', ampersandIndex + 1);
    }

    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
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
