/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a00x.a006;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.PageElementFunction;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;

/**
 * Algorithm for analyzing error 06 of check wikipedia project.
 * Error 06: DEFAULTSORT with special letters
 */
public class CheckErrorAlgorithm006 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Fix DEFAULTSORT"),
  };

  public CheckErrorAlgorithm006() {
    super("DEFAULTSORT with special letters");
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
    List<PageElementFunction> tags = analysis.getDefaultSorts();
    if ((tags == null) || (tags.isEmpty())) {
      return false;
    }
    boolean result = false;
    for (PageElementFunction tag : tags) {
      result |= analyzeTag(analysis, tag, errors);
    }
    return result;
  }

  /**
   * Analyze a tag to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param tag Tag to analyze
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeTag(
      PageAnalysis analysis,
      PageElementFunction tag,
      Collection<CheckErrorResult> errors) {

    // Check for error
    EnumWikipedia wiki = analysis.getWikipedia();
    boolean characterFound = false;
    boolean characterReplaced = false;
    String unknownCharacters = "";
    String text = "";
    int lastPos = 0;
    int currentPos = 0;
    String value = (tag.getParameterCount() > 0) ? tag.getParameterValue(0) : "";
    while (currentPos < value.length()) {
      char character = value.charAt(currentPos);
      boolean error = !SpecialCharacters.isAuthorized(character, wiki);
      if (error && character == '{' && value.startsWith("{{", currentPos)) {
        int templateEndIndex = value.indexOf("}}", currentPos);
        if (templateEndIndex > 0) {
          error = false;
          currentPos = templateEndIndex + 1;
        }
      }
      if (error) {
        characterFound = true;
        text += value.substring(lastPos, currentPos);
        String newCharacter = SpecialCharacters.proposeReplacement(character, wiki);
        if (!Character.toString(character).equals(newCharacter)) {
          characterReplaced = true;
        } else {
          unknownCharacters += character;
        }
        text += newCharacter;
        lastPos = currentPos + 1;
      }
      currentPos++;
    }

    // Report error
    if (characterFound) {
      if (errors == null) {
        return true;
      }
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, tag.getBeginIndex(), tag.getEndIndex());
      if (characterReplaced) {
        if (lastPos < value.length()) {
          text += value.substring(lastPos);
        }
        errorResult.addReplacement(PageElementFunction.createFunction(tag.getFunctionName(), text));
      } else {
        errorResult.addText(
            GT._T("Unable to replace the characters [{0}]", unknownCharacters));
      }
      errors.add(errorResult);
      return true;
    }
    if (tag.getParameterCount() > 1) {
      if (errors == null) {
        return true;
      }
      int separatorIndex = tag.getParameterSeparatorOffset(1);
      CheckErrorResult errorResult = createCheckErrorResult(analysis, separatorIndex, separatorIndex + 1);
      errors.add(errorResult);
      return true;
    }
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
