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
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing errors based on HTML named entities.
 */
public abstract class CheckErrorAlgorithmHtmlNamedEntities extends CheckErrorAlgorithmBase {

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
      result |= analyzeAmpersand(analysis, errors, ampersandIndex);
      ampersandIndex = contents.indexOf('&', ampersandIndex + 1);
    }
    return result;
  }

  /**
   * Analyze an ampersand to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param ampersandIndex Index of the ampersand in the text.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeAmpersand(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      int ampersandIndex) {

    // Check if we should look for a match at this position
    if (analysis.comments().isAt(ampersandIndex) ||
        (analysis.getSurroundingTag(WikiTagType.SOURCE, ampersandIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, ampersandIndex) != null)) {
      return false;
    }
    PageElementExternalLink link = analysis.isInExternalLink(ampersandIndex);
    if (link != null) {
      int offset = link.getTextOffset();
      if ((offset < 0) || (ampersandIndex < link.getBeginIndex() + offset)) {
        return false;
      }
    }

    boolean result = false;
    for (HtmlCharacters htmlCharacter : getHtmlCharacters()) {
      result |= analyzeHtmlCharacter(analysis, errors, ampersandIndex, htmlCharacter);
    }
    return result;
  }

  /**
   * Analyze an HTML character to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param ampersandIndex Index of the ampersand in the text.
   * @param htmlCharacter HTML character.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeHtmlCharacter(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      int ampersandIndex,
      HtmlCharacters htmlCharacter) {

    // Check if the character is used
    String contents = analysis.getContents();
    String name = htmlCharacter.getName();
    if ((name == null) ||
        !contents.startsWith(name, ampersandIndex + 1) ||
        !htmlCharacter.shouldReplaceName()) {
      return false;
    }
    ErrorLevel errorLevel = ErrorLevel.ERROR;

    // Analyze semicolon after the name
    int colonIndex = ampersandIndex + name.length() + 1;
    int maxLength = contents.length();
    if (useSemiColon()) {
      if ((colonIndex >= maxLength) || (contents.charAt(colonIndex) != ';')) {
        return false;
      }
    } else {
      if ((colonIndex < maxLength) && (contents.charAt(colonIndex) == ';')) {
        return false;
      }
      if (Character.isLetterOrDigit(contents.charAt(colonIndex))) {
        errorLevel = ErrorLevel.WARNING;
      }
      colonIndex--;
    }

    // Report error
    if (errors == null) {
      return true;
    }

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
    if (shouldAddSuggestions(analysis, ampersandIndex, htmlCharacter)) {
      errorResult.addReplacement(
          "" + htmlCharacter.getValue(),
          shouldReplaceAutomatically(analysis, ampersandIndex, htmlCharacter));
      if (endIndex > colonIndex + 1) {
        errorResult.addReplacement(
            "" + htmlCharacter.getValue() +
            contents.substring(colonIndex + 1, endIndex));
      }
      if (!useSemiColon()) {
        errorResult.addReplacement(
            "&amp;" + contents.substring(ampersandIndex + 1, endIndex));
      }
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * @return True if full HTML named entities should be searched.
   */
  protected boolean useSemiColon() {
    return true;
  }

  /**
   * @param analysis Page analysis.
   * @param ampersandIndex Index of the ampersand in the text.
   * @param htmlCharacter HTML character.
   * @return True if suggestions should be added.
   */
  protected boolean shouldAddSuggestions(
      PageAnalysis analysis,
      int ampersandIndex,
      HtmlCharacters htmlCharacter) {
    return true;
  }

  /**
   * @param analysis Page analysis.
   * @param ampersandIndex Index of the ampersand in the text.
   * @param htmlCharacter HTML character.
   * @return True if suggestion should be applied automatically.
   */
  protected boolean shouldReplaceAutomatically(
      PageAnalysis analysis,
      int ampersandIndex,
      HtmlCharacters htmlCharacter) {
    return false;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
