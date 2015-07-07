/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter.HighlightPainter;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.constants.CWConfigurationError;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Abstract base class for analyzing errors.
 */
public abstract class CheckErrorAlgorithmBase implements CheckErrorAlgorithm {

  /**
   * Configuration of the error.
   */
  private CWConfigurationError configuration;

  private final String name;

  /**
   * @param name Name of the error.
   */
  public CheckErrorAlgorithmBase(String name) {
    this.name = name;
  }

  /**
   * @return Name of the error.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Textual representation of the object.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return getShortDescriptionReplaced();
  }

  /**
   * @return Flag indicating if this algorithm is available.
   */
  @Override
  public boolean isAvailable() {
    return true;
  }

  /**
   * @return True if the error has a list of pages.
   */
  @Override
  public boolean hasList() {
    if (getErrorNumber() < MAX_ERROR_NUMBER_WITH_LIST) {
      return true;
    }
    return hasSpecialList();
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return false;
  }

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  @Override
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit) {
    return null;
  }

  /**
   * @param configuration Configuration of the error.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#setConfiguration(org.wikipediacleaner.api.constants.CWConfigurationError)
   */
  @Override
  public void setConfiguration(CWConfigurationError configuration) {
    this.configuration = configuration;
  }

  /**
   * @return Short description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  @Override
  public String getShortDescription() {
    return configuration.getShortDescription();
  }

  /**
   * @return Short description of the error.
   */
  @Override
  public String getShortDescriptionReplaced() {
    return configuration.getShortDescriptionReplaced();
  }

  /**
   * @return Long description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  @Override
  public String getLongDescription() {
    return configuration.getLongDescription();
  }

  /**
   * @return Link to error description.
   */
  @Override
  public String getLink() {
    return configuration.getLink();
  }

  /**
   * Tell if a page is among the white list.
   * 
   * @param title Page title.
   * @return Page among the white list ?
   */
  @Override
  public boolean isInWhiteList(String title) {
    return configuration.isInWhiteList(title);
  }

  /**
   * @return White list page name.
   */
  @Override
  public String getWhiteListPageName() {
    return configuration.getWhiteListPageName();
  }

  /**
   * Create a CheckErrorResult object.
   * 
   * @param analysis Page analysis
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   * @return new CheckErrorResult object.
   */
  public CheckErrorResult createCheckErrorResult(
      PageAnalysis analysis,
      int startPosition, int endPosition) {
    return createCheckErrorResult(
        analysis,
        startPosition, endPosition,
        ErrorLevel.ERROR);
  }


  /**
   * Create a CheckErrorResult object.
   * 
   * @param analysis Page analysis
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   * @param errorLevel Error level.
   * @return new CheckErrorResult object.
   */
  public CheckErrorResult createCheckErrorResult(
      PageAnalysis analysis,
      int startPosition, int endPosition,
      ErrorLevel errorLevel) {
    if ((!ErrorLevel.CORRECT.equals(errorLevel)) &&
        (analysis != null) &&
        (analysis.getPage() != null)) {
      if (isInWhiteList(analysis.getPage().getTitle())) {
        errorLevel = ErrorLevel.CORRECT;
      }
    }
    return new CheckErrorResult(
        this,
        startPosition, endPosition,
        errorLevel);
  }

  /**
   * @return Priority.
   */
  @Override
  public int getPriority() {
    return configuration.getPriority();
  }

  /**
   * @return Error number.
   * (See Check Wikipedia project for the description of errors)
   */
  @Override
  public String getErrorNumberString() {
    String baseName = CheckErrorAlgorithm.class.getName();
    String className = getClass().getName();
    if (className.startsWith(baseName)) {
      return className.substring(baseName.length());
    }
    return "unknown";
  }

  /**
   * @return Error number.
   * (See Check Wikipedia project for the description of errors)
   */
  @Override
  public int getErrorNumber() {
    int errorNumber = -1;
    try {
      String errorNumberString = getErrorNumberString();
      errorNumber = Integer.parseInt(errorNumberString);
    } catch (NumberFormatException e) {
      //
    }
    return errorNumber;
  }


  /**
   * @param property Property name.
   * @param useWiki Flag indicating if wiki configuration can be used.
   * @param useGeneral Flag indicating if general configuration can be used.
   * @param acceptEmpty Flag indicating if empty strings are accepted.
   * @return Property value.
   */
  @Override
  public String getSpecificProperty(
      String property,
      boolean useWiki, boolean useGeneral, boolean acceptEmpty) {
    return configuration.getSpecificProperty(property, useWiki, useGeneral, useWiki, acceptEmpty);
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = new Hashtable<String, String>();
    parameters.put("link", GT._("Title of the article describing this type of error"));
    parameters.put("noauto", GT._("Set to true to prevent automatic modifications for this type of error"));
    parameters.put("whitelist", GT._("List of false positives for this type of error"));
    parameters.put("whitelistpage", GT._("Page containing the list of false positives for this type of error"));
    return parameters;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public final String automaticFix(PageAnalysis analysis) {
    if (configuration.getNoAuto()) {
      return analysis.getContents();
    }
    return internalAutomaticFix(analysis);
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return analysis.getContents();
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public final String botFix(PageAnalysis analysis) {
    if (configuration.getNoAuto()) {
      return analysis.getContents();
    }
    return internalBotFix(analysis);
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  protected String internalBotFix(PageAnalysis analysis) {
    return internalAutomaticFix(analysis);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return null;
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
    throw new IllegalStateException("This algorithm has no global fixes");
  }

  /**
   * Fix all the errors in the page by using the first replacement proposed.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  public String fixUsingFirstReplacement(String fixName, PageAnalysis analysis) {
    String result = analysis.getContents();
    List<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(analysis, errors, false)) {
      for (int i = errors.size(); i > 0; i--) {
        CheckErrorResult errorResult = errors.get(i - 1);
        String newText = errorResult.getFirstReplacement();
        if (newText != null) {
          String tmp =
            result.substring(0, errorResult.getStartPosition()) +
            newText +
            result.substring(errorResult.getEndPosition());
          result = tmp;
        }
      }
    }
    return result;
  }

  /**
   * Fix all the errors in the page by using automatic replacement proposed.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  public String fixUsingAutomaticReplacement(PageAnalysis analysis) {
    String result = analysis.getContents();
    List<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(analysis, errors, true)) {
      Collections.sort(errors);
      for (int i = errors.size(); i > 0; i--) {
        CheckErrorResult errorResult = errors.get(i - 1);
        String newText = errorResult.getAutomaticReplacement();
        if (newText != null) {
          String tmp =
            result.substring(0, errorResult.getStartPosition()) +
            newText +
            result.substring(errorResult.getEndPosition());
          result = tmp;
        }
      }
    }
    return result;
  }

  /**
   * Fix all the errors in the page by using automatic bot replacement proposed.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  public String fixUsingAutomaticBotReplacement(PageAnalysis analysis) {
    String result = analysis.getContents();
    List<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(analysis, errors, true)) {
      Collections.sort(errors);
      for (int i = errors.size(); i > 0; i--) {
        CheckErrorResult errorResult = errors.get(i - 1);
        String newText = errorResult.getAutomaticBotReplacement();
        if (newText != null) {
          String tmp =
            result.substring(0, errorResult.getStartPosition()) +
            newText +
            result.substring(errorResult.getEndPosition());
          result = tmp;
        }
      }
    }
    return result;
  }

  /**
   * Fix all the errors in the page by removing.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  public String fixUsingRemove(String fixName, PageAnalysis analysis) {
    String result = analysis.getContents();
    List<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(analysis, errors, false)) {
      for (int i = errors.size(); i > 0; i--) {
        CheckErrorResult errorResult = errors.get(i - 1);
        String tmp =
          result.substring(0, errorResult.getStartPosition()) +
          result.substring(errorResult.getEndPosition());
        result = tmp;
      }
    }
    return result;
  }

  /**
   * @param contents Contents.
   * @param startIndex Index for starting the search.
   * @return First index after the start index that is not a space character.
   */
  protected int getFirstIndexAfterSpace(String contents, int startIndex) {
    if (contents == null) {
      return startIndex;
    }
    int maxLength = contents.length();
    while ((startIndex < maxLength) && (contents.charAt(startIndex) == ' ')) {
      startIndex++;
    }
    return startIndex;
  }

  /**
   * @param contents Contents.
   * @param startIndex Index for starting the search.
   * @return Last index before the start index that is not a space character.
   */
  protected int getLastIndexBeforeSpace(String contents, int startIndex) {
    if (contents == null) {
      return startIndex;
    }
    while ((startIndex >= 0) && (contents.charAt(startIndex) == ' ')) {
      startIndex--;
    }
    return startIndex;
  }

  /**
   * @param contents Contents.
   * @param startIndex Index for starting the search.
   * @return Last index before the start index that is not a whitespace character.
   */
  protected int getLastIndexBeforeWhiteSpace(String contents, int startIndex) {
    if (contents == null) {
      return startIndex;
    }
    while ((startIndex >= 0) && (Character.isWhitespace(contents.charAt(startIndex)))) {
      startIndex--;
    }
    return startIndex;
  }

  /**
   * Search for simple text in page.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, String search) {
    return simpleTextSearch(pageAnalysis, errors, search, (String[]) null);
  }

  /**
   * Search for simple text in page.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacement Text proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      PageAnalysis pageAnalysis, Collection<CheckErrorResult> errors,
      String search, String replacement) {
    return simpleTextSearch(
        pageAnalysis, errors, search,
        (replacement != null) ? new String[] { replacement } : null);
  }

  /**
   * Search for simple text in page.
   * 
   * @param pageAnalysis Page analysis.
   * @param begin Index in text to begin search.
   * @param end Index in text to end search.
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacement Text proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      PageAnalysis pageAnalysis, int begin, int end,
      Collection<CheckErrorResult> errors,
      String search, String replacement) {
    return simpleTextSearch(
        pageAnalysis, begin, end, errors, search,
        (replacement != null) ? new String[] { replacement } : null);
  }

  /**
   * Search for simple text in page.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacements Texts proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      PageAnalysis pageAnalysis, Collection<CheckErrorResult> errors,
      String search, String[] replacements) {
    String contents = pageAnalysis.getContents();
    return simpleTextSearch(
        pageAnalysis, 0, contents.length(),
        errors, search, replacements);
  }

  /**
   * Search for simple text in page.
   * 
   * @param analysis Page analysis.
   * @param begin Index in text to begin search.
   * @param end Index in text to end search.
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacements Texts proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      PageAnalysis analysis, int begin, int end,
      Collection<CheckErrorResult> errors,
      String search, String[] replacements) {
    int startIndex = begin;
    boolean result = false;
    String contents = analysis.getContents();
    while ((startIndex < contents.length()) && (startIndex < end)) {
      startIndex = contents.indexOf(search, startIndex);
      if ((startIndex >= 0) && (startIndex < end)) {
        if (errors == null) {
          return true;
        }
        result = true;
        int endIndex = startIndex + search.length();
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, startIndex, endIndex);
        if (replacements != null) {
          for (int i = 0; i < replacements.length; i++) {
            if (replacements[i] != null) {
              errorResult.addReplacement(replacements[i]);
            }
          }
        }
        errors.add(errorResult);
        startIndex = endIndex;
      } else {
        startIndex = contents.length();
      }
    }
    return result;
  }

  /**
   * Find tags.
   * 
   * @param found Flag indicating if a tag has already been found.
   * @param analysis Page analysis.
   * @param errors Errors.
   * @param tagName Tag name.
   * @return Flag indicating if a tag has been found.
   */
  protected boolean addTags(
      boolean found, PageAnalysis analysis,
      Collection<CheckErrorResult> errors, String tagName) {
    if (found && (errors == null)) {
      return found;
    }
    boolean result = found;
    Collection<PageElementTag> tags = analysis.getTags(tagName);
    if (tags != null) {
      for (PageElementTag tag : tags) {
        boolean shouldCount = true;
        if (shouldCount &&
            !PageElementTag.TAG_WIKI_NOWIKI.equalsIgnoreCase(tagName)) {
          if (analysis.getSurroundingTag(
              PageElementTag.TAG_WIKI_NOWIKI,
              tag.getBeginIndex()) != null) {
            shouldCount = false;
          }
        }
        if (shouldCount) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              tag.getBeginIndex(), tag.getEndIndex());
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /**
   * Create a default DEFAULTSORT.
   * 
   * @param analysis Page analysis.
   * @return Default DEFAULTSORT.
   */
  protected String createDefaultSort(PageAnalysis analysis) {
    // Basic check
    if ((analysis == null) ||
        (analysis.getContents() == null)) {
      return null;
    }
    String contents = analysis.getContents();
    if ((analysis.getPage() == null) ||
        (analysis.getPage().getTitle() == null)) {
      return contents;
    }

    // Get DEFAULTSORT name
    String defaultSort = "DEFAULTSORT:";
    MagicWord magicWord = analysis.getWikiConfiguration().getMagicWordByName(MagicWord.DEFAULT_SORT);
    if (magicWord != null) {
      String value = analysis.getWPCConfiguration().getString(WPCConfigurationString.DEFAULTSORT);
      if ((value != null) && (value.trim().length() > 0)) {
        value = value.trim();
        if (magicWord.isPossibleAlias(value)) {
          defaultSort = value;
        }
      }
    }

    // Create DEFAULTSORT
    StringBuilder buffer = new StringBuilder();
    buffer.append("{{");
    buffer.append(defaultSort);

    // Remove special characters from title
    String title = analysis.getPage().getTitle();
    StringBuilder currentTitle = new StringBuilder();
    EnumWikipedia wiki = analysis.getWikipedia();
    for (int i = 0; i < title.length(); i++) {
      char character = title.charAt(i);
      if (!CheckErrorAlgorithms.isAlgorithmActive(wiki, 6) ||
          SpecialCharacters.isAuthorized(character, wiki)) {
        currentTitle.append(character);
      } else {
        currentTitle.append(SpecialCharacters.proposeReplacement(character, wiki));
      }
    }

    // Manage capitals and small letters
    title = currentTitle.toString();
    currentTitle.setLength(0);
    boolean previousSpace = true;
    for (int i = 0; i < title.length(); i++) {
      char character = title.charAt(i);
      if (previousSpace) {
        if (Character.isLowerCase(character) && (i == 0)) {
          currentTitle.append(Character.toUpperCase(character));
        } else {
          currentTitle.append(character);
        }
      } else {
        currentTitle.append(character);
      }

      previousSpace = (character == ' ');
    }

    // Finish
    buffer.append(currentTitle);
    buffer.append("}}");

    return buffer.toString();
  }

  // ==========================================================================
  // Manage highlights in the text
  // ==========================================================================

  /**
   * A painter for highlighting text.
   */
  private final static HighlightPainter highlighter =
      new DefaultHighlighter.DefaultHighlightPainter(Color.YELLOW);

  /**
   * Add an highlight on the text pane.
   * 
   * @param textPane Text pane.
   * @param beginIndex Beginning of the highlight area.
   * @param endIndex End of the highlight area.
   * @return Highlight tag.
   */
  protected Object addHighlight(
      JTextPane textPane, int beginIndex, int endIndex) {
    if (textPane != null) {
      try {
        return textPane.getHighlighter().addHighlight(
            beginIndex, endIndex, highlighter);
      } catch (BadLocationException e) {
        //
      }
    }
    return null;
  }

  /**
   * Remove an highlight of the text pane.
   * 
   * @param textPane Text pane.
   * @param highlight Highlight tag.
   */
  protected void removeHighlight(JTextPane textPane, Object highlight) {
    if ((textPane != null) && (highlight != null)) {
      textPane.getHighlighter().removeHighlight(highlight);
    }
  }
}
