/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
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
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementDefaultsort;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
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
  public boolean isAvailable() {
    return true;
  }

  /**
   * @param configuration Configuration of the error.
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#setConfiguration(org.wikipediacleaner.api.constants.CWConfigurationError)
   */
  public void setConfiguration(CWConfigurationError configuration) {
    this.configuration = configuration;
  }

  /**
   * @return Short description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getShortDescription() {
    return configuration.getShortDescription();
  }

  /**
   * @return Short description of the error.
   */
  public String getShortDescriptionReplaced() {
    return configuration.getShortDescriptionReplaced();
  }

  /**
   * @return Long description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getLongDescription() {
    return configuration.getLongDescription();
  }

  /**
   * @return Flag indicating if the detection is fully done.
   */
  public boolean isFullDetection() {
    return true;
  }

  /**
   * @return Link to error description.
   */
  public String getLink() {
    return configuration.getLink();
  }

  /**
   * Tell if a page is among the white list.
   * 
   * @param title Page title.
   * @return Page among the white list ?
   */
  public boolean isInWhiteList(String title) {
    return configuration.isInWhiteList(title);
  }

  /**
   * @return White list page name.
   */
  public String getWhiteListPageName() {
    return configuration.getWhiteListPageName();
  }

  /**
   * Create a CheckErrorResult object.
   * 
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   * @return new CheckErrorResult object.
   */
  public CheckErrorResult createCheckErrorResult(
      Page page,
      int startPosition, int endPosition) {
    return createCheckErrorResult(
        page,
        startPosition, endPosition,
        ErrorLevel.ERROR);
  }


  /**
   * Create a CheckErrorResult object.
   * 
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   * @param errorLevel Error level.
   * @return new CheckErrorResult object.
   */
  public CheckErrorResult createCheckErrorResult(
      Page page,
      int startPosition, int endPosition,
      ErrorLevel errorLevel) {
    if ((!ErrorLevel.CORRECT.equals(errorLevel)) && (page != null)) {
      if (isInWhiteList(page.getTitle())) {
        errorLevel = ErrorLevel.CORRECT;
      }
    }
    return new CheckErrorResult(
        getShortDescription(),
        startPosition, endPosition,
        errorLevel);
  }

  /**
   * @return Priority.
   */
  public int getPriority() {
    return configuration.getPriority();
  }

  /**
   * @return Error number.
   * (See Check Wikipedia project for the description of errors)
   */
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
  public String getSpecificProperty(
      String property,
      boolean useWiki, boolean useGeneral, boolean acceptEmpty) {
    return configuration.getSpecificProperty(property, useWiki, useGeneral, acceptEmpty);
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  public Map<String, String> getParameters() {
    Map<String, String> parameters = new Hashtable<String, String>();
    parameters.put("link", GT._("Title of the article describing this type of error"));
    parameters.put("whitelist", GT._("List of false positives for this type of error"));
    parameters.put("whitelistpage", GT._("Page containing the list of false positives for this type of error"));
    return parameters;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  public String automaticFix(Page page, String contents) {
    return contents;
  }

  /**
   * @return List of possible global fixes.
   */
  public String[] getGlobalFixes() {
    return null;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  public String fix(String fixName, Page page, String contents, MWPane textPane) {
    throw new IllegalStateException("This algorithm has no global fixes");
  }

  /**
   * Fix all the errors in the page by using the first replacement proposed.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  public String fixUsingFirstReplacement(String fixName, Page page, String contents) {
    String result = contents;
    PageAnalysis pageAnalysis = new PageAnalysis(page, contents);
    List<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(pageAnalysis, errors)) {
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
   * Fix all the errors in the page by removing.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @return Page contents after fix.
   */
  public String fixUsingRemove(String fixName, Page page, String contents) {
    String result = contents;
    PageAnalysis pageAnalysis = new PageAnalysis(page, contents);
    List<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(pageAnalysis, errors)) {
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
   * @param pageAnalysis Page analysis.
   * @param begin Index in text to begin search.
   * @param end Index in text to end search.
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacements Texts proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      PageAnalysis pageAnalysis, int begin, int end,
      Collection<CheckErrorResult> errors,
      String search, String[] replacements) {
    int startIndex = begin;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    while ((startIndex < contents.length()) && (startIndex < end)) {
      startIndex = contents.indexOf(search, startIndex);
      if ((startIndex >= 0) && (startIndex < end)) {
        if (errors == null) {
          return true;
        }
        result = true;
        int endIndex = startIndex + search.length();
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(), startIndex, endIndex);
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
   * @param pageAnalysis Page analysis.
   * @param errors Errors.
   * @param tagName Tag name.
   * @return Flag indicating if a tag has been found.
   */
  protected boolean addTags(
      boolean found, PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, String tagName) {
    if (found && (errors == null)) {
      return found;
    }
    boolean result = found;
    Collection<PageElementTag> tags = pageAnalysis.getTags(tagName);
    if (tags != null) {
      for (PageElementTag tag : tags) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            pageAnalysis.getPage(),
            tag.getBeginIndex(), tag.getEndIndex());
        errors.add(errorResult);
      }
    }
    return result;
  }

  /**
   * Add DEFAULTSORT if it's missing.
   * 
   * @param pageAnalysis Page analysis.
   * @return Page contents with the DEFAULTSORT added.
   */
  protected String addDefaultSort(PageAnalysis pageAnalysis) {
    // Basic check
    if ((pageAnalysis == null) ||
        (pageAnalysis.getContents() == null)) {
      return null;
    }
    String contents = pageAnalysis.getContents();
    if ((pageAnalysis.getPage() == null) ||
        (pageAnalysis.getPage().getTitle() == null)) {
      return contents;
    }

    // Check that DEFAULTSORT is missing
    PageElementDefaultsort defaultSort = pageAnalysis.getNextDefaultSort(0);
    if (defaultSort != null) {
      return contents;
    }

    // Find position to insert DEFAULTSORT
    int index = contents.length();
    PageElementCategory firstCategory = pageAnalysis.getNextCategory(0);
    if (firstCategory != null) {
      index = firstCategory.getBeginIndex();
    } else {
      PageElementLanguageLink firstLanguage = pageAnalysis.getNextLanguageLink(0);
      if (firstLanguage != null) {
        index = firstLanguage.getBeginIndex();
      }
    }

    // Add DEFAULTSORT
    StringBuilder buffer = new StringBuilder();
    if (index > 0) {
      buffer.append(contents.substring(0, index));
      if (contents.charAt(index - 1) != '\n') {
        buffer.append('\n');
      }
    }
    buffer.append("{{DEFAULTSORT:");

    // Remove special characters from title
    String title = pageAnalysis.getPage().getTitle();
    StringBuilder currentTitle = new StringBuilder();
    for (int i = 0; i < title.length(); i++) {
      char character = title.charAt(i);
      if (SpecialCharacters.isAuthorized(character, pageAnalysis.getWikipedia())) {
        currentTitle.append(character);
      } else {
        currentTitle.append(SpecialCharacters.proposeReplacement(character));
      }
    }

    // Manage capitals and small letters
    title = currentTitle.toString();
    currentTitle.setLength(0);
    boolean previousLetter = false;
    boolean previousSpace = true;
    for (int i = 0; i < title.length(); i++) {
      char character = title.charAt(i);
      if (previousSpace) {
        if (Character.isLowerCase(character)) {
          currentTitle.append(Character.toUpperCase(character));
        } else {
          currentTitle.append(character);
        }
      } else if (previousLetter) {
        if (Character.isUpperCase(character)) {
          currentTitle.append(Character.toLowerCase(character));
        } else {
          currentTitle.append(character);
        }
      } else {
        currentTitle.append(character);
      }

      previousLetter = Character.isLetter(character);
      previousSpace = (character == ' ');
    }

    // Finish
    buffer.append(currentTitle);
    buffer.append("}}\n");
    if (index < contents.length()) {
      buffer.append(contents.substring(index));
    }

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
