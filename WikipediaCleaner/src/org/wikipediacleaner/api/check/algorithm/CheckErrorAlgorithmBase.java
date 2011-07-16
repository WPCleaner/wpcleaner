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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.SpecialCharacters;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageContents;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTagData;
import org.wikipediacleaner.i18n.GT;


/**
 * Abstract base class for analyzing errors.
 */
public abstract class CheckErrorAlgorithmBase implements CheckErrorAlgorithm {

  private int priority = CheckErrorAlgorithms.PRIORITY_UNKOWN;
  private String shortDescription;
  private String shortDescriptionReplaced;
  private String longDescription;
  private String link;
  private String[] whiteList;

  /**
   * @param shortDescription Short description.
   */
  public CheckErrorAlgorithmBase(String shortDescription) {
    this.shortDescription = shortDescription;
  }

  /* (non-Javadoc)
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
   * @return Short description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getShortDescription() {
    return shortDescription;
  }

  /**
   * @return Short description of the error.
   */
  public String getShortDescriptionReplaced() {
    return shortDescriptionReplaced;
  }

  /**
   * @param desc Short description.
   */
  public void setShortDescription(String desc) {
    this.shortDescription = (desc != null) ? desc : "";
    shortDescriptionReplaced = this.shortDescription;
    shortDescriptionReplaced = shortDescriptionReplaced.replaceAll("&lt;", "<");
    shortDescriptionReplaced = shortDescriptionReplaced.replaceAll("&gt;", ">");
  }

  /**
   * @return Long description of the error.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getLongDescription() {
    return longDescription;
  }

  /**
   * @param desc Long description.
   */
  public void setLongDescription(String desc) {
    /*if (desc != null) {
      desc = desc.replaceAll("<nowiki>", "");
      desc = desc.replaceAll("</nowiki>", "");
    }*/
    this.longDescription = desc;
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
    return link;
  }

  /**
   * @param link Link to error description.
   */
  public void setLink(String link) {
    this.link = link;
  }

  /**
   * Tell if a page is among the white list.
   * 
   * @param title Page title.
   * @return Page among the white list ?
   */
  public boolean isInWhiteList(String title) {
    if ((whiteList == null) || (title == null)) {
      return false;
    }
    for (int i = 0; i < whiteList.length; i++) {
      if (title.equals(whiteList[i])) {
        return true;
      }
    }
    return false;
  }

  /**
   * @param whiteList White list.
   */
  public void setWhiteList(String[] whiteList) {
    this.whiteList = whiteList;
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
    return priority;
  }

  /**
   * @param priority Priority.
   */
  public void setPriority(int priority) {
    this.priority = priority;
  }

  /**
   * @return Error number.
   * (See Check Wikipedia project for the description of errors)
   */
  public String getErrorNumberString() {
    String baseName = CheckErrorAlgorithm.class.getName();
    String name = getClass().getName();
    if (name.startsWith(baseName)) {
      return name.substring(baseName.length());
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

  /* (non-Javadoc)
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm#getParameters()
   */
  public Map<String, String> getParameters() {
    Map<String, String> parameters = new Hashtable<String, String>();
    parameters.put("link", GT._("Title of the article describing this type of error"));
    parameters.put("whitelist", GT._("List of false positives for this type of error"));
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
   * @return Page contents after fix.
   */
  public String fix(String fixName, Page page, String contents) {
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
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacements Texts proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      PageAnalysis pageAnalysis, Collection<CheckErrorResult> errors,
      String search, String[] replacements) {
    int startIndex = 0;
    boolean result = false;
    String contents = pageAnalysis.getContents();
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf(search, startIndex);
      if (startIndex >= 0) {
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
    int startIndex = 0;
    String contents = pageAnalysis.getContents();
    while ((startIndex < contents.length())) {
      PageElementTagData tag = PageContents.findNextStartTag(
          pageAnalysis.getPage(), contents, tagName, startIndex);
      if (tag != null) {
        result = true;
        if (errors != null) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), tag.getBeginIndex(), tag.getEndIndex());
          errors.add(errorResult);
        }
        startIndex = tag.getEndIndex();
      } else {
        startIndex = contents.length();
      }
    }
    startIndex = 0;
    while ((startIndex < contents.length())) {
      PageElementTagData tag = PageContents.findNextEndTag(
          pageAnalysis.getPage(), contents, tagName, startIndex);
      if (tag != null) {
        result = true;
        if (errors != null) {
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), tag.getBeginIndex(), tag.getEndIndex());
          errors.add(errorResult);
        }
        startIndex = tag.getEndIndex();
      } else {
        startIndex = contents.length();
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
    // TODO

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
}
