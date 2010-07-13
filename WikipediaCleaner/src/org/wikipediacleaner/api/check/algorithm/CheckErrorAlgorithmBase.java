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

import org.wikipediacleaner.api.check.CheckError;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Page;


/**
 * Abstract base class for analyzing errors.
 */
public abstract class CheckErrorAlgorithmBase implements CheckErrorAlgorithm {

  private int priority = CheckError.PRIORITY_UNKOWN;
  private String shortDescription;
  private String shortDescriptionReplaced;
  private String longDescription;
  private String link;

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
    this.shortDescription = desc;
    shortDescriptionReplaced = desc;
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
  public String getErrorNumber() {
    String baseName = CheckErrorAlgorithm.class.getName();
    String name = getClass().getName();
    if (name.startsWith(baseName)) {
      return name.substring(baseName.length());
    }
    return "unknown";
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
    ArrayList<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(page, contents, errors)) {
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
    ArrayList<CheckErrorResult> errors = new ArrayList<CheckErrorResult>();
    if (analyze(page, contents, errors)) {
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
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(Page page, String contents, ArrayList<CheckErrorResult> errors, String search) {
    return simpleTextSearch(page, contents, errors, search, (String[]) null);
  }

  /**
   * Search for simple text in page.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacement Text proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      Page page, String contents, ArrayList<CheckErrorResult> errors,
      String search, String replacement) {
    return simpleTextSearch(
        page, contents, errors, search,
        (replacement != null) ? new String[] { replacement } : null);
  }

  /**
   * Search for simple text in page.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param errors Errors found in the page.
   * @param search Text to be searched.
   * @param replacements Texts proposed as a replacement.
   * @return Flag indicating if the error was found.
   */
  protected boolean simpleTextSearch(
      Page page, String contents, ArrayList<CheckErrorResult> errors,
      String search, String[] replacements) {
    int startIndex = 0;
    boolean result = false;
    while (startIndex < contents.length()) {
      startIndex = contents.indexOf(search, startIndex);
      if (startIndex >= 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        int endIndex = startIndex + search.length();
        CheckErrorResult errorResult = new CheckErrorResult(getShortDescription(), startIndex, endIndex);
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
}
