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
import org.wikipediacleaner.api.data.DefaultsortBlock;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.TagBlock;
import org.wikipediacleaner.api.data.TagData;
import org.wikipediacleaner.api.data.TemplateBlock;


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

  /**
   * Find the first DEFAULTSORT after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return DEFAULTSORT found.
   */
  protected DefaultsortBlock findNextDefaultsort(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex < contents.length())) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        DefaultsortBlock template = DefaultsortBlock.analyzeBlock(page.getWikipedia(), contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  /**
   * Find the first template after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Tag found.
   */
  protected TemplateBlock findNextTemplate(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex < contents.length())) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        TemplateBlock template = TemplateBlock.analyzeBlock(null, contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  /**
   * Find the first template after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param templateName Template to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  protected TemplateBlock findNextTemplate(
      Page page, String contents,
      String templateName, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex < contents.length())) {
      int tmpIndex = contents.indexOf("{{", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        TemplateBlock template = TemplateBlock.analyzeBlock(templateName, contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }

  /**
   * Find tags.
   * 
   * @param found Flag indicating if a tag has already been found.
   * @param page Page.
   * @param contents Page contents.
   * @param errors Errors.
   * @param tagName Tag name.
   * @return Flag indicating if a tag has been found.
   */
  protected boolean addTags(boolean found, Page page, String contents, ArrayList<CheckErrorResult> errors, String tagName) {
    if (found && (errors == null)) {
      return found;
    }
    boolean result = found;
    int startIndex = 0;
    while ((startIndex < contents.length())) {
      TagData tag = findNextStartTag(page, contents, tagName, startIndex);
      if (tag != null) {
        result = true;
        if (errors != null) {
          CheckErrorResult errorResult = new CheckErrorResult(
              getShortDescription(), tag.getStartIndex(), tag.getEndIndex());
          errors.add(errorResult);
        }
        startIndex = tag.getEndIndex();
      } else {
        startIndex = contents.length();
      }
    }
    startIndex = 0;
    while ((startIndex < contents.length())) {
      TagData tag = findNextEndTag(page, contents, tagName, startIndex);
      if (tag != null) {
        result = true;
        if (errors != null) {
          CheckErrorResult errorResult = new CheckErrorResult(
              getShortDescription(), tag.getStartIndex(), tag.getEndIndex());
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
   * Find the first tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tagName Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  protected TagBlock findNextTag(
      Page page, String contents,
      String tagName, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex < contents.length())) {
      int tmpIndex = contents.indexOf("<", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        TagBlock tag = TagBlock.analyzeBlock(tagName, contents, tmpIndex);
        if (tag != null) {
          return tag;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }

  /**
   * Find the last start tag before an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  protected TagData findPreviousStartTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex > 0)) {
      int tmpIndex = contents.lastIndexOf(">", currentIndex - 1);
      if (tmpIndex < 0) {
        currentIndex = 0;
      } else {
        int infoTagEnd = tmpIndex + 1;
        tmpIndex--;
        currentIndex = tmpIndex;
        // Possible whitespaces
        while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex--;
        }
        int startIndex = contents.lastIndexOf("<", tmpIndex);
        if (startIndex < 0) {
          currentIndex = 0;
        } else {
          int previousEndIndex = contents.lastIndexOf(">", tmpIndex);
          if (previousEndIndex < startIndex) {
            int infoTagStart = startIndex;
            startIndex++;
            // Possible whitespaces
            while ((startIndex < tmpIndex) && (contents.charAt(startIndex) == ' ')) {
              startIndex++;
            }
            if (tag.equalsIgnoreCase(contents.substring(startIndex, startIndex + tag.length()))) {
              startIndex += tag.length();
              if ((contents.charAt(startIndex) == ' ') ||
                  (contents.charAt(startIndex) == '>')) {
                return new TagData(infoTagStart, infoTagEnd); 
              }
            }
          }
        }
      }
    }
    return null;
  }

  /**
   * Find the first start tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  protected TagData findNextStartTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex < contents.length())) {
      int tmpIndex = contents.indexOf("<", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        int infoTagStart = tmpIndex;
        tmpIndex++;
        currentIndex = tmpIndex;
        // Possible whitespaces
        while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex++;
        }
        int endIndex = contents.indexOf(">", tmpIndex);
        if (endIndex < 0) {
          currentIndex = contents.length();
        } else {
          int nextStartIndex = contents.indexOf("<", tmpIndex);
          if ((nextStartIndex < 0) || (endIndex < nextStartIndex)) {
            int infoTagEnd = endIndex + 1;
            if (tag.equalsIgnoreCase(contents.substring(tmpIndex, tmpIndex + tag.length()))) {
              tmpIndex += tag.length();
              if ((contents.charAt(tmpIndex) == ' ') ||
                  (contents.charAt(tmpIndex) == '>')) {
                return new TagData(infoTagStart, infoTagEnd); 
              }
            }
          }
        }
      }
    }
    return null;
  }

  /**
   * Find the last end tag before an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  protected TagData findPreviousEndTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex > 0)) {
      int tmpIndex = contents.lastIndexOf(">", currentIndex - 1);
      if (tmpIndex < 0) {
        currentIndex = 0;
      } else {
        int infoTagEnd = tmpIndex + 1;
        tmpIndex--;
        currentIndex = tmpIndex;
        // Possible whitespaces
        while ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == ' ')) {
          tmpIndex--;
        }
        int startIndex = contents.lastIndexOf("<", tmpIndex);
        if (startIndex < 0) {
          currentIndex = 0;
        } else {
          int previousEndIndex = contents.lastIndexOf(">", tmpIndex);
          if (previousEndIndex < startIndex) {
            int infoTagStart = startIndex;
            startIndex++;
            if ((startIndex < tmpIndex) && (contents.charAt(startIndex) == '/')) {
              startIndex++;
              // Possible whitespaces
              while ((startIndex < tmpIndex) && (contents.charAt(startIndex) == ' ')) {
                startIndex++;
              }
              if (tag.equalsIgnoreCase(contents.substring(startIndex, startIndex + tag.length()))) {
                startIndex += tag.length();
                if ((contents.charAt(startIndex) == ' ') ||
                    (contents.charAt(startIndex) == '>')) {
                  return new TagData(infoTagStart, infoTagEnd); 
                }
              }
            }
          }
        }
      }
    }
    return null;
  }

  /**
   * Find the first end tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tag Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  protected TagData findNextEndTag(
      Page page, String contents,
      String tag, int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex < contents.length())) {
      int tmpIndex = contents.indexOf("<", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        int infoTagStart = tmpIndex;
        tmpIndex++;
        currentIndex = tmpIndex;
        if ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == '/')) {
          tmpIndex++;
          // Possible whitespaces
          while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
            tmpIndex++;
          }
          int endIndex = contents.indexOf(">", tmpIndex);
          if (endIndex < 0) {
            currentIndex = contents.length();
          } else {
            int nextStartIndex = contents.indexOf("<", tmpIndex);
            if ((nextStartIndex < 0) || (endIndex < nextStartIndex)) {
              int infoTagEnd = endIndex + 1;
              if (tag.equalsIgnoreCase(contents.substring(tmpIndex, tmpIndex + tag.length()))) {
                tmpIndex += tag.length();
                if ((contents.charAt(tmpIndex) == ' ') ||
                    (contents.charAt(tmpIndex) == '>')) {
                  return new TagData(infoTagStart, infoTagEnd); 
                }
              }
            }
          }
        }
      }
    }
    return null;
  }
}
