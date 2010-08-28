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

package org.wikipediacleaner.api.data;


/**
 * Utility class to manage page contents.
 */
public class PageContents {

  // ==========================================================================
  // Internal link management
  // ==========================================================================

  /**
   * Find the first internal link after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Internal link found.
   */
  public static PageElementInternalLink findNextInternalLink(
      Page page, String contents,
      int currentIndex) {
    if (contents == null) {
      return null;
    }
    while ((currentIndex < contents.length())) {
      int tmpIndex = contents.indexOf("[[", currentIndex);
      if (tmpIndex < 0) {
        currentIndex = contents.length();
      } else {
        PageElementInternalLink link = PageElementInternalLink.analyzeBlock(
            page.getWikipedia(), contents, tmpIndex);
        if (link!= null) {
          return link;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // DEFAULTSORT management
  // ==========================================================================

  /**
   * Find the first DEFAULTSORT after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return DEFAULTSORT found.
   */
  public static PageElementDefaultsort findNextDefaultsort(
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
        PageElementDefaultsort template = PageElementDefaultsort.analyzeBlock(page.getWikipedia(), contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 2;
      }
    }
    return null;
  }

  // ==========================================================================
  // Template management
  // ==========================================================================

  /**
   * Find the first template after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTemplate findNextTemplate(
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
        PageElementTemplate template = PageElementTemplate.analyzeBlock(null, contents, tmpIndex);
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
  public static PageElementTemplate findNextTemplate(
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
        PageElementTemplate template = PageElementTemplate.analyzeBlock(templateName, contents, tmpIndex);
        if (template != null) {
          return template;
        }
        currentIndex = tmpIndex + 1;
      }
    }
    return null;
  }

  // ==========================================================================
  // Tag management
  // ==========================================================================

  /**
   * Find the first tag after an index in the page contents.
   * 
   * @param page Page.
   * @param contents Page contents (may be different from page.getContents()).
   * @param tagName Tag to be found.
   * @param currentIndex The last index.
   * @return Tag found.
   */
  public static PageElementTag findNextTag(
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
        PageElementTag tag = PageElementTag.analyzeBlock(tagName, contents, tmpIndex);
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
  public static PageElementTagData findPreviousStartTag(
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
                return new PageElementTagData(infoTagStart, infoTagEnd); 
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
  public static PageElementTagData findNextStartTag(
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
                return new PageElementTagData(infoTagStart, infoTagEnd); 
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
  public static PageElementTagData findPreviousEndTag(
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
                  return new PageElementTagData(infoTagStart, infoTagEnd); 
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
  public static PageElementTagData findNextEndTag(
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
                  return new PageElementTagData(infoTagStart, infoTagEnd); 
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
