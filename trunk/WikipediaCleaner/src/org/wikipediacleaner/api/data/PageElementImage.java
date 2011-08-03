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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Class containing information about a complete image ([[namespace:image|...|text]]). 
 */
public class PageElementImage extends PageElement {

  private final EnumWikipedia wikipedia;
  private final String namespaceNotTrimmed;
  private final String namespace;
  private final String imageNotTrimmed;
  private final String image;
  private final List<String> magicWords;
  private final String descriptionNotTrimmed;
  private final String description;

  /**
   * Analyze contents to check if it matches an image.
   * 
   * @param wikipedia Wikipedia.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementImage analyzeBlock(
      EnumWikipedia wikipedia, String contents, int index) {
    // Verify arguments
    if (contents == null) {
      return null;
    }

    // Look for '[['
    int tmpIndex = index;
    if ((tmpIndex >= contents.length()) ||
        (!contents.startsWith("[[", tmpIndex))) {
      return null;
    }
    tmpIndex += 2;
    int beginIndex = tmpIndex;

    // Possible whitespaces characters
    while ((tmpIndex < contents.length()) && (contents.charAt(tmpIndex) == ' ')) {
      tmpIndex++;
    }

    // Search for :
    while ((tmpIndex < contents.length()) &&
           (contents.charAt(tmpIndex) != ':') &&
           (contents.charAt(tmpIndex) != '|') &&
           (contents.charAt(tmpIndex) != ']') &&
           (contents.charAt(tmpIndex) != '[')) {
      tmpIndex++;
    }
    if ((tmpIndex >= contents.length()) || (contents.charAt(tmpIndex) != ':')) {
      return null;
    }

    // Check that namespace is Image
    int colonIndex = tmpIndex;
    Namespace imageNamespace = Namespace.getNamespace(Namespace.IMAGE, wikipedia.getNamespaces());
    if (!imageNamespace.isPossibleName(contents.substring(beginIndex, colonIndex).trim())) {
      return null;
    }

    // Search for |
    while ((tmpIndex < contents.length()) &&
           (contents.charAt(tmpIndex) != '|') &&
           (contents.charAt(tmpIndex) != ']')) {
      tmpIndex++;
    }
    if (tmpIndex >= contents.length()) {
      return null;
    }

    // Simple Image tag [[Image:name]]
    if (contents.charAt(tmpIndex) == ']') {
      if (!contents.startsWith("]]", tmpIndex)) {
        return null;
      }
      return new PageElementImage(
          wikipedia, index, tmpIndex + 2,
          contents.substring(beginIndex, colonIndex),
          contents.substring(colonIndex + 1, tmpIndex),
          null, null);
    }

    // Find elements of image
    int firstPipeIndex = tmpIndex;
    int pipeIndex = firstPipeIndex;
    tmpIndex++;
    int linkCount = 0;
    int templateCount = 0;
    List<String> magicWords = new ArrayList<String>();
    while (tmpIndex < contents.length()) {
      if ((templateCount <= 0) && (linkCount <= 0) && (contents.startsWith("]]", tmpIndex))) {
        String element = contents.substring(pipeIndex + 1, tmpIndex);
        if (wikipedia.isPossibleAliasForImgMagicWord(element.trim())) {
          magicWords.add(element);
          pipeIndex = tmpIndex;
        }
        return new PageElementImage(
            wikipedia, index, tmpIndex + 2,
            contents.substring(beginIndex, colonIndex),
            contents.substring(colonIndex + 1, firstPipeIndex),
            magicWords,
            (pipeIndex < tmpIndex) ? contents.substring(pipeIndex + 1, tmpIndex) : null);
      } else if ((templateCount <= 0) && (linkCount <= 0) && (contents.charAt(tmpIndex) == '|')) {
        String element = contents.substring(pipeIndex + 1, tmpIndex);
        if (wikipedia.isPossibleAliasForImgMagicWord(element.trim())) {
          magicWords.add(element);
          pipeIndex = tmpIndex;
        }
      } else if (contents.startsWith("[[", tmpIndex)) {
        linkCount++;
        tmpIndex++;
      } else if (contents.startsWith("]]", tmpIndex)) {
        linkCount--;
        tmpIndex++;
      } else if (contents.startsWith("{{", tmpIndex)) {
        templateCount++;
        tmpIndex++;
      } else if (contents.startsWith("}}", tmpIndex)) {
        templateCount--;
        tmpIndex++;
      }
      tmpIndex++;
    }

    return null;
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /**
   * @return Image namespace name.
   */
  public String getNamespace() {
    return namespace;
  }

  /**
   * @return Image name.
   */
  public String getImage() {
    return image;
  }

  /**
   * @return Image description.
   */
  public String getDescription() {
    return description;
  }

  /**
   * @return Image alternate description.
   */
  public String getAlternateDescription() {
    if (magicWords != null) {
      for (String magicWord : magicWords) {
        if (wikipedia.getMagicWord(MagicWord.IMG_ALT).isPossibleAlias(magicWord)) {
          int equalIndex = magicWord.indexOf("=");
          if (equalIndex >= 0) {
            return magicWord.substring(equalIndex + 1).trim();
          }
        }
      }
    }
    return null;
  }

  /**
   * @return Magic words.
   */
  public Collection<String> getMagicWords() {
    return magicWords;
  }

  private PageElementImage(
      EnumWikipedia wikipedia, int beginIndex, int endIndex,
      String namespace, String image,
      List<String> magicWords, String description) {
    super(beginIndex, endIndex);
    this.wikipedia = wikipedia;
    this.namespaceNotTrimmed = namespace;
    this.namespace = (namespace != null) ? namespace.trim() : null;
    this.imageNotTrimmed = image;
    this.image = (image != null) ? image.trim() : null;
    this.magicWords = magicWords;
    this.descriptionNotTrimmed = description;
    this.description = (description != null) ? description.trim() : null;
  }

  public String getDescriptionReplacement(String newDescription) {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    sb.append(namespaceNotTrimmed);
    if (imageNotTrimmed != null) {
      sb.append(':');
      sb.append(imageNotTrimmed);
    }
    for (String magicWord : magicWords) {
      sb.append('|');
      sb.append(magicWord);
    }
    if (newDescription != null) {
      sb.append('|');
      sb.append(newDescription);
    }
    sb.append("]]");
    return sb.toString();
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return getDescriptionReplacement(descriptionNotTrimmed);
  }
}
