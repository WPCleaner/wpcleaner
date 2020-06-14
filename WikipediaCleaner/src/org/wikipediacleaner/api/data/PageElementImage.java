/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WikiConfiguration;
import org.wikipediacleaner.api.data.contents.ContentsUtil;


/**
 * Class containing information about a complete image ([[namespace:image|...|text]]). 
 */
public class PageElementImage extends PageElement {

  private final EnumWikipedia wiki;
  private final String namespaceNotTrimmed;
  private final String namespace;
  private final String imageNotTrimmed;
  private final String image;
  private final int firstPipeOffset;
  private final List<Parameter> parameters;

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

    // Possible whitespace characters
    tmpIndex = ContentsUtil.moveIndexAfterWhitespace(contents, tmpIndex);

    // Search for :
    tmpIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, tmpIndex, ":|[]");
    if ((tmpIndex >= contents.length()) || (contents.charAt(tmpIndex) != ':')) {
      return null;
    }

    // Check that namespace is Image
    int colonIndex = tmpIndex;
    Namespace imageNamespace = wikipedia.getWikiConfiguration().getNamespace(Namespace.IMAGE);
    if (!imageNamespace.isPossibleName(contents.substring(beginIndex, colonIndex).trim())) {
      return null;
    }

    // Search for |
    tmpIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, tmpIndex, "|]");
    if (tmpIndex >= contents.length()) {
      return null;
    }

    // Simple Image tag [[Image:name]]
    int firstPipeIndex = tmpIndex;
    if (contents.charAt(tmpIndex) == ']') {
      if (!contents.startsWith("]]", tmpIndex)) {
        return null;
      }
      return new PageElementImage(
          wikipedia, index, tmpIndex + 2,
          contents.substring(beginIndex, colonIndex),
          contents.substring(colonIndex + 1, tmpIndex),
          firstPipeIndex - index, null);
    }

    // Find elements of image
    int pipeIndex = firstPipeIndex;
    tmpIndex++;
    int linkCount = 0;
    int externalLinkCount = 0;
    int templateCount = 0;
    List<Parameter> parameters = new ArrayList<Parameter>();
    WikiConfiguration wikiConfiguration = wikipedia.getWikiConfiguration();
    while (tmpIndex < contents.length()) {
      if ((templateCount <= 0) && (linkCount <= 0) && (contents.startsWith("]]", tmpIndex))) {
        if ((externalLinkCount > 0) &&
            (contents.startsWith("]]]", tmpIndex))) {
          externalLinkCount--;
          tmpIndex++;
        }
        String element = contents.substring(pipeIndex + 1, tmpIndex).trim();
        if (element.length() >= 0) {
          MagicWord magicWord = wikiConfiguration.getImgMagicWord(element);
          Parameter param = new Parameter(
              pipeIndex + 1 - index, tmpIndex - index,
              element, magicWord);
          parameters.add(param);
        }
        pipeIndex = tmpIndex;
        return new PageElementImage(
            wikipedia, index, tmpIndex + 2,
            contents.substring(beginIndex, colonIndex),
            contents.substring(colonIndex + 1, firstPipeIndex),
            firstPipeIndex - index, parameters);
      } else if ((templateCount <= 0) && (linkCount <= 0) && (contents.charAt(tmpIndex) == '|')) {
        String element = contents.substring(pipeIndex + 1, tmpIndex).trim();
        if (element.length() >= 0) {
          MagicWord magicWord = wikiConfiguration.getImgMagicWord(element);
          Parameter param = new Parameter(
              pipeIndex + 1 - index, tmpIndex - index,
              element, magicWord);
          parameters.add(param);
        }
        pipeIndex = tmpIndex;
      } else if (contents.startsWith("[[", tmpIndex)) {
        linkCount++;
        tmpIndex++;
      } else if (contents.startsWith("]]", tmpIndex)) {
        if ((externalLinkCount > 0) &&
            (contents.startsWith("]]]", tmpIndex))) {
          externalLinkCount--;
          tmpIndex++;
        }
        linkCount--;
        tmpIndex++;
      } else if (contents.startsWith("[", tmpIndex)) {
        externalLinkCount++;
      } else if (contents.startsWith("]", tmpIndex)) {
        externalLinkCount--;
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
   * @return Wiki.
   */
  public EnumWikipedia getWiki() {
    return wiki;
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
   * @return Offset of the first |
   */
  public int getFirstPipeOffset() {
    return firstPipeOffset;
  }

  /**
   * @return Parameter for the description.
   */
  public Parameter getDescriptionParameter() {
    if (parameters == null) {
      return null;
    }
    Parameter result = null;
    for (Parameter param : parameters) {
      if (param.getMagicWord() == null) {
        result = param; // To take the last parameter not related to a magic word
      }
    }
    return result;
  }

  /**
   * @return Image description.
   */
  public String getDescription() {
    Parameter param = getDescriptionParameter();
    if (param == null) {
      return null;
    }
    return param.getContents();
  }

  /**
   * @return Offset of image description.
   */
  public int getDescriptionOffset() {
    Parameter param = getDescriptionParameter();
    if (param == null) {
      return -1;
    }
    return param.getBeginOffset();
  }

  /**
   * @param magicWordName Magic word.
   * @return Parameter matching the magic word.
   */
  public Parameter getParameter(String magicWordName) {
    if (parameters == null) {
      return null;
    }
    WikiConfiguration wikiConfiguration = wiki.getWikiConfiguration();
    for (Parameter param : parameters) {
      String contents = param.getContents();
      if ((contents != null) &&
          (param.getMagicWord() != null) &&
          (wikiConfiguration.getMagicWordByName(magicWordName).isPossibleAlias(contents))) {
        return param;
      }
    }
    return null;
  }

  /**
   * @return Image alternate description.
   */
  public String getAlternateDescription() {
    Parameter param = getParameter(MagicWord.IMG_ALT);
    if ((param == null) || (param.getContents() == null)) {
      return null;
    }
    String contents = param.getContents();
    // TODO: Don't rely on "="
    int equalIndex = contents.indexOf("=");
    if (equalIndex >= 0) {
      return contents.substring(equalIndex + 1).trim();
    }
    return null;
  }

  /**
   * @return Parameters.
   */
  public Collection<Parameter> getParameters() {
    return parameters;
  }

  /**
   * @param wiki Wiki.
   * @param beginIndex Image begin index.
   * @param endIndex Image end index.
   * @param namespace Namespace.
   * @param image Image name.
   * @param firstPipeOffset Offset of the first "|"
   * @param parameters Parameters.
   */
  private PageElementImage(
      EnumWikipedia wiki, int beginIndex, int endIndex,
      String namespace, String image,
      int firstPipeOffset, List<Parameter> parameters) {
    super(beginIndex, endIndex);
    this.wiki = wiki;
    this.namespaceNotTrimmed = namespace;
    this.namespace = (namespace != null) ? namespace.trim() : null;
    this.imageNotTrimmed = image;
    this.image = (image != null) ? image.trim() : null;
    this.firstPipeOffset = firstPipeOffset;
    this.parameters = parameters;
  }

  /**
   * Change image to have a different description.
   * 
   * @param newDescription New description.
   * @return String representing the image with the new description.
   */
  public String getDescriptionReplacement(String newDescription) {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    sb.append(namespaceNotTrimmed);
    if (imageNotTrimmed != null) {
      sb.append(':');
      sb.append(imageNotTrimmed);
    }
    boolean descriptionAdded = false;
    for (Parameter param : parameters) {
      if (param.getMagicWord() != null) {
        sb.append('|');
        sb.append(param.getContents());
      } else {
        if (newDescription != null) {
          sb.append('|');
          sb.append(newDescription);
        }
        descriptionAdded = true;
      }
    }
    if (!descriptionAdded && (newDescription != null)) {
      sb.append('|');
      sb.append(newDescription);
    }
    sb.append("]]");
    return sb.toString();
  }

  /**
   * Change image to have a different descriptions.
   * 
   * @param newDescription New description.
   * @param altDescription New alternate description.
   * @return String representing the image with the new descriptions.
   */
  public String getDescriptionReplacement(
      String newDescription, String altDescription) {
    StringBuilder sb = new StringBuilder();
    sb.append("[[");
    sb.append(namespaceNotTrimmed);
    if (imageNotTrimmed != null) {
      sb.append(':');
      sb.append(imageNotTrimmed);
    }
    boolean descriptionAdded = false;
    boolean altDescriptionAdded = false;
    for (Parameter param : parameters) {
      MagicWord magicWord = param.getMagicWord();
      if (magicWord != null) {
        if (MagicWord.IMG_ALT.equals(magicWord.getName())) {
          if (altDescription != null) {
            sb.append('|');
            sb.append(magicWord.getName());
            sb.append('=');
            sb.append(altDescription);
          }
          altDescriptionAdded = true;
        } else {
          sb.append('|');
          sb.append(param.getContents());
        }
      } else {
        if (newDescription != null) {
          sb.append('|');
          sb.append(newDescription);
        }
        descriptionAdded = true;
      }
    }
    if (!altDescriptionAdded && (altDescription != null)) {
      sb.append('|');
      sb.append(MagicWord.IMG_ALT);
      sb.append('=');
      sb.append(altDescription);
    }
    if (!descriptionAdded && (newDescription != null)) {
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
    Parameter param = getDescriptionParameter();
    return getDescriptionReplacement(param != null ? param.getContents() : null);
  }

  /**
   * Bean for holding informations about a parameter.
   */
  public static class Parameter {
    /**
     * Begin offset.
     */
    private final int beginOffset;

    /**
     * End offset.
     */
    private final int endOffset;

    /**
     * Contents.
     */
    private final String contents;

    /**
     * Magic word.
     */
    private final MagicWord magicWord;

    public Parameter(
        int beginOffset, int endOffset,
        String contents, MagicWord magicWord) {
      this.beginOffset = beginOffset;
      this.endOffset = endOffset;
      this.contents = contents;
      this.magicWord = magicWord;
    }

    /**
     * @return Begin offset.
     */
    public int getBeginOffset() {
      return beginOffset;
    }

    /**
     * @return End offset.
     */
    public int getEndOffset() {
      return endOffset;
    }

    /**
     * @return Contents.
     */
    public String getContents() {
      return contents;
    }

    /**
     * @return Magic word.
     */
    public MagicWord getMagicWord() {
      return magicWord;
    }
  }
}
