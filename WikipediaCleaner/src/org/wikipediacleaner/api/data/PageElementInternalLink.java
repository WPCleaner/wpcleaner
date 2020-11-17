/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.ilink.ContentsInternalLinkBuilder;


/**
 * Class containing information about a complete internal link ([[link#anchor|text]]). 
 */
public class PageElementInternalLink extends PageElement {

  /** Characters included in the link when they follow the link */
  public static final String EXTENSION_CHARACTERS =
      "abcdefghijklmnopqrstuvwxyz" +
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  private final String linkNotTrimmed;
  private final String link;
  private final String anchorNotTrimmed;
  private final String anchor;
  private final String textNotTrimmed;
  private final String text;
  private final int    textOffset;

  /**
   * Analyze contents to check if it matches an internal link.
   * 
   * @param wiki Wiki.
   * @param contents Contents.
   * @param index Block start index.
   * @return Block details it there's a block.
   */
  public static PageElementInternalLink analyzeBlock(
      EnumWikipedia wiki, String contents, int index) {
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

    // Retrieve link
    int anchorIndex = -1;
    int pipeIndex = -1;
    int endIndex = -1;
    int level3CurlyBrackets = 0;
    while ((tmpIndex < contents.length()) &&
           (endIndex < 0) &&
           (pipeIndex < 0)) {
      switch (contents.charAt(tmpIndex)) {
      case '[':
        return null;
      case ']':
        if ((level3CurlyBrackets == 0) &&
            (contents.startsWith("]]", tmpIndex))) {
          endIndex = tmpIndex;
          tmpIndex++;
        } else {
          return null;
        }
        break;
      case '{':
        if (contents.startsWith("{{{", tmpIndex)) {
          level3CurlyBrackets++;
          tmpIndex += 2;
        } else if (contents.startsWith("{{", tmpIndex)) {
          int tmpEndIndex = ContentsUtil.moveIndexForwardWhileNotFound(contents, tmpIndex + 2, "\n}]{[");
          MagicWord magicWord = wiki.getWikiConfiguration().getFunctionMagicWord(contents.substring(tmpIndex + 2, tmpEndIndex), false);
          if ((magicWord == null) || !contents.startsWith("}}", tmpEndIndex)) {
            return null;
          }
          tmpIndex = tmpEndIndex + 1;
        } else {
          return null;
        }
        break;
      case '}':
        if ((contents.startsWith("}}}", tmpIndex)) &&
            (level3CurlyBrackets > 0)) {
          level3CurlyBrackets--;
          tmpIndex += 2;
        } else {
          return null;
        }
        break;
      case '#':
        if ((level3CurlyBrackets == 0) &&
            (anchorIndex < 0)) {
          anchorIndex = tmpIndex;
        }
        break;
      case '|':
        if (level3CurlyBrackets == 0) {
          pipeIndex = tmpIndex;
        }
      }
      tmpIndex++;
    }

    // Retrieve link text
    if (endIndex < 0) {
      int level1Brackets = 0;
      int level2CurlyBrackets = 0;
      level3CurlyBrackets = 0;
      while ((tmpIndex < contents.length()) && (endIndex < 0)) {
        switch (contents.charAt(tmpIndex)) {
        case '[':
          if (contents.startsWith("[[", tmpIndex)) {
            return null;
          }
          level1Brackets++;
          break;
        case ']':
          if (contents.startsWith("]]]", tmpIndex)) {
            if (level1Brackets > 0) {
              level1Brackets--;
            } else {
              endIndex = tmpIndex;
              tmpIndex++;
            }
          } else if (contents.startsWith("]]", tmpIndex)) {
            endIndex = tmpIndex;
            tmpIndex++;
          } else if (level1Brackets > 0) {
            level1Brackets--;
          }
          break;
        case '{':
          if (contents.startsWith("{{{", tmpIndex)) {
            level3CurlyBrackets++;
            tmpIndex += 2;
          } else if (contents.startsWith("{{", tmpIndex)) {
            level2CurlyBrackets++;
            tmpIndex++;
          }
          break;
        case '}':
          if ((contents.startsWith("}}}", tmpIndex)) &&
              (level3CurlyBrackets > 0)) {
            level3CurlyBrackets--;
            tmpIndex += 2;
          } else if ((contents.startsWith("}}", tmpIndex)) &&
                     (level2CurlyBrackets > 0)) {
            level2CurlyBrackets--;
            tmpIndex++;
          }
          break;
        }
        tmpIndex++;
      }
    }
    if (endIndex < 0) {
      return null;
    }

    // Extract link elements
    String link = null;
    String anchor = null;
    String text = null;
    int textOffset = -1;
    if ((pipeIndex >= 0) && (pipeIndex < endIndex)) {
      if ((anchorIndex >= 0) && (anchorIndex < pipeIndex)) {
        link = contents.substring(beginIndex, anchorIndex);
        anchor = contents.substring(anchorIndex + 1, pipeIndex);
      } else {
        link = contents.substring(beginIndex, pipeIndex);
      }
      text = contents.substring(pipeIndex + 1, endIndex);
      textOffset = pipeIndex + 1 - index;
    } else if ((anchorIndex >= 0) && (anchorIndex < endIndex)) {
      link = contents.substring(beginIndex, anchorIndex);
      anchor = contents.substring(anchorIndex + 1, endIndex);
    } else {
      link = contents.substring(beginIndex, endIndex);
    }

    // Check that it is really an internal link
    String linkTrimmed = link.trim();
    int colonIndex = linkTrimmed.indexOf(':');
    if (colonIndex > 0) {
      String namespaceName = linkTrimmed.substring(0, colonIndex);

      // Is it a category ?
      Namespace category = wiki.getWikiConfiguration().getNamespace(Namespace.CATEGORY);
      if ((category != null) && (category.isPossibleName(namespaceName))) {
        return null;
      }

      // Is it a file / image ?
      Namespace image = wiki.getWikiConfiguration().getNamespace(Namespace.IMAGE);
      if ((image != null) && (image.isPossibleName(namespaceName))) {
        return null;
      }

      // Is it a language link ?
      if (Language.isLanguageCode(
          wiki.getWikiConfiguration().getLanguages(), namespaceName)) {
        if (!namespaceName.equals(wiki.getSettings().getLanguage())) {
          return null;
        }
      }
    }

    // Is it an interwiki ?
    if (colonIndex >= 0) {
      String namespaceName = null;
      if (colonIndex == 0) {
        if (linkTrimmed.length() > 1) {
          colonIndex = linkTrimmed.indexOf(':', 1);
          if (colonIndex > 1) {
            namespaceName = linkTrimmed.substring(1, colonIndex);
          }
        }
      } else {
        namespaceName = linkTrimmed.substring(0, colonIndex);
      }
      if ((namespaceName != null) &&
          (wiki.getWikiConfiguration() != null) &&
          (wiki.getWikiConfiguration().getInterwikis() != null)) {
        for (Interwiki iw : wiki.getWikiConfiguration().getInterwikis()) {
          if (iw.getPrefix().equals(namespaceName) &&
              !iw.isForWiki(wiki)) {
            return null;
          }
        }
      }
    }

    // Check that this is not an external link between double square brackets
    if (PageElementExternalLink.isPossibleProtocol(linkTrimmed, 0)) {
      return null;
    }

    // Create internal link
    return new PageElementInternalLink(
        wiki,
        index, endIndex + 2,
        link, anchor, text, textOffset);
  }

  /**
   * Compute namespace based on the link.
   * 
   * @param wiki Wiki.
   * @return Namespace identifier.
   */
  public int getNamespace(EnumWikipedia wiki) {
    if ((wiki == null) || (link == null)) {
      return Namespace.MAIN;
    }
    int colonIndex = link.indexOf(':');
    if (colonIndex <= 0) {
      return Namespace.MAIN;
    }
    return Namespace.getNamespace(
        wiki.getWikiConfiguration().getNamespaces(),
        link.substring(0, colonIndex));
  }

  public String getLink() {
    return link;
  }

  public String getLinkNotNormalized() {
    return (linkNotTrimmed != null) ? linkNotTrimmed.trim() : null;
  }

  public String getAnchor() {
    return anchor;
  }

  public String getFullLink() {
    if (anchor == null) {
      return link;
    }
    return link + "#" + anchor;
  }

  public String getText() {
    return text;
  }

  public int getTextOffset() {
    return textOffset;
  }

  public String getDisplayedText() {
    if (text != null) {
      return text;
    }
    if (anchor == null) {
      return linkNotTrimmed;
    }
    return linkNotTrimmed + "#" + anchorNotTrimmed;
  }

  public String getDisplayedTextNotTrimmed() {
    if (textNotTrimmed != null) {
      return textNotTrimmed;
    }
    if (anchor == null) {
      return linkNotTrimmed;
    }
    return linkNotTrimmed + "#" + anchorNotTrimmed;
  }

  private PageElementInternalLink(
      EnumWikipedia wikipedia,
      int beginIndex, int endIndex,
      String link, String anchor,
      String text, int textOffset) {
    super(beginIndex, endIndex);
    this.linkNotTrimmed = link;
    this.link = (link != null) ? wikipedia.normalizeTitle(link) : null;
    this.anchorNotTrimmed = anchor;
    this.anchor = (anchor != null) ? anchor.trim() : null;
    this.textNotTrimmed = text;
    this.text = (text != null) ? text.trim() : null;
    this.textOffset = textOffset;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return ContentsInternalLinkBuilder
        .from(linkNotTrimmed)
        .withAnchor(anchorNotTrimmed)
        .withText(textNotTrimmed)
        .toString();
  }
}
