/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2024  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a0xx.a06x.a069;

import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;

class ISBNExtension {

  public final int beginIndex;
  public final int endIndex;
  public final int parenthesisBefore;
  public final int parenthesisAfter;
  public final int squareBracketBefore;
  public final int squareBracketAfter;
  public final int smallBefore;
  public final int smallAfter;
  public final String textAfter;

  private ISBNExtension(
      final int beginIndex, final int endIndex,
      final int parenthesisBefore, final int parenthesisAfter,
      final int squareBracketBefore, final int squareBracketAfter,
      final int smallBefore, final int smallAfter,
      final String textAfter) {
    this.beginIndex = beginIndex;
    this.endIndex = endIndex;
    this.parenthesisBefore = parenthesisBefore;
    this.parenthesisAfter = parenthesisAfter;
    this.squareBracketBefore = squareBracketBefore;
    this.squareBracketAfter = squareBracketAfter;
    this.smallBefore = smallBefore;
    this.smallAfter = smallAfter;
    this.textAfter = textAfter;
  }

  public static ISBNExtension of(
      final PageAnalysis analysis,
      final int initialBeginIndex,
      final int initialEndIndex) {
    
    // Try to extend before the ISBN
    String contents = analysis.getContents();
    int parenthesisBefore = 0;
    int squareBracketBefore = 0;
    int smallBefore = 0;
    int beginIndex = initialBeginIndex;
    boolean continueSearching = true;
    while (continueSearching && beginIndex > 0) {
      continueSearching = false;
      if (contents.charAt(beginIndex - 1) == '(') {
        parenthesisBefore++;
        beginIndex--;
        continueSearching = true;
      } else if (contents.charAt(beginIndex - 1) == '[') {
        squareBracketBefore++;
        beginIndex--;
        continueSearching = true;
      } else if (contents.charAt(beginIndex - 1) == '>') {
        PageElementTag smallTagBefore = analysis.isInTag(beginIndex - 1, HtmlTagType.SMALL);
        if (smallTagBefore != null &&
            smallTagBefore.getEndIndex() == beginIndex &&
            !smallTagBefore.isFullTag() &&
            !smallTagBefore.isEndTag()) {
          smallBefore++;
          beginIndex = smallTagBefore.getBeginIndex();
          continueSearching = true;
        }
      }
    }

    // Try extend after the ISBN
    int parenthesisAfter = 0;
    int squareBracketAfter = 0;
    int smallAfter = 0;
    int endIndex = initialEndIndex;
    String textAfter = "";
    continueSearching = true;
    while (continueSearching && endIndex < contents.length()) {
      continueSearching = false;
      if (contents.charAt(endIndex) == ')') {
        parenthesisAfter++;
        endIndex++;
        continueSearching = true;
      } else if (contents.charAt(endIndex) == ']') {
        squareBracketAfter++;
        endIndex++;
        continueSearching = true;
      } else if (contents.charAt(endIndex) == '.') {
        textAfter += '.';
        endIndex++;
        continueSearching = true;
      } else if (contents.charAt(endIndex) == '<') {
        PageElementTag smallTagAfter = analysis.isInTag(endIndex, HtmlTagType.SMALL);
        if (smallTagAfter != null &&
            smallTagAfter.getBeginIndex() == endIndex &&
            !smallTagAfter.isFullTag() &&
            smallTagAfter.isEndTag()) {
          smallAfter++;
          endIndex = smallTagAfter.getEndIndex();
          continueSearching = true;
        }
      }
    }
    
    return new ISBNExtension(
        beginIndex, endIndex,
        parenthesisBefore, parenthesisAfter,
        squareBracketBefore, squareBracketAfter,
        smallBefore, smallAfter,
        textAfter);
  }
}
