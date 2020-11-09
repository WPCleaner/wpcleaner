/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2018  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.analysis;

import org.wikipediacleaner.api.data.contents.ContentsInterval;
import org.wikipediacleaner.api.data.contents.comment.ContainerComment;
import org.wikipediacleaner.api.data.contents.comment.ContentsComment;

/**
 * Management of page contents (text, analysis into elements)
 */
public class Contents {

  /**
   * @param text Text of page.
   * @return Contents management for the page.
   */
  public static Contents createContents(String text) {
    return new Contents(text);
  }

  /** Text of the page */
  private final String text;

  /** Contents analyzer */
  private final AnalyzerContents analyzer;

  /**
   * @param text Text of the page.
   */
  private Contents(String text) {
    this.text = (text != null) ? text : "";
    this.analyzer = new AnalyzerContents(this);
  }

  // ==============================================================================================
  // General functions
  // ==============================================================================================

  /**
   * @return Text of the page.
   */
  public String getText() {
    return text;
  }

  /**
   * @return Length of the text of the page.
   */
  public int length() {
    return text.length();
  }

  /**
   * @param interval Interval of the sub-string.
   * @return Sub-string for the requested interval.
   */
  public String substring(ContentsInterval interval) {
    return text.substring(interval.getBeginIndex(), interval.getEndIndex());
  }

  /**
   * @param beginIndex Begin index of the sub-string.
   * @param endIndex End index of the sub-string.
   * @return Sub-string for the requested indexes.
   */
  public String substring(int beginIndex, int endIndex) {
    return text.substring(beginIndex, endIndex);
  }

  // ==============================================================================================
  // Management of elements
  // ==============================================================================================

  /** Container for the comments */
  ContainerComment comments = null;

  /**
   * @return Comments container.
   */
  public ContainerComment comments() {
    if (comments == null) {
      analyzer.analyze(ContentsComment.class);
    }
    return comments;
  }
}
